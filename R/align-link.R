#' Add a plot to annotate selected observations
#'
#' - `align_line`: Annotate a list of spread observations. Observations will be
#'   connected to the panel by a line.
#' - `align_range`: Annotate a list of ranges of observations. Observation
#'   ranges will be connected to the panel by a polygon.
#'
#' @inheritParams align_gg
#' @param ranges A list of observation ranges. Each range will be represented
#' by a facet panel.
#' @param links A list of observations. Each group of observations will be
#' represented by a facet panel.
#' @param position Which side the link should be added to? A string containing
#' one or more of `r oxford_and(.tlbr)`. For a horizontal [`stack_layout()`],
#' only `l` (left) and `r` (right) can be used. For a vertical
#' [`stack_layout()`], only `b` (bottom) and `t` (top) are available.
#'
#' - Link ranges can be customized using the `plot.ggalign_ranges` theme element
#'   with [`element_polygon()`].
#' - Link lines can be customized using the `plot.ggalign_lines` theme element
#'   with [`element_line()`].
#'
#' @section ggplot2 specification:
#' `align_ranges` initializes a ggplot.
#'
#' The data in the underlying `ggplot` object will contain following columns:
#'
#'  - `.panel`: the panel for the aligned axis. It means `x-axis` for vertical
#'    stack layout (including top and bottom annotation), `y-axis` for
#'    horizontal stack layout (including left and right annotation).
#'
#'  - `.names` ([`vec_names()`][vctrs::vec_names]) and `.index`
#'    ([`vec_size()`][vctrs::vec_size()]/[`NROW()`]): a character names (only
#'    applicable when names exists) and an integer of index of the original
#'    data.
#'
#'  - `.row_names` and `.row_index`: the row names and an integer of
#'    row index of the original matrix (only applicable if `data` is a
#'    `matrix`).
#'
#'  - `.column_names` and `.column_index`: the column names and column index of
#'    the original matrix (only applicable if `data` is a `matrix`).
#'
#'  - `value`: the actual value (only applicable if `data` is a `matrix` or
#'    atomic vector).
#'
#' `matrix` input will be automatically melted into a long foramted data frame.
#'
#' Atomic vector will be put in the `value` column of the data frame.
#'
#' In the case where the input data is already a data frame, following columns
#' (`.panel`, `.index`, `.names`) are added to the data frame.
#'
#' @export
align_line <- function(data = waiver(), mapping = aes(),
                       links = waiver(), position = waiver(),
                       size = NULL, active = NULL) {
    assert_layout_position(position)
    if (inherits(data, "uneval")) {
        cli_abort(c(
            "{.arg data} cannot be {.obj_type_friendly {data}}",
            "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
        ))
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    align(
        new_align_link(
            "AlignLine",
            arg = "links",
            class = "align_line_plot",
            element = "plot.ggalign_lines"
        ),
        plot = ggplot(mapping = mapping),
        size = size, data = data,
        params = list(links = links, position = position),
        schemes = new_schemes(),
        active = active
    )
}

#' @importFrom ggplot2 ggproto
#' @export
alignpatch.align_line_plot <- function(x) {
    ggproto(NULL, PatchAlignLinePlot, plot = x)
}

#' @export
`[.alignLineGtable` <- function(x, i, j) {
    # subset will violate the `alignLineGtable` `shape`
    # we always use the next method
    class(x) <- setdiff(class(x), "alignLineGtable")
    x$links_data <- NULL
    NextMethod()
}

# preDraw:
#  - makeContext
#  - pushvpgp
#  - preDrawDetails: by default, do noting
# makeContent:
# drawDetails:
# postDraw:
#  - postDrawDetails: by default, do noting
#  - popgrobvp
#' @importFrom grid makeContent unit convertHeight convertWidth viewport
#' @export
makeContent.alignLineGtable <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)

    # Grab grob metadata
    plot_widths <- compute_null_width(.subset2(x, "widths"),
        valueOnly = TRUE
    )
    # from top to the bottom
    plot_heights <- compute_null_height(.subset2(x, "heights"),
        valueOnly = TRUE
    )
    panel_loc <- find_panel(x)
    links_data <- .subset2(x, "links_data")
    breaks <- .subset2(links_data, "breaks")
    link_position <- .subset2(links_data, "link_position")
    full_breaks <- .subset2(links_data, "full_breaks")
    direction <- .subset2(links_data, "direction")
    spacing <- convertHeight(
        .subset2(links_data, "spacing"), "mm",
        valueOnly = TRUE
    )

    # each break represent an `observation`, for panel space, we use `NA`
    # obs arranged from left to top, and from bottom to top
    obs <- unlist(vec_interleave(full_breaks, list(NA)), FALSE, FALSE)
    obs <- obs[-length(obs)] # remove the last panel space, shouldn't exist
    sizes <- numeric(length(obs))
    sizes[is.na(obs)] <- spacing
    n_spacing <- length(full_breaks) - 1L

    # then, we define the link grobs
    coord_x <- coord_y <- numeric()
    if (is_horizontal(direction)) { # the link should be in left or right
        sizes[!is.na(obs)] <- (height - spacing * n_spacing) /
            sum(lengths(full_breaks)) # nobs
        points_y <- cumsum(sizes) - (sizes / 2L)
        # from bottom to the top, following the ordering of the `breaks`
        panel_index <- seq(
            from = .subset2(panel_loc, "b"),
            to = .subset2(panel_loc, "t"),
            length.out = length(breaks)
        )
        l_border <- plot_widths[seq_len(.subset2(panel_loc, "l") - 1L)]
        r_border <- plot_widths[-seq_len(.subset2(panel_loc, "r"))]
        # we'll reverse the `plot_cum_heights`, so the ordering index should
        # also be reversed
        panel_index <- nrow(x) - panel_index + 1L
        # for a gtable, heights are from top to the bottom,
        # we reverse the heights
        plot_cum_heights <- cumsum(rev(plot_heights))
        for (position in link_position) {
            for (i in seq_along(panel_index)) {
                # we match the observations
                pos <- match(.subset2(breaks, i), obs)
                # we arrange pos from lower to higher
                coord_y <- c(coord_y, vec_interleave(
                    # seq(
                    #     from = plot_cum_heights[panel_index[i] - 1L],
                    #     to = plot_cum_heights[panel_index[i]],
                    #     length.out = length(pos)
                    # )[seq_along(pos)[order(pos)]],
                    vec_rep( # we always link the midpoint
                        mean(plot_cum_heights[panel_index[i] + (-1:0)]),
                        length(pos)
                    ),
                    points_y[pos]
                ))
                if (position == "left") {
                    coord_x <- c(
                        coord_x,
                        vec_rep(c(sum(l_border), 0), length(pos))
                    )
                } else {
                    coord_x <- c(
                        coord_x,
                        vec_rep(c(width - sum(r_border), width), length(pos))
                    )
                }
            }
        }
    } else {
        sizes[!is.na(obs)] <- (width - spacing * n_spacing) /
            sum(lengths(full_breaks)) # nobs
        points_x <- cumsum(sizes) - (sizes / 2)
        panel_index <- seq(
            from = .subset2(panel_loc, "l"),
            to = .subset2(panel_loc, "r"),
            length.out = length(breaks)
        )
        t_border <- plot_heights[seq_len(.subset2(panel_loc, "t") - 1L)]
        b_border <- plot_heights[-seq_len(.subset2(panel_loc, "b"))]
        plot_cum_widths <- cumsum(plot_widths)
        for (position in link_position) {
            for (i in seq_along(panel_index)) {
                # we match the observations
                pos <- match(.subset2(breaks, i), obs)
                coord_x <- c(coord_x, vec_interleave(
                    # seq(
                    #     from = plot_cum_widths[panel_index[i] - 1L],
                    #     to = plot_cum_widths[panel_index[i]],
                    #     length.out = length(pos)
                    # )[seq_along(pos)[order(pos)]],
                    vec_rep( # we always link the midpoint
                        mean(plot_cum_widths[panel_index[i] + (-1:0)]),
                        length(pos)
                    ),
                    points_x[pos]
                ))
                if (position == "bottom") {
                    coord_y <- c(
                        coord_y,
                        vec_rep(c(sum(b_border), 0), length(pos))
                    )
                } else {
                    coord_y <- c(
                        coord_y,
                        vec_rep(c(height - sum(t_border), height), length(pos))
                    )
                }
            }
        }
    }
    layout <- .subset2(x, "layout")
    panels <- layout[grepl("^panel", .subset2(layout, "name")), , drop = FALSE]
    x <- gtable_add_grob(
        x,
        grobs = ggplot2::element_grob(
            .subset2(links_data, "element"),
            x = coord_x, y = coord_y,
            id.lengths = vec_rep(2L, length(coord_x) / 2L),
            default.units = "mm"
        ),
        t = 1L, l = 1L, b = -1L, r = -1L,
        # always draw with panel area
        z = min(panels$z)
    )
    NextMethod()
}

####################################################################
#' @importFrom ggplot2 ggproto
new_align_link <- function(`_class` = NULL, arg, class, element, ...) {
    ggproto(`_class`, AlignLinkProto,
        extra_params = c(arg, "position"),
        arg = arg, class = class, element = element, ...
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include alignpatch-ggplot2.R
PatchAlignLinkProto <- ggproto(
    "PatchAlignLinkProto", PatchGgplot,
    add_plot = function(self, gt, plot, t, l, b, r, name, z = 2L) {
        gtable_add_grob(
            gt,
            grobs = plot,
            # t = 8, l = 6, b = 14, r = 12
            # t = t + 7L, l = l + 5L, b = b - 6L, r = r - 5L,
            t = t + TOP_BORDER, l = l + LEFT_BORDER,
            name = name, z = z
        )
    },
    add_background = function(self, gt, bg, t, l, b, r, name, z = 1L) {
        gtable_add_grob(
            gt,
            grobs = bg,
            t = t + TOP_BORDER, l = l + LEFT_BORDER,
            name = name, z = z
        )
    },
    get_sizes = function(self, free = NULL, gt = self$gt) {
        PatchGgplot$get_sizes(.tlbr, gt = gt)
    },
    align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                            gt = self$gt) {
        gt # free from alignment
    }
)

PatchAlignLinePlot <- ggproto(
    "PatchAlignLinePlot", PatchAlignLinkProto,
    patch_gtable = function(self, plot = self$plot) {
        ans <- ggproto_parent(PatchAlignLinkProto, self)$patch_gtable(
            plot = plot
        )
        # re-define the draw method, we assign new class
        ans <- add_class(ans, "alignLineGtable")
        ans$links_data <- .subset2(plot, "links_data")
        ans
    }
)

#' @importFrom ggplot2 ggproto ggplot margin element_rect
AlignLinkProto <- ggproto("AlignLinkProto", AlignGg,
    class = NULL, element = NULL,
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_schemes(plot, schemes)
        if (inherits(plot, self$class)) {
            element <- calc_element(self$element, complete_theme(plot$theme))
            if (inherits(element, "element_blank")) {
                class(plot) <- setdiff(class(plot), self$class)
                plot$links_data <- NULL
            } else {
                # save spacing for usage
                plot$links_data$spacing <- calc_element(
                    switch_direction(
                        self$direction,
                        "panel.spacing.y",
                        "panel.spacing.x"
                    ),
                    complete_theme(theme)
                ) %||% unit(0, "mm")
                plot$links_data$element <- element
            }
        }
        plot
    },
    setup_params = function(self, nobs, params) {
        if (!is.waive(x <- .subset2(params, self$arg))) {
            if (!is.list(x)) x <- list(x)
            params[[self$arg]] <- lapply(x, function(link) {
                ans <- vec_as_location(
                    unclass(link),
                    n = nobs,
                    names = self$labels,
                    missing = "error",
                    arg = self$arg,
                    call = self$call
                )
                if (inherits(link, "AsIs")) ans <- I(ans)
                ans
            })
        }
        params
    },
    setup_plot = function(self, plot, layout_data, layout_coords, layout_name) {
        ggadd_default(plot, theme = theme(
            panel.border = element_rect(fill = NA, colour = "grey20"),
            panel.background = element_rect(fill = "white", colour = NA)
        ))
    },

    #' @importFrom stats reorder
    build = function(self, plot, coords, extra_coords, previous_coords = NULL) {
        params <- .subset2(self, "params")
        direction <- self$direction
        position <- self$position
        axis <- to_coord_axis(direction)
        # parse link position
        support_link <- switch_direction(
            direction, c("left", "right"), c("bottom", "top")
        )
        if (is.waive(link_position <- .subset2(params, "position"))) {
            if (is.null(position)) {
                link_position <- support_link
            } else {
                link_position <- opposite_pos(position)
            }
        } else if (!is.null(link_position)) {
            link_position <- complete_pos(split_position(link_position))
            warn <- setdiff(link_position, support_link)
            if (length(warn)) {
                cli_warn(sprintf("Cannot add links in {.field %s}", warn))
            }
            link_position <- intersect(link_position, support_link)
            if (length(link_position) == 0L) link_position <- NULL
        }

        # parse ranges
        panel <- .subset2(coords, "panel")
        index <- .subset2(coords, "index")
        subset <- seq_along(index) # used to match the original data
        full_breaks <- split(subset, panel)
        if (is.waive(data <- .subset2(params, self$arg))) {
            breaks <- full_breaks
        } else {
            breaks <- lapply(data, function(link) {
                if (!inherits(link, "AsIs")) { # match the original data index
                    link <- match(link, index)
                }
                subset[link]
            })
        }
        subset <- unlist(breaks, FALSE, FALSE)

        # prepare data for the plot ------------------------------
        plot_panel <- names(breaks) %||% seq_along(breaks)
        plot_data <- data_frame0(.panel = factor(
            vec_rep_each(plot_panel, list_sizes(breaks)), unique(plot_panel)
        ))
        plot_data$.index <- index[subset]
        if (!is.null(self$labels)) {
            plot_data[[".names"]] <- .subset(
                self$labels,
                .subset2(plot_data, ".index")
            )
        }
        if (!is.null(data <- .subset2(self, "data"))) {
            plot_data <- inner_join(plot_data, data, by = ".index")
        }
        plot$data <- ggalign_attr_restore(plot_data, data)

        # set up facets
        if (length(breaks) > 1L) {
            default_facet <- switch_direction(
                direction,
                ggplot2::facet_wrap(
                    facets = ggplot2::vars(.data$.panel),
                    ncol = 1L, as.table = FALSE
                ),
                ggplot2::facet_wrap(
                    facets = ggplot2::vars(.data$.panel),
                    nrow = 1L, as.table = FALSE
                )
            )
        } else {
            default_facet <- ggplot2::facet_null()
        }
        plot <- plot + align_melt_facet(default_facet, plot$facet)
        if (!is.null(link_position)) {
            plot$links_data <- list(
                full_breaks = full_breaks,
                breaks = breaks, direction = direction,
                link_position = link_position
            )
            plot <- add_class(plot, self$class, "patch_ggplot")
        }
        plot
    }
)
