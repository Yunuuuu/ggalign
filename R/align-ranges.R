align_ranges <- function(data = waiver(), mapping = aes(),
                         ranges = waiver(), position = waiver(),
                         size = NULL, active = NULL) {
    assert_layout_position(position)
    if (inherits(data, "uneval")) {
        cli_abort(c(
            "{.arg data} cannot be {.obj_type_friendly {data}}",
            "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
        ))
    }
    assert_active(active)
    active <- update_active(active, new_active(
        use = TRUE, order = NA_integer_, name = NA_character_
    ))
    align(AlignRanges,
        plot = ggplot(mapping = mapping),
        size = size, data = data,
        params = list(ranges = ranges, position = position),
        schemes = new_schemes(),
        active = active
    )
}

#' @importFrom ggplot2 ggproto ggplot margin element_rect
AlignRanges <- ggproto("AlignRanges", AlignGG,
    extra_params = c("ranges", "position"),
    setup_params = function(self, nobs, params) {
        if (!is.waive(.subset2(params, "ranges"))) {
            params$ranges <- lapply(
                .subset2(params, "ranges"),
                function(range) {
                    ans <- vec_as_location(
                        unclass(range),
                        n = nobs,
                        names = self$labels,
                        missing = "error",
                        arg = "ranges",
                        call = self$call
                    )
                    if (inherits(range, "AsIs")) ans <- I(ans)
                    ans
                }
            )
        }
        params$margin <- .subset2(params, "margin") %||% margin()
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
        # parse link
        support_link <- switch_direction(
            direction, c("left", "right"), c("top", "bottom")
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
                cli_warn(sprintf("Cannot add link ranges in {.field %s}", warn))
            }
            link_position <- intersect(link_position, support_link)
            if (length(link_position) == 0L) link_position <- NULL
        }

        # parse ranges
        panel <- .subset2(coords, "panel")
        index <- .subset2(coords, "index")
        subset <- seq_along(index) # used to match the original data
        full_breaks <- split(subset, panel)
        if (is.waive(ranges <- .subset2(params, "ranges"))) {
            breaks <- full_breaks
        } else {
            breaks <- lapply(ranges, function(range) {
                if (!inherits(range, "AsIs")) { # match the original data index
                    range <- match(range, index)
                }
                subset[range]
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
                ggplot2::facet_grid(
                    rows = ggplot2::vars(fct_rev(.data$.panel)),
                    scales = "free", space = "free",
                    drop = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free", space = "free",
                    drop = FALSE
                )
            )
        } else {
            default_facet <- ggplot2::facet_null()
        }
        plot <- plot + align_melt_facet(plot$facet, default_facet, direction)
        if (!is.null(link_position)) {
            plot$align_ranges_data <- list(
                full_breaks = full_breaks,
                breaks = breaks, direction = direction,
                link_position = link_position
            )
            plot <- add_class(plot, "align_ranges_plot", "patch_ggplot")
        }
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_schemes(plot, schemes)
        if (inherits(plot, "align_ranges_plot")) {
            theme <- complete_theme(theme)
            element <- calc_element("plot.ggalign_ranges", theme)
            if (inherits(element, "element_blank")) {
                class(plot) <- setdiff(class(plot), "align_ranges_plot")
                plot$align_ranges_data <- NULL
            } else {
                # save spacing for usage
                plot$align_ranges_data$spacing <- calc_element(
                    switch_direction(
                        self$direction,
                        "panel.spacing.y",
                        "panel.spacing.x"
                    ),
                    theme
                ) %||% unit(0, "mm")
                plot$align_ranges_data$element <- element
            }
        }
        plot
    }
)

#' @importFrom ggplot2 ggproto
#' @export
alignpatch.align_ranges_plot <- function(x) {
    ggproto(NULL, PatchAlignRangesPlot, plot = x)
}

#' @export
`[.alignRangesGtable` <- function(x, i, j) {
    # subset will violate the `alignRangesGtable` `shape`
    # we always use the next method
    class(x) <- setdiff(class(x), "alignRangesGtable")
    x$align_ranges_data <- NULL
    NextMethod()
}

#' @include alignpatch-ggplot2.R
PatchAlignRangesPlot <- ggproto(
    "PatchAlignRangesPlot", PatchGgplot,
    patch_gtable = function(self, plot = self$plot) {
        ans <- ggproto_parent(PatchGgplot, self)$patch_gtable(plot = plot)
        # re-define the draw method, we assign new class
        ans <- add_class(ans, "alignRangesGtable")
        ans$align_ranges_data <- .subset2(plot, "align_ranges_data")
        ans
    },
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
makeContent.alignRangesGtable <- function(x) {
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
    range_data <- .subset2(x, "align_ranges_data")
    breaks <- .subset2(range_data, "breaks")
    link_position <- .subset2(range_data, "link_position")
    full_breaks <- .subset2(range_data, "full_breaks")
    direction <- .subset2(range_data, "direction")
    spacing <- convertHeight(
        .subset2(range_data, "spacing"), "mm",
        valueOnly = TRUE
    )

    # each break represent an `observation`, for panel space, we use `NA`
    # obs arranged from left to top, and from bottom to top
    obs <- unlist(vec_interleave(full_breaks, list(NA)), FALSE, FALSE)
    obs <- obs[-length(obs)] # remove the last panel space, shouldn't exist
    sizes <- numeric(length(obs))
    sizes[is.na(obs)] <- spacing
    n_spacing <- length(full_breaks) - 1L
    sizes[!is.na(obs)] <- (height - spacing * n_spacing) /
        sum(lengths(full_breaks)) # nobs
    cum_sizes <- cumsum(sizes)

    # then, we define the link grobs
    coord_x <- coord_y <- numeric()
    if (is_horizontal(direction)) { # the link should be in left or right
        # from bottom to the top, following the ordering of the `breaks`
        panel_index <- seq(
            from = .subset2(panel_loc, "b"),
            to = .subset2(panel_loc, "t"),
            by = -2L
        )
        panel_index <- nrow(x) - panel_index + 1L
        l_border <- plot_widths[seq_len(.subset2(panel_loc, "l") - 1L)]
        r_border <- plot_widths[-seq_len(.subset2(panel_loc, "r"))]
        # for a gtable, heights are from top to the bottom,
        # we reverse the heights
        plot_cum_heights <- cumsum(rev(plot_heights))
        for (position in link_position) {
            for (i in seq_along(panel_index)) {
                # we match the observations
                pos <- match(.subset2(breaks, i), obs)
                coord_y <- c(
                    coord_y,
                    # for height next to the plot panel
                    plot_cum_heights[panel_index[i] + (-1:0)],
                    # for height in the border
                    c(
                        cum_sizes[max(pos)],
                        cum_sizes[min(pos)] - sizes[min(pos)]
                    )
                )
                if (position == "left") {
                    coord_x <- c(
                        coord_x,
                        vec_rep_each(c(sum(l_border), 0), 2L)
                    )
                } else {
                    coord_x <- c(
                        coord_x,
                        vec_rep_each(c(width - sum(r_border), width), 2L)
                    )
                }
            }
        }
    } else {

    }
    layout <- .subset2(x, "layout")
    panels <- layout[grepl("^panel", .subset2(layout, "name")), , drop = FALSE]
    x <- gtable_add_grob(
        x,
        grobs = ggplot2::element_grob(
            .subset2(range_data, "element"),
            x = coord_x, y = coord_y,
            id.lengths = vec_rep(4L, length(coord_x) / 4L),
            default.units = "mm"
        ),
        t = 1L, l = 1L, b = -1L, r = -1L,
        # always draw with panel area
        z = min(panels$z)
    )
    NextMethod()
}
