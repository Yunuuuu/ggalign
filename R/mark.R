#' Define the links to connect the marked observations
#'
#' @description
#' This function allows users to define links between marked observations and
#' plot panel (e.g., for creating visual connections for related data), which
#' could help explain the observations.
#'
#' @param .draw A function used to draw the links. The function must return a
#'   [`grob()`][grid::grob] object. If the function does not return a valid
#'   `grob`, nothing will be drawn. The input data for the function must contain
#'   two arguments: a data frame for the panel side coordinates and a data frame
#'   for the marked observation coordinates.
#' @inheritParams pair_links
#' @seealso
#'  - [`mark_line()`]
#'  - [`mark_tetragon()`]
#'  - [`.mark_draw()`]
#' @importFrom rlang is_empty inject
#' @export
mark_draw <- function(.draw, ...) {
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function")
    }
    new_draw <- function(data) {
        ans <- lapply(data, function(dd) {
            draw(.subset2(dd, "panel"), .subset2(dd, "link"))
        })
        ans <- ans[vapply(ans, is.grob, logical(1L), USE.NAMES = FALSE)]
        if (!is_empty(ans)) {
            grid::gTree(children = inject(grid::gList(!!!ans)))
        }
    }
    .mark_draw(new_draw, ...)
}

#' @inherit mark_draw title
#'
#' @description
#' A base version of [`mark_draw`], designed for performance optimization. This
#' function is used to build other `mark_*` functions that manage the drawing of
#' links between marked observations.
#'
#' @param .draw A function used to draw the links. The function must return a
#'   [`grob()`][grid::grob] object. If the function does not return a valid
#'   `grob`, nothing will be drawn. The input data for the function contains a
#'   list, where each item is a list of two data frames: one for the panel side
#'   coordinates (`"panel"`) and one for the marked observations coordinates
#'   (`"link"`).
#'
#' @inheritParams mark_draw
#' @seealso [`mark_draw()`]
#' @export
.mark_draw <- function(.draw, ...) {
    if (override_call(call <- caller_call())) {
        call <- current_call()
    }
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function", call = call)
    }
    links <- pair_links(...)
    structure(list(draw = draw, links = links), class = "ggalign_mark_draw")
}

#' @export
print.ggalign_mark_draw <- function(x, ...) {
    header <- sprintf("<%s>", vec_ptype_full(x))
    cat(header, sep = "\n")
    obj_print_data(.subset2(x, "links"))
    invisible(x)
}

#' Link the observations and the panel with a line
#'
#' @param ... Additional arguments passed on to [`mark_draw()`].
#' @param element A [`element_line()`][ggplot2::element_line] object. Vectorized
#'   fields will be recycled to match the total number of groups, or you can
#'   wrap the element with [`I()`] to recycle to match the drawing groups. The
#'   drawing groups typically correspond to the number of observations, as each
#'   observation will be linked with the plot panel.
#' @importFrom ggplot2 element_line
#' @export
mark_line <- function(..., element = NULL) {
    assert_s3_class(element, "element_line", allow_null = TRUE)
    default <- calc_element("ggalign.line", complete_theme(theme_get()))
    if (is.null(element)) {
        element <- default
    } else {
        element <- ggplot2::merge_element(element, default)
    }
    .mark_draw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")
            data_frame0(
                # there is only one row for panel, it's safe to use
                # vec_interleave directly
                x = vec_interleave(
                    (panel$x + panel$xend) / 2L,
                    (link$x + link$xend) / 2L
                ),
                y = vec_interleave(
                    (panel$y + panel$yend) / 2L,
                    (link$y + link$yend) / 2L
                )
            )
        })
        if (inherits(element, "AsIs")) {
            element <- element_rep_len(element,
                length.out = sum(list_sizes(data)) / 2L
            )
        } else {
            element <- element_rep_len(element, length.out = length(data))
            element <- element_vec_rep_each(element,
                times = list_sizes(data) / 2L
            )
        }
        data <- vec_rbind(!!!data)
        element_grob(
            element,
            x = data$x, y = data$y,
            id.lengths = vec_rep(2L, vec_size(data) / 2L),
            default.units = "native"
        )
    }, ...)
}

#' Link the observations and the panel with a quadrilateral
#'
#' @param ... Additional arguments passed on to [`mark_draw()`].
#' @param element A [`element_polygon()`] object. Vectorized fields will be
#'   recycled to match the total number of groups, or you can wrap the element
#'   with [`I()`] to recycle to match the drawing groups. The drawing groups
#'   are usually the same as the defined groups, but they will differ when the
#'   defined group of observations is separated and cannot be linked with a
#'   single quadrilateral. In such cases, the number of drawing groups will be
#'   larger than the number of defined groups.
#' @export
mark_tetragon <- function(..., element = NULL) {
    assert_s3_class(element, "element_polygon", allow_null = TRUE)
    default <- calc_element("ggalign.polygon", complete_theme(theme_get()))
    if (is.null(element)) {
        element <- default
    } else {
        element <- ggplot2::merge_element(element, default)
    }
    .mark_draw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")

            # find the consecutive groups
            index <- .subset2(link, "link_index")
            oindex <- order(index)
            group <- cumsum(c(0L, diff(index[oindex])) != 1L)

            # restore the order
            group <- group[order(oindex)]

            # split link into groups
            link <- vec_split(link, group)

            # for each group, we draw a quadrilateral
            vec_rbind(!!!lapply(.subset2(link, "val"), function(dd) {
                data_frame0(
                    x = vec_c(panel$x, panel$xend, max(dd$xend), min(dd$x)),
                    y = vec_c(panel$y, panel$yend, max(dd$yend), min(dd$y))
                )
            }))
        })
        if (inherits(element, "AsIs")) {
            element <- element_rep_len(element,
                length.out = sum(list_sizes(data)) / 4L
            )
        } else {
            element <- element_rep_len(element, length.out = length(data))
            element <- element_vec_rep_each(element,
                times = list_sizes(data) / 4L
            )
        }
        data <- vec_rbind(!!!data)
        element_grob(
            element,
            x = data$x, y = data$y,
            id.lengths = vec_rep(4L, nrow(data) / 4L),
            default.units = "native"
        )
    }, ...)
}

# Not implemented completely
#' @importFrom rlang arg_match0
mark_triangle <- function(..., orientation = "plot", element = NULL) {
    assert_s3_class(element, "element_polygon", allow_null = TRUE)
    default <- calc_element("ggalign.polygon", complete_theme(theme_get()))
    if (is.null(element)) {
        element <- default
    } else {
        element <- ggplot2::merge_element(element, default)
    }
    orientation <- arg_match0(orientation, c("plot", "mark"))
    .mark_draw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")

            # find the consecutive groups
            index <- .subset2(link, "link_index")
            oindex <- order(index)
            group <- cumsum(c(0L, diff(index[oindex])) != 1L)

            # restore the order
            group <- group[order(oindex)]

            # split link into groups
            link <- vec_split(link, group)

            # for each group, we draw a quadrilateral
            tetragon_list <- lapply(.subset2(link, "val"), function(dd) {
                data_frame0(
                    x = vec_c(panel$x, panel$xend, max(dd$xend), min(dd$x)),
                    y = vec_c(panel$y, panel$yend, max(dd$yend), min(dd$y))
                )
            })
            vec_rbind(!!!tetragon_list)
        })
        data <- vec_rbind(!!!data)
        element_grob(
            element,
            x = data$x, y = data$y,
            id.lengths = vec_rep(3L, nrow(data) / 3L),
            default.units = "native"
        )
    }, ...)
}

#####################################################
#' @export
`[.ggalignMarkGtable` <- function(x, i, j) {
    # subset will violate the `ggalignMarkGtable` `shape`
    # we always use the next method
    x <- remove_class(x, "ggalignMarkGtable")
    x$ggalign_link_data <- NULL
    NextMethod()
}

#' @importFrom ggplot2 ggproto
#' @export
alignpatch.ggalign_mark_plot <- function(x) {
    ggproto(NULL, PatchAlignMark, plot = x)
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include alignpatch-ggplot2.R
PatchAlignMark <- ggproto(
    "PatchAlignMark", PatchGgplot,
    patch_gtable = function(self, plot = self$plot) {
        ans <- ggproto_parent(PatchGgplot, self)$patch_gtable(plot = plot)
        # re-define the draw method, we assign new class
        ans <- add_class(ans, "ggalignMarkGtable")
        ans$ggalign_link_data <- .subset2(plot, "ggalign_link_data")
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
#' @importFrom stats reorder
#' @export
makeContent.ggalignMarkGtable <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)

    # Grab grob metadata
    plot_widths <- compute_null_width(.subset2(x, "widths"),
        valueOnly = TRUE
    )
    plot_widths <- scales::rescale(plot_widths, c(0, 1), from = c(0, width))
    plot_heights <- compute_null_height(.subset2(x, "heights"),
        valueOnly = TRUE
    )
    plot_heights <- scales::rescale(plot_heights, c(0, 1), from = c(0, height))

    panel_loc <- find_panel(x)
    data <- .subset2(x, "ggalign_link_data")
    full_data1 <- .subset2(data, "full_data1")
    full_data2 <- .subset2(data, "full_data2")
    direction <- .subset2(data, "direction")
    link_index_list <- .subset2(data, "link_index")
    data_index_list <- .subset2(data, "data_index")

    # prepare output for current for loop
    coords <- vector("list", 2L)
    names(coords) <- c("hand1", "hand2")
    for (link in names(coords)) {
        link_index <- lapply(link_index_list, .subset2, link)
        data_index <- lapply(data_index_list, .subset2, link)
        # early exit and step into next cycle if no link
        if (all(vapply(link_index, is.null, logical(1L), USE.NAMES = FALSE))) {
            coords[link] <- list(link_index)
            next
        }
        full_breaks <- switch(link,
            hand1 = full_data1,
            hand2 = full_data2
        )

        spacing <- .subset2(data, switch(link,
            hand1 = "spacing1",
            hand2 = "spacing2"
        ))

        # each value represent an `observation`, for panel space, we use `NA`
        # obs arranged from left to top, and from bottom to top
        points <- unlist(vec_interleave(full_breaks, list(NA)), FALSE, FALSE)
        # remove the last panel space, shouldn't exist
        points <- points[-length(points)]
        sizes <- numeric(length(points))
        n_spacing <- length(full_breaks) - 1L

        # then, we define the link grobs
        if (is_horizontal(direction)) { # the link should be in left or right
            spacing <- convertHeight(spacing, "mm", valueOnly = TRUE)
            spacing <- scales::rescale(spacing, c(0, 1), from = c(0, height))
            sizes[is.na(points)] <- spacing
            cell_height <- (1 - spacing * n_spacing) / sum(!is.na(points))
            sizes[!is.na(points)] <- cell_height # nobs
            yend <- cumsum(sizes)
            link_x <- switch(link,
                hand1 = 0,
                hand2 = 1
            )
            link_coord <- data_frame0(
                x = link_x, xend = link_x,
                y = yend - sizes, yend = yend
            )
            link_coord <- vec_slice(link_coord, !is.na(points))

            # from bottom to the top, following the ordering of the `breaks`
            panel_index <- seq(
                from = .subset2(panel_loc, "b"),
                to = .subset2(panel_loc, "t"),
                length.out = length(link_index)
            )
            l_border <- plot_widths[seq_len(.subset2(panel_loc, "l") - 1L)]
            r_border <- plot_widths[-seq_len(.subset2(panel_loc, "r"))]

            # for a gtable, heights are from top to the bottom,
            # we reverse the heights
            # we have reversed the `plot_cum_heights`, so the ordering index
            # should also be reversed
            panel_index <- nrow(x) - panel_index + 1L
            panel_yend <- cumsum(rev(plot_heights))
            panel_x <- switch(link,
                hand1 = sum(l_border),
                hand2 = 1 - sum(r_border)
            )
            panel_coord <- data_frame0(
                x = panel_x,
                xend = panel_x,
                y = (panel_yend - rev(plot_heights))[panel_index],
                yend = panel_yend[panel_index]
            )
        } else { # the link should be in top or bottom
            spacing <- convertWidth(spacing, "mm", valueOnly = TRUE)
            spacing <- scales::rescale(spacing, c(0, 1), from = c(0, width))
            sizes[is.na(points)] <- spacing
            cell_width <- (1 - spacing * n_spacing) / sum(!is.na(points))
            sizes[!is.na(points)] <- cell_width
            xend <- cumsum(sizes)
            link_y <- switch(link,
                hand1 = 1,
                hand2 = 0
            )
            link_coord <- data_frame0(
                x = xend - sizes, xend = xend,
                y = link_y, yend = link_y
            )
            link_coord <- vec_slice(link_coord, !is.na(points))

            panel_index <- seq(
                from = .subset2(panel_loc, "l"),
                to = .subset2(panel_loc, "r"),
                length.out = length(link_index)
            )
            t_border <- plot_heights[seq_len(.subset2(panel_loc, "t") - 1L)]
            b_border <- plot_heights[-seq_len(.subset2(panel_loc, "b"))]
            panel_xend <- cumsum(plot_widths)
            panel_y <- switch(link,
                hand1 = 1 - sum(t_border),
                hand2 = sum(b_border)
            )
            panel_coord <- data_frame0(
                x = (panel_xend - plot_widths)[panel_index],
                xend = panel_xend[panel_index],
                y = panel_y, yend = panel_y
            )
        }
        hand <- switch(link,
            hand1 = switch_direction(direction, "left", "top"),
            hand2 = switch_direction(direction, "right", "bottom")
        )
        nms <- names(link_index)
        link_panels <- vec_rep_each(names(full_breaks), list_sizes(full_breaks))
        coords[[link]] <- lapply(seq_along(link_index), function(i) {
            l_index <- .subset2(link_index, i)
            if (is.null(l_index)) return(NULL) # styler: off
            d_index <- .subset2(data_index, i)
            link <- vec_slice(link_coord, l_index)
            link$link_id <- nms[i]
            link$link_panel <- reorder(
                vec_slice(link_panels, l_index), l_index,
                order = FALSE
            )
            link$link_index <- l_index
            link$.hand <- hand
            link$.index <- d_index
            panel <- vec_slice(panel_coord, i)
            list(panel = panel, link = link)
        })
    }
    coords <- vec_interleave(
        .subset2(coords, "hand1"),
        .subset2(coords, "hand2")
    )
    coords <- list_drop_empty(coords)
    draw <- .subset2(data, "draw")
    if (!is.grob(grob <- draw(coords))) {
        return(NextMethod())
    }
    layout <- .subset2(x, "layout")
    panels <- layout[grepl("^panel", .subset2(layout, "name")), , drop = FALSE]
    x <- gtable_add_grob(
        x,
        grobs = grob,
        t = 1L, l = 1L, b = -1L, r = -1L,
        # always draw with panel area
        z = min(panels$z)
    )
    NextMethod()
}
