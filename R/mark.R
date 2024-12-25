#' Define the links to connect the marked observations
#'
#' @description
#' This function allows users to define links between marked observations (e.g.,
#' for creating visual connections for related data), which could help explain
#' the observations. Following function can be used to create the links.
#'
#' - `link`: Helper function to create a pair of links.
#' - `link_range`: Helper function to target a range of observations.
#'
#' @param .draw A function used to draw the links. The function must return a
#'   [`grob()`][grid::grob] object. If the function does not return a valid
#'   `grob`, nothing will be drawn. The input data for the function must contain
#'   two arguments: a data frame for the panel side coordinates and a data frame
#'   for the marked observation coordinates.
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of `link()` objects used to
#'   define the linked observations for a panel. Each element of the list will
#'   define a plot panel.
#' @param .link1,.link2 Same as `link1` and `link2`. But you can provide a
#'   list of them directly.
#' @param link1,link2 An integer or character index, or a `link_range()` object
#'   to define the linked observations. For integer indices, you can wrap them
#'   with [`I()`] to indicate the order based on the layout. You can also use
#'   `waiver()` to inherit the values from the opposite link argument.
#' @param point1,point2 A single integer or character index, defining the lower
#'   and higher bounds of the range. For integer indices, wrap them with [`I()`]
#'   to indicate the ordered index by the layout.
#' @seealso
#'  - [`mark_line()`]
#'  - [`mark_tetragon()`]
#'  - [`mark_pdraw()`]
#' @export
mark_draw <- function(.draw, ..., .link1 = NULL, .link2 = NULL) {
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function")
    }
    new_draw <- function(data) {
        ans <- lapply(data, function(dd) {
            draw(.subset2(dd, "panel"), .subset2(dd, "link"))
        })
        ans <- ans[!vapply(ans, is.grob, logical(1L), USE.NAMES = FALSE)]
        if (length(ans) == 0L) return(NULL) # styler: off
        grid::gTree(children = inject(grid::gList(!!!ans)))
    }
    mark_pdraw(new_draw, ..., .link1 = .link1, .link2 = .link2)
}

#' @inherit mark_draw title
#'
#' @description
#' A base version of [`mark_draw`], designed for performance optimization.  This
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
#' @export
mark_pdraw <- function(.draw, ..., .link1 = NULL, .link2 = NULL) {
    if (override_call(call <- caller_call())) {
        call <- current_call()
    }
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function", call = call)
    }
    links <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
    valid <- vapply(
        links, inherits, logical(1L), "ggalign_link",
        USE.NAMES = FALSE
    )
    if (!all(valid)) {
        cli_abort("{.arg ...} must be created with {.fn link}", call = call)
    }
    link1 <- check_link_list(.link1, call = call)
    link2 <- check_link_list(.link2, call = call)

    # Ensure that if both links have the same length
    if (length(link1) != length(link2)) {
        cli_abort(
            "{.arg .link1} and {.arg .link2} must have the same length.",
            call = call
        )
    }
    links <- c(links, mapply(function(l1, l2) {
        new_link(l1, l2)
    }, l1 = link1, l2 = link2, SIMPLIFY = FALSE))

    # remove links with both are `NULL`
    links <- links[!vapply(links, function(link) {
        all(vapply(link, is.null, logical(1L), USE.NAMES = FALSE))
    }, logical(1L), USE.NAMES = FALSE)]
    structure(list(draw = draw, links = links), class = "ggalign_mark_draw")
}

#' @export
#' @rdname mark_draw
link <- function(link1 = NULL, link2 = NULL) {
    if (!is_valid_link(link1)) {
        cli_abort(
            "{.arg link1} must be a numeric or character index or a {.fn link_range} object"
        )
    }
    if (!is_valid_link(link2)) {
        cli_abort(
            "{.arg link2} must be a numeric or character index or a {.fn link_range} object"
        )
    }
    if (is.null(link1) && is.null(link2)) {
        cli_abort(
            "At least one of {.arg link1} or {.arg link2} must not be `NULL`."
        )
    }
    new_link(link1, link2)
}

new_link <- function(link1, link2 = NULL) {
    structure(list(link1 = link1, link2 = link2), class = "ggalign_link")
}

check_link_list <- function(link, arg = caller_arg(link),
                            call = caller_call()) {
    if (is_valid_link(link)) {
        list(link)
    } else if (is.list(link)) {
        valid <- vapply(link, is_valid_link, logical(1L), USE.NAMES = FALSE)
        if (!all(valid)) {
            cli_abort(
                "{.arg {arg}} must be a numeric or character index, a {.fn link_range} object, or a list of these.",
                call = call
            )
        }
        link
    } else {
        cli_abort(
            "{.arg {arg}} must be a numeric or character index, a {.fn link_range} object, or a list of these.",
            call = call
        )
    }
}

is_valid_link <- function(x) {
    is.waive(x) ||
        is.null(x) ||
        is.numeric(x) ||
        is.character(x) ||
        inherits(x, "link_range")
}

#' @export
#' @rdname mark_draw
link_range <- function(point1, point2) {
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg ...} must be a single numeric or character index")
    }
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg ...} must be a single numeric or character index")
    }
    structure(list(point1 = point1, point2 = point2), class = "link_range")
}

make_link_data <- function(link, design, labels = NULL,
                           arg = caller_arg(link)) {
    if (is.null(link)) {
        return(NULL)
    }

    n <- .subset2(design, "nobs")
    if (!inherits(link, "AsIs") || is.character(link)) {
        # match the original data index
        if (inherits(link, "link_range")) {
            point1 <- .subset2(link, "point1")
            if (!inherits(point1, "AsIs") || is.character(point1)) {
                point1 <- vec_as_location(
                    point1,
                    n = n,
                    names = labels,
                    arg = "point1",
                    call = quote(link_range())
                )
            }
            point2 <- .subset2(link, "point2")
            if (!inherits(point2, "AsIs") || is.character(point2)) {
                point2 <- vec_as_location(
                    point2,
                    n = n,
                    names = labels,
                    arg = "point2",
                    call = quote(link_range())
                )
            }
            link <- match(c(point1, point2), .subset2(design, "index"))
            link <- (link[1L]):(link[2L])
        } else {
            link <- vec_as_location(
                link,
                n = n,
                names = labels,
                arg = arg,
                call = quote(link())
            )
            link <- match(link, .subset2(design, "index"))
        }
    } else if (inherits(link, "link_range")) {
        point1 <- .subset2(link, "point1")
        # for character, we always match the original data
        if (is.character(point1)) {
            point1 <- vec_as_location(
                point1,
                n = n,
                names = labels,
                arg = "point1",
                call = quote(link_range())
            )
            point1 <- match(point1, .subset2(design, "index"))
        }
        point2 <- .subset2(link, "point2")
        if (is.character(point2)) {
            point2 <- vec_as_location(
                point2,
                n = n,
                names = labels,
                arg = "point2",
                call = quote(link_range())
            )
            point2 <- match(point2, .subset2(design, "index"))
        }
        link <- point1:point2
    }

    # always use integer, otherwise, will cause error when drawing
    # due to loss of precision, I don't know why, it should be integer already?
    vec_cast(link, integer())
}

#' Link the observations and the panel with a line
#'
#' @param ... Additional arguments passed on to [`mark_draw()`].
#' @param element A [`element_line()`][ggplot2::element_line] object.
#' @importFrom ggplot2 element_line
#' @export
mark_line <- function(..., element = NULL) {
    assert_s3_class(element, "element_line", allow_null = TRUE)
    element <- element %||% element_line(
        color = "black",
        linewidth = 0.5,
        linetype = 1,
        lineend = "butt"
    )
    mark_pdraw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")
            data_frame0(
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
        data <- vec_rbind(!!!data)
        element_grob(
            element,
            x = data$x, y = data$y,
            id.lengths = vec_rep(2L, nrow(data) / 2L),
            default.units = "native"
        )
    }, ...)
}

#' Link the observations and the panel with a quadrilateral
#'
#' @param ... Additional arguments passed on to [`mark_draw()`].
#' @param element A [`element_polygon()`] object.
#' @export
mark_tetragon <- function(..., element = NULL) {
    assert_s3_class(element, "element_polygon", allow_null = TRUE)
    element <- element %||% element_polygon(
        fill = NA,
        color = "black",
        linewidth = 0.5,
        linetype = 1,
        alpha = NA,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
    )
    mark_pdraw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")

            # find the consecutive groups
            index <- .subset2(link, "index")
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
            id.lengths = vec_rep(4L, nrow(data) / 4L),
            default.units = "native"
        )
    }, ...)
}

#' @importFrom rlang arg_match0
mark_triangle <- function(..., orientation = "plot", element = NULL) {
    assert_s3_class(element, "element_polygon", allow_null = TRUE)
    element <- element %||% element_polygon(
        fill = NA,
        color = "black",
        linewidth = 0.5,
        linetype = 1,
        alpha = NA,
        lineend = "butt",
        linejoin = "round",
        linemitre = 10
    )
    orientation <- arg_match0(orientation, c("plot", "mark"))
    mark_pdraw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")

            # find the consecutive groups
            index <- .subset2(link, "index")
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
    class(x) <- setdiff(class(x), "ggalignMarkGtable")
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

    # for a gtable, heights are from top to the bottom,
    # we reverse the heights
    plot_heights <- rev(plot_heights)

    panel_loc <- find_panel(x)
    data <- .subset2(x, "ggalign_link_data")
    full_data1 <- .subset2(data, "full_data1")
    full_data2 <- .subset2(data, "full_data2")
    direction <- .subset2(data, "direction")
    link_index_list <- .subset2(data, "link_index")
    data_index_list <- .subset2(data, "data_index")

    # prepare output for current for loop
    coords <- vector("list", 2L)
    names(coords) <- c("link1", "link2")
    for (link in names(coords)) {
        link_index <- lapply(link_index_list, .subset2, link)
        data_index <- lapply(data_index_list, .subset2, link)
        # early exit and step into next cycle if no link
        if (all(vapply(link_index, is.null, logical(1L), USE.NAMES = FALSE))) {
            coords[link] <- list(link_index)
            next
        }
        full_breaks <- switch(link,
            link1 = full_data1,
            link2 = full_data2
        )
        spacing <- convertHeight(
            .subset2(
                data,
                switch(link,
                    link1 = "spacing1",
                    link2 = "spacing2"
                )
            ),
            "mm",
            valueOnly = TRUE
        )
        spacing <- switch_direction(
            direction,
            scales::rescale(spacing, c(0, 1), from = c(0, height)),
            scales::rescale(spacing, c(0, 1), from = c(0, width))
        )

        # each value represent an `observation`, for panel space, we use `NA`
        # obs arranged from left to top, and from bottom to top
        points <- unlist(vec_interleave(full_breaks, list(NA)), FALSE, FALSE)
        # remove the last panel space, shouldn't exist
        points <- points[-length(points)]
        sizes <- numeric(length(points))
        sizes[is.na(points)] <- spacing
        n_spacing <- length(full_breaks) - 1L

        # then, we define the link grobs
        if (is_horizontal(direction)) { # the link should be in left or right
            cell_height <- (1 - spacing * n_spacing) / sum(!is.na(points))
            sizes[!is.na(points)] <- cell_height # nobs
            yend <- cumsum(sizes)
            link_x <- switch(link,
                link1 = 0,
                link2 = 1
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

            # we have reversed the `plot_cum_heights`, so the ordering index
            # should also be reversed
            panel_index <- nrow(x) - panel_index + 1L
            panel_yend <- cumsum(plot_heights)
            panel_x <- switch(link,
                link1 = sum(l_border),
                link2 = 1 - sum(r_border)
            )
            panel_coord <- data_frame0(
                x = panel_x,
                xend = panel_x,
                y = (panel_yend - plot_heights)[panel_index],
                yend = panel_yend[panel_index]
            )
        } else {
            cell_width <- (1 - spacing * n_spacing) / sum(!is.na(points))
            sizes[!is.na(points)] <- cell_width
            xend <- cumsum(sizes)
            link_y <- switch(link,
                link1 = 1,
                link2 = 0
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
                link1 = 1 - sum(t_border),
                link2 = sum(b_border)
            )
            panel_coord <- data_frame0(
                x = (panel_xend - plot_widths)[panel_index],
                xend = panel_xend[panel_index],
                y = panel_y, yend = panel_y
            )
        }
        coords[[link]] <- lapply(seq_along(link_index), function(panel_index) {
            l_index <- .subset2(link_index, panel_index)
            if (is.null(l_index)) return(NULL) # styler: off
            d_index <- .subset2(data_index, panel_index)
            link <- vec_slice(link_coord, l_index)
            link$index <- l_index
            link$data_index <- d_index
            panel <- vec_slice(panel_coord, panel_index)
            list(panel = panel, link = link)
        })
    }
    coords <- list_transpose(coords)
    coords <- list_drop_empty(unlist(coords, FALSE, FALSE))
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
