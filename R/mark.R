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
#' @inheritParams .mark_draw
#' @seealso
#'  - [`mark_line()`]
#'  - [`mark_tetragon()`]
#'  - [`mark_triangle()`]
#'  - [`.mark_draw()`]
#' @importFrom rlang is_empty inject
#' @importFrom grid gTree gList
#' @export
mark_draw <- function(.draw, ...) {
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function")
    }
    args <- names(formals(draw))
    if (length(args) < 2L && args != "...") {
        cli_abort(
            "{.arg .draw} must be a function that takes at least two arguments"
        )
    }
    new_draw <- function(data) {
        ans <- lapply(data, function(dd) {
            draw(.subset2(dd, "panel"), .subset2(dd, "link"))
        })
        ans <- ans[vapply(ans, is.grob, logical(1L), USE.NAMES = FALSE)]
        if (!is_empty(ans)) {
            gTree(children = inject(gList(!!!ans)))
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
#' @inheritParams pair_links
#' @seealso [`mark_draw()`]
#' @export
.mark_draw <- function(.draw, ...) {
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function")
    }
    args <- names(formals(draw))
    if (length(args) < 1L && args != "...") {
        cli_abort(
            "{.arg .draw} must be a function that takes at least one argument"
        )
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
#' @inheritParams .mark_draw
#' @param .element A [`element_line()`][ggplot2::element_line] object.
#'   Vectorized fields will be recycled to match the total number of groups, or
#'   you can wrap the element with [`I()`] to recycle to match the drawing
#'   groups. The drawing groups typically correspond to the number of
#'   observations, as each observation will be linked with the plot panel.
#' @importFrom ggplot2 element_line complete_theme is_theme_element
#' @export
mark_line <- function(..., .element = NULL) {
    assert_s3_class(.element,
        is_theme_element,
        "element_line",
        type = "line",
        allow_null = TRUE
    )
    default <- calc_element("line", complete_theme(theme_get()))
    if (is.null(.element)) {
        .element <- default
    } else {
        .element <- ggplot2::merge_element(.element, default)
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
        if (inherits(.element, "AsIs")) {
            .element <- element_rep_len(.element,
                length.out = sum(list_sizes(data)) / 2L
            )
        } else {
            .element <- element_rep_len(.element, length.out = length(data))
            .element <- element_vec_rep_each(.element,
                times = list_sizes(data) / 2L
            )
        }
        data <- vec_rbind(!!!data)
        element_grob(
            .element,
            x = data$x, y = data$y,
            id.lengths = vec_rep(2L, vec_size(data) / 2L),
            default.units = "native"
        )
    }, ...)
}

#' Link the observations and the panel with a quadrilateral
#'
#' @inheritParams .mark_draw
#' @param .element A [`element_polygon()`][ggplot2::element_polygon] object.
#'   Vectorized fields will be recycled to match the total number of groups, or
#'   you can wrap the element with [`I()`] to recycle to match the drawing
#'   groups. The drawing groups are usually the same as the defined groups, but
#'   they will differ when the defined group of observations is separated and
#'   cannot be linked with a single quadrilateral. In such cases, the number of
#'   drawing groups will be larger than the number of defined groups.
#' @importFrom ggplot2 is_theme_element
#' @export
mark_tetragon <- function(..., .element = NULL) {
    assert_s3_class(.element,
        is_theme_element,
        "element_polygon",
        type = "polygon",
        allow_null = TRUE
    )
    default <- calc_element("polygon", complete_theme(theme_get()))
    if (is.null(.element)) {
        .element <- default
    } else {
        .element <- ggplot2::merge_element(.element, default)
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
        if (inherits(.element, "AsIs")) {
            .element <- element_rep_len(.element,
                length.out = sum(list_sizes(data)) / 4L
            )
        } else {
            .element <- element_rep_len(.element, length.out = length(data))
            .element <- element_vec_rep_each(.element,
                times = list_sizes(data) / 4L
            )
        }
        data <- vec_rbind(!!!data)
        if (vec_size(data)) {
            element_grob(
                .element,
                x = data$x, y = data$y,
                id.lengths = vec_rep(4L, nrow(data) / 4L),
                default.units = "native"
            )
        }
    }, ...)
}

#' Link the observations and the panel with a triangle
#'
#' @inheritParams .mark_draw
#' @param orientation A single string, either `"plot"` or `"observation"`,
#'   indicating the base of the triangle.
#' @param .element An [`element_polygon()`][ggplot2::element_polygon] object.
#'   Vectorized fields will be recycled to match the total number of groups, or
#'   you can wrap the element with [`I()`] to recycle to match the drawing
#'   groups.
#'   - When `orientation` is `"plot"`, the drawing groups typically correspond
#'     to the number of observations.
#'   - When `orientation` is `"observation"`, the drawing groups usually match
#'     the defined groups, but will differ if the defined group of observations
#'     is separated and cannot be linked with a single triangle. In this case,
#'     the number of drawing groups will be larger than the number of defined
#'     groups.
#' @importFrom rlang arg_match0
#' @export
mark_triangle <- function(..., orientation = "plot", .element = NULL) {
    assert_s3_class(.element,
        is_theme_element,
        "element_polygon",
        type = "polygon",
        allow_null = TRUE
    )
    default <- calc_element("polygon", complete_theme(theme_get()))
    if (is.null(.element)) {
        .element <- default
    } else {
        .element <- ggplot2::merge_element(.element, default)
    }
    orientation <- arg_match0(orientation, c("plot", "observation"))
    .mark_draw(.draw = function(data) {
        data <- lapply(data, function(d) {
            panel <- .subset2(d, "panel")
            link <- .subset2(d, "link")
            if (identical(orientation, "plot")) {
                # for each link, we draw a triangle
                triangle_list <- lapply(vec_seq_along(link), function(i) {
                    dd <- vec_slice(link, i)
                    data_frame0(
                        x = vec_c(panel$x, panel$xend, (dd$x + dd$xend) / 2L),
                        y = vec_c(panel$y, panel$yend, (dd$y + dd$yend) / 2L),
                    )
                })
            } else {
                # find the consecutive groups
                index <- .subset2(link, "link_index")
                oindex <- order(index)
                group <- cumsum(c(0L, diff(index[oindex])) != 1L)

                # restore the order
                group <- group[order(oindex)]

                # split link into groups
                link <- vec_split(link, group)

                # for each group, we draw a triangle
                triangle_list <- lapply(.subset2(link, "val"), function(dd) {
                    data_frame0(
                        x = vec_c(
                            (panel$x + panel$xend) / 2L,
                            max(dd$xend), min(dd$x)
                        ),
                        y = vec_c(
                            (panel$y + panel$yend) / 2L,
                            max(dd$yend), min(dd$y)
                        )
                    )
                })
            }
            vec_rbind(!!!triangle_list)
        })
        if (inherits(.element, "AsIs")) {
            .element <- element_rep_len(.element,
                length.out = sum(list_sizes(data)) / 3L
            )
        } else {
            .element <- element_rep_len(.element, length.out = length(data))
            .element <- element_vec_rep_each(.element,
                times = list_sizes(data) / 3L
            )
        }
        data <- vec_rbind(!!!data)
        if (vec_size(data)) {
            element_grob(
                .element,
                x = data$x, y = data$y,
                id.lengths = vec_rep(3L, nrow(data) / 3L),
                default.units = "native"
            )
        }
    }, ...)
}

#####################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
patch.ggalign_mark <- function(x) {
    link_data <- attr(x, "ggalign_link_data", exact = TRUE)
    attr(x, "ggalign_link_data") <- NULL
    Parent <- NextMethod()
    ggproto(
        "PatchAlignMark", Parent,
        link_data = link_data,
        border_sizes = function(self, gt, options) {
            out <- ggproto_parent(Parent, self)$border_sizes(gt, options)
            free_spaces(out, .TLBR)
        },
        align_border = function(self, gt, t, l, b, r, options) {
            gt
        },
        place = function(self, gtable, gt, t, l, b, r, i,
                         bg_z, plot_z, options) {
            ggproto_parent(Parent, self)$place(
                gtable, gt,
                t + TOP_BORDER, l + LEFT_BORDER,
                t + TOP_BORDER, l + LEFT_BORDER,
                i, bg_z, plot_z, options
            )
        },
        place_gt = function(self, gtable, gt, t, l, b, r, i, z, options) {
            ggproto_parent(Parent, self)$place_gt(
                gtable, markGrob(gt, self$link_data),
                t, l, b, r, i, z, options
            )
        }
    )
}

#' @importFrom grid gTree
markGrob <- function(grob, link, ...) {
    gTree(grob = grob, link = link, ..., cl = "markGrob")
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
#' @importFrom grid makeContent setChildren gList
#' @importFrom grid unit convertHeight convertWidth viewport
#' @importFrom stats reorder
#' @export
makeContent.markGrob <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)

    # Grab the inner grob metadata
    inner <- .subset2(x, "grob")
    plot_widths <- compute_null_width(
        .subset2(inner, "widths"),
        valueOnly = TRUE
    )
    plot_widths <- scales::rescale(plot_widths, c(0, 1), from = c(0, width))
    plot_heights <- compute_null_height(
        .subset2(inner, "heights"),
        valueOnly = TRUE
    )
    plot_heights <- scales::rescale(plot_heights, c(0, 1), from = c(0, height))

    data <- .subset2(x, "link")
    panel_loc <- find_panel(inner)
    full_data1 <- .subset2(data, "full_data1")
    full_data2 <- .subset2(data, "full_data2")
    direction <- .subset2(data, "direction")
    link_index_list <- .subset2(data, "link_index")
    data_index_list <- .subset2(data, "data_index")
    obs_size <- .subset2(data, "obs_size")

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
            obs_height <- (1 - spacing * n_spacing) / sum(!is.na(points))
            sizes[!is.na(points)] <- obs_height # nobs
            yend <- cumsum(sizes)
            link_x <- switch(link,
                hand1 = 0,
                hand2 = 1
            )
            # by default, the height for each observation is `1`,
            # if we define obs size, we just re-scale it
            removed <- (1 - obs_size) * obs_height
            link_coord <- data_frame0(
                x = link_x, xend = link_x,
                y = yend - sizes + removed / 2L,
                yend = yend - removed / 2L
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
            panel_index <- nrow(inner) - panel_index + 1L
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
            obs_width <- (1 - spacing * n_spacing) / sum(!is.na(points))
            sizes[!is.na(points)] <- obs_width
            xend <- cumsum(sizes)
            link_y <- switch(link,
                hand1 = 1,
                hand2 = 0
            )
            # by default, the width for each observation is `1`,
            # if we define obs size, we just re-scale it
            removed <- (1 - obs_size) * obs_width
            link_coord <- data_frame0(
                x = xend - sizes + removed / 2L,
                xend = xend - removed / 2L,
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
        link_panels <- factor(link_panels, names(full_breaks))
        coords[[link]] <- lapply(seq_along(link_index), function(i) {
            l_index <- .subset2(link_index, i)
            if (is.null(l_index)) return(NULL) # styler: off
            d_index <- .subset2(data_index, i)
            link <- vec_slice(link_coord, l_index)
            link$link_id <- nms[i]
            link$link_panel <- vec_slice(link_panels, l_index)
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
    if (is.gList(mark <- draw(coords))) mark <- gTree(children = mark)
    if (is.grob(mark)) {
        layout <- .subset2(inner, "layout")
        panels <- layout[
            grepl("^panel", .subset2(layout, "name")), ,
            drop = FALSE
        ]
        inner <- gtable_add_grob(
            inner,
            grobs = mark,
            t = 1L, l = 1L, b = -1L, r = -1L,
            # always draw with panel area
            z = min(panels$z)
        )
    }
    setChildren(x, children = gList(inner))
}
