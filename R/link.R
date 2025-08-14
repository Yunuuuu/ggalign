#' Define the links to connect a pair of observations
#'
#' @description
#' This function allows users to define links between a pair of observations,
#' facilitating the visualization of connections between related data points.
#'
#' @param .draw A function used to draw the links. The function must return a
#'   [`grob()`][grid::grob] object. If the function does not return a valid
#'   `grob`, no drawing will occur. The input data for the function must contain
#'   two arguments: a data frame for the left hand coordinates and a data frame
#'   for the right hand observation coordinates.
#' @inheritParams .link_draw
#' @seealso
#'  - [`link_line()`]
#'  - [`.link_draw()`]
#' @importFrom rlang is_empty inject
#' @importFrom grid gTree gList
#' @export
link_draw <- function(.draw, ...) {
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
            draw(.subset2(dd, "hand1"), .subset2(dd, "hand2"))
        })
        ans <- ans[vapply(ans, is.grob, logical(1L), USE.NAMES = FALSE)]
        if (!is_empty(ans)) inject(gList(!!!ans))
    }
    .link_draw(new_draw, ...)
}

#' @inherit link_draw title
#'
#' @description
#' A base version of [`link_draw()`], optimized for performance. This function
#' serves as the foundation for building other `link_*` functions that manage
#' the drawing of links between pairs of observations.
#'
#' @param .draw A function used to draw the links. The function must return a
#'   [`grob()`][grid::grob] object. If the function does not return a valid
#'   `grob`, no drawing will occur. The input data for the function contains a
#'   list, where each item is a list of two data frames: one for the left hand
#'   coordinates (`"hand1"`) and one for the right hand observations coordinates
#'   (`"hand2"`).
#' @inheritParams pair_links
#' @seealso [`link_draw()`]
#' @export
.link_draw <- function(.draw, ...) {
    if (override_call(call <- caller_call())) {
        call <- current_call()
    }
    if (!is.function(draw <- allow_lambda(.draw))) {
        cli_abort("{.arg .draw} must be a function", call = call)
    }
    args <- names(formals(draw))
    if (length(args) < 1L && args != "...") {
        cli_abort(
            "{.arg .draw} must be a function that takes at least one argument"
        )
    }
    links <- pair_links(...)
    structure(list(draw = draw, links = links), class = "ggalign_link_draw")
}

#' @export
print.ggalign_link_draw <- function(x, ...) {
    header <- sprintf("<%s>", vec_ptype_full(x))
    cat(header, sep = "\n")
    obj_print_data(.subset2(x, "links"))
    invisible(x)
}

#' Link the paired observations with a line
#'
#' @inheritParams .link_draw
#' @param .element A [`element_line()`][ggplot2::element_line] object.
#'   Vectorized fields will be recycled to match the total number of groups, or
#'   you can wrap the element with [`I()`] to recycle to match the drawing
#'   groups. The drawing groups typically correspond to the product of the
#'   number of observations from both sides, as each pair of observations will
#'   be linked with a single line.
#' @importFrom ggplot2 element_line complete_theme
#' @export
link_line <- function(..., .element = NULL) {
    assert_s3_class(.element, "element_line", allow_null = TRUE)
    default <- calc_element("ggalign.line", complete_theme(theme_get()))
    if (is.null(.element)) {
        .element <- default
    } else {
        .element <- ggplot2::merge_element(.element, default)
    }
    ans <- .link_draw(.draw = function(data) {
        data <- lapply(data, function(d) {
            # if the link is only in one side, we do nothing
            hand1 <- .subset2(d, "hand1")
            hand2 <- .subset2(d, "hand2")
            if (is.null(hand1) || is.null(hand2)) {
                return(NULL)
            }
            data <- cross_join(hand1, hand2)
            data_frame0(
                x = vec_interleave(
                    (data$x.x + data$xend.x) / 2L,
                    (data$x.y + data$xend.y) / 2L
                ),
                y = vec_interleave(
                    (data$y.x + data$yend.x) / 2L,
                    (data$y.y + data$yend.y) / 2L
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
        if (vec_size(data)) {
            element_grob(
                .element,
                x = data$x, y = data$y,
                id.lengths = vec_rep(2L, vec_size(data) / 2L),
                default.units = "native"
            )
        }
    }, ...)
    add_class(ans, "ggalign_link_line")
}

#' Link the paired observations with a quadrilateral
#'
#' @inheritParams .link_draw
#' @inheritParams mark_tetragon
#' @export
link_tetragon <- function(..., .element = NULL) {
    assert_s3_class(.element, "element_polygon", allow_null = TRUE)
    default <- calc_element("ggalign.polygon", complete_theme(theme_get()))
    if (is.null(.element)) {
        .element <- default
    } else {
        .element <- ggplot2::merge_element(.element, default)
    }
    .link_draw(.draw = function(data) {
        data <- lapply(data, function(d) {
            # if the link is only in one side, we do nothing
            if (is.null(.subset2(d, "hand1")) ||
                is.null(.subset2(d, "hand2"))) {
                return(NULL)
            }
            both <- lapply(d, function(link) {
                # find the consecutive groups
                index <- .subset2(link, "link_index")
                oindex <- order(index)
                group <- cumsum(c(0L, diff(index[oindex])) != 1L)

                # restore the order
                group <- group[order(oindex)]

                # split link into groups
                .subset2(vec_split(link, group), "val")
            })
            both <- vec_expand_grid(!!!both)
            ans <- .mapply(function(hand1, hand2) {
                data_frame0(
                    x = vec_c(
                        min(hand1$x), max(hand1$xend),
                        max(hand2$xend), min(hand2$x)
                    ),
                    y = vec_c(
                        min(hand1$y), max(hand1$yend),
                        max(hand2$yend), min(hand2$y)
                    )
                )
            }, both, NULL)
            vec_rbind(!!!ans)
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
                id.lengths = vec_rep(4L, vec_size(data) / 4L),
                default.units = "native"
            )
        }
    }, ...)
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
#' @importFrom grid makeContent convertHeight convertWidth gList setChildren
#' @export
makeContent.ggalignLinkTree <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)

    # Grab grob metadata
    full_data1 <- .subset2(x, "full_data1")
    full_data2 <- .subset2(x, "full_data2")
    direction <- .subset2(x, "direction")
    link_index_list <- .subset2(x, "link_index")
    data_index_list <- .subset2(x, "data_index")
    obs_size <- .subset2(x, "obs_size")

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

        spacing <- .subset2(x, switch(link,
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
            link
        })
    }

    # hand1 - hand2
    data <- .mapply(function(hand1, hand2) {
        list(hand1 = hand1, hand2 = hand2)
    }, coords, NULL)
    draw <- .subset2(x, "draw")
    if (is.grob(grob <- draw(data))) { # wrap single grob to a gList
        grob <- gList(grob)
    }
    if (is.gList(grob)) {
        setChildren(x, grob)
    } else {
        x
    }
}
