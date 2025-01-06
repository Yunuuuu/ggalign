# Exported function for ggplot2
# Usually a quick shortcuts to define something
#
#' Remove axis elements
#'
#' @param axes Which axes elements should be removed? A string containing
#' one or more of `r oxford_and(c(.tlbr, "x", "y"))`.
#' @param text If `TRUE`, will remove the axis labels.
#' @param ticks If `TRUE`, will remove the axis ticks.
#' @param title If `TRUE`, will remove the axis title.
#' @param line If `TRUE`, will remove the axis line.
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples
#' p <- ggplot() +
#'     geom_point(aes(x = wt, y = qsec), data = mtcars)
#' p + theme_no_axes()
#' p + theme_no_axes("b")
#' p + theme_no_axes("l")
#' @importFrom rlang inject
#' @importFrom ggplot2 theme element_blank
#' @export
theme_no_axes <- function(axes = "xy", text = TRUE, ticks = TRUE,
                          title = TRUE, line = FALSE) {
    assert_string(axes, empty_ok = FALSE)
    if (grepl("[^tlbrxy]", axes)) {
        cli_abort(sprintf(
            "{.arg axes} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    }
    axes <- split_position(axes)
    el <- list(text = text, ticks = ticks, title = title, line = line)
    el <- names(el)[vapply(el, isTRUE, logical(1L), USE.NAMES = FALSE)]
    el_axis <- el_pos <- NULL
    if (length(positions <- vec_set_intersect(axes, .tlbr))) {
        positions <- .subset(
            c(t = "top", l = "left", b = "bottom", r = "right"),
            positions
        )
        el_pos <- vec_expand_grid(pos = positions, el = el)
        el_pos <- paste("axis",
            .subset2(el_pos, "el"),
            if_else(.subset2(el_pos, "pos") %in% c("top", "bottom"), "x", "y"),
            .subset2(el_pos, "pos"),
            sep = "."
        )
    }
    if (length(axes <- vec_set_intersect(axes, c("x", "y")))) {
        el_axis <- vec_expand_grid(axes = axes, el = el)
        el_axis <- paste("axis",
            .subset2(el_axis, "el"), .subset2(el_axis, "axes"),
            sep = "."
        )
    }
    el <- c(el_axis, el_pos)
    el <- vec_set_names(vec_rep(list(element_blank()), length(el)), el)
    inject(theme(!!!el, validate = FALSE))
}

##########################################################################
#' Theme Polygon elements
#'
#' Draw polygon.
#'
#' @inheritParams ggplot2::element_rect
#' @inheritParams geom_rect3d
#' @inheritParams ggplot2::fill_alpha
#' @seealso [`element_rect`][ggplot2::element_rect]
#' @return A `element_polygon` object
#' @export
element_polygon <- function(fill = NULL, colour = NULL, linewidth = NULL,
                            linetype = NULL, alpha = NULL, lineend = NULL,
                            linejoin = NULL, linemitre = NULL, color = NULL,
                            inherit.blank = FALSE) {
    if (!is.null(color)) colour <- color
    structure(
        list(
            fill = fill, colour = colour, alpha = alpha,
            linewidth = linewidth, linetype = linetype,
            lineend = lineend, linejoin = linejoin, linemitre = linemitre,
            inherit.blank = inherit.blank
        ),
        class = c("ggalign_element_polygon", "element_polygon", "element")
    )
}

#' @importFrom grid gpar
#' @importFrom ggplot2 element_grob
#' @export
element_grob.ggalign_element_polygon <- function(element,
                                                 x = c(0, 0.5, 1, 0.5),
                                                 y = c(0.5, 1, 0.5, 0),
                                                 fill = NULL,
                                                 colour = NULL,
                                                 linewidth = NULL,
                                                 linetype = NULL, ...) {
    gp <- gpar(
        lwd = ggfun("len0_null")(linewidth * .pt),
        col = colour,
        fill = fill,
        lty = linetype
    )
    element_gp <- gpar(
        lwd = ggfun("len0_null")(element$linewidth * .pt),
        col = element$colour,
        fill = fill_alpha(element$fill, element$alpha %||% NA),
        lty = element$linetype,
        lineend = element$lineend,
        linejoin = element$linejoin,
        linemitre = element$linemitre
    )
    grid::polygonGrob(
        x = x, y = y,
        gp = ggfun("modify_list")(element_gp, gp), ...
    )
}

#' Theme curve elements
#'
#' Draw curve.
#'
#' @inheritParams element_polygon
#' @inheritParams grid::curveGrob
#' @param arrow.fill Fill colour for arrows.
#' @return A `element_curve` object
#' @export
element_curve <- function(colour = NULL, linewidth = NULL, linetype = NULL,
                          lineend = NULL, color = NULL, curvature = NULL,
                          angle = NULL, ncp = NULL, shape = NULL,
                          arrow = NULL, arrow.fill = NULL,
                          inherit.blank = FALSE) {
    colour <- color %||% colour
    arrow.fill <- arrow.fill %||% colour
    arrow <- arrow %||% FALSE
    structure(
        list(
            colour = colour, linewidth = linewidth, linetype = linetype,
            lineend = lineend, curvature = curvature, angle = angle,
            ncp = ncp, shape = shape, arrow = arrow, arrow.fill = arrow.fill,
            inherit.blank = inherit.blank
        ),
        class = c("ggalign_element_curve", "element_curve", "element")
    )
}

#' @importFrom grid gpar gTree gList
#' @importFrom ggplot2 element_grob
#' @export
element_grob.ggalign_element_curve <- function(element, x = 0:1, y = 0:1,
                                               colour = NULL, linewidth = NULL, linetype = NULL, lineend = NULL,
                                               arrow.fill = NULL,
                                               default.units = "npc",
                                               id = NULL,
                                               id.lengths = NULL, ...) {
    arrow <- if (is.logical(element$arrow) && !element$arrow) {
        NULL
    } else {
        element$arrow
    }
    if (is.null(arrow)) {
        arrow.fill <- colour
        element$arrow.fill <- element$colour
    }
    # The gp settings can override element_gp
    gp <- gpar(
        col = colour,
        fill = arrow.fill %||% colour,
        lwd = ggfun("len0_null")(linewidth * .pt),
        lty = linetype,
        lineend = lineend
    )
    element_gp <- gpar(
        col = element$colour,
        fill = element$arrow.fill %||% element$colour,
        lwd = ggfun("len0_null")(element$linewidth * .pt),
        lty = element$linetype,
        lineend = element$lineend
    )
    gp <- ggfun("modify_list")(element_gp, gp)
    if (is.null(id)) {
        if (is.null(id.lengths)) {
            id <- vec_rep(1L, length(x))
        } else {
            id <- vec_rep_each(seq_along(id.lengths), id.lengths)
        }
    }
    index_list <- .subset2(vec_split(seq_along(x), id), "val")
    ans <- lapply(index_list, function(index) {
        grid::curveGrob(
            vec_slice(x, index),
            vec_slice(y, index),
            default.units = default.units,
            gp = gp[index],
            curvature = element$curvature,
            angle = element$angle,
            ncp = element$ncp,
            shape = element$shape,
            arrow = arrow,
            square = FALSE, squareShape = 1,
            inflect = FALSE, open = TRUE,
            ...
        )
    })
    gTree(children = inject(gList(!!!ans)))
}

##########################################################################
element_lengths <- function(.el, .fn, ...) {
    fields <- element_vec_fields(.el)
    if (is.null(fields)) 0L
    lengths(.el[fields])
}

#' Apply a function to the fields of an element object
#'
#' @description
#' For an [`element`][ggplot2::element_blank] object, some fields are
#' vectorized, while others are not. This function allows you to apply a
#' function to the vectorized fields.
#'
#' The following helper functions are available:
#'
#'  - `element_rep`: Applies [`rep()`].
#'  - `element_rep_len`: Applies [`rep_len()`].
#'  - `element_vec_recycle`: Applies [`vec_recycle()`][vctrs::vec_recycle].
#'  - `element_vec_rep`: Applies [`vec_rep()`][vctrs::vec_rep].
#'  - `element_vec_rep_each`: Applies [`vec_rep_each()`][vctrs::vec_rep_each].
#'  - `element_vec_slice`: Applies [`vec_slice()`][vctrs::vec_slice].
#'
#' @param .el An [`element`][ggplot2::element_blank] object.
#' @param .fn The function to be applied to the vectorized fields of the element
#' object.
#' @param ... Additional arguments passed on to `fn`.
#' @export
element_vec <- function(.el, .fn, ...) {
    fields <- element_vec_fields(.el)
    if (is.null(fields)) {
        return(.el)
    }
    .el[fields] <- lapply(.el[fields], function(value) {
        if (!is.null(value)) value <- .fn(value, ...)
        value
    })
    .el
}

#' @export
#' @rdname element_vec
element_rep <- function(.el, ...) element_vec(.el, rep, ...)

#' @param length.out Non-negative integer. The desired length of the output
#' vector. Other inputs will be coerced to a double vector and the first element
#' taken. Ignored if `NA` or invalid.
#' @export
#' @rdname element_vec
element_rep_len <- function(.el, length.out, ...) {
    element_vec(.el, rep_len, ..., length.out = length.out)
}

#' @inheritParams vctrs::vec_recycle
#' @export
#' @rdname element_vec
element_vec_recycle <- function(.el, size, ...) {
    element_vec(.el, vec_recycle, size = size, ...)
}

#' @inheritParams vctrs::vec_rep
#' @export
#' @rdname element_vec
element_vec_rep <- function(.el, times, ...) {
    element_vec(.el, vec_rep, times = times, ...)
}

#' @export
#' @rdname element_vec
element_vec_rep_each <- function(.el, times, ...) {
    element_vec(.el, vec_rep_each, times = times, ...)
}

#' @inheritParams vctrs::vec_slice
#' @importFrom grid is.unit
#' @export
#' @rdname element_vec
element_vec_slice <- function(.el, i, ...) {
    element_vec(.el, function(x) {
        if (is.unit(x)) x[i, ...] else vec_slice(x, i, ...)
    })
}

element_vec_fields <- function(el) UseMethod("element_vec_fields")

#' @export
element_vec_fields.ggalign_element_polygon <- function(el) {
    c(
        "fill", "colour", "linewidth", "linetype",
        "lineend", "linejoin", "linemitre", "alpha"
    )
}

#' @export
element_vec_fields.element_blank <- function(el) NULL

#' @export
element_vec_fields.element_polygon <- function(el) {
    c("fill", "colour", "linewidth", "linetype")
}

#' @export
element_vec_fields.element_point <- function(el) {
    c("colour", "shape", "size", "fill", "stroke")
}

#' @export
element_vec_fields.element_rect <- function(el) {
    c("fill", "colour", "linewidth", "linetype")
}

#' @export
element_vec_fields.element_line <- function(el) {
    c("colour", "linewidth", "linetype", "lineend")
}

#' @export
element_vec_fields.element_text <- function(el) {
    c(
        "family", "face", "colour", "size", "hjust", "vjust",
        "angle", "lineheight"
    )
}

#' @export
element_vec_fields.default <- function(el) stop_input_type(el, "an element")

######################################################
#' Remove scale expansion
#'
#' @param borders Which border should be removed? A string containing
#' one or more of `r oxford_and(c(.tlbr, "x", "y"))`.
#' @return An object which can be added to ggplot.
#' @export
no_expansion <- function(borders = "tlbr") {
    assert_string(borders, empty_ok = FALSE)
    if (grepl("[^tlbrxy]", borders)) {
        cli_abort(sprintf(
            "{.arg borders} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    }
    borders <- .subset(list(
        t = "top", l = "left", b = "bottom", r = "right",
        x = c("left", "right"), y = c("bottom", "top")
    ), split_position(borders))
    borders <- vec_unique(unlist(borders, recursive = FALSE, use.names = FALSE))
    # expansion in x-axis
    structure(list(borders = borders), class = c("ggalign_no_expansion"))
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_no_expansion <- function(object, plot, object_name) {
    borders <- .subset2(object, "borders")
    ParentLayout <- .subset2(plot, "layout")

    # tricks to ensure remove `coord` won't remove `no_expansion()`
    plot$layout <- ggproto(NULL, ParentLayout,
        setup_panel_params = function(self) {
            ParentCoord <- self$coord
            self$coord <- ggproto(NULL, ParentCoord,
                setup_panel_params = function(self, scale_x, scale_y,
                                              params = list()) {
                    if (!is.null(scale_x) &&
                        any(borders == c("left", "right"))) {
                        expansion <- scale_x$expand %|w|%
                            ggfun("default_expansion")(scale_x, expand = TRUE)
                        if (any(borders == "left")) {
                            expansion[1:2] <- 0
                        }
                        if (any(borders == "right")) {
                            expansion[3:4] <- 0
                        }
                        scale_x$expand <- expansion
                    }
                    if (!is.null(scale_y) &&
                        any(borders == c("bottom", "top"))) {
                        expansion <- scale_y$expand %|w|%
                            ggfun("default_expansion")(scale_y, expand = TRUE)
                        if (any(borders == "bottom")) {
                            expansion[1:2] <- 0
                        }
                        if (any(borders == "top")) {
                            expansion[3:4] <- 0
                        }
                        scale_y$expand <- expansion
                    }
                    ggproto_parent(ParentCoord, self)$setup_panel_params(
                        scale_x = scale_x, scale_y = scale_y, params = params
                    )
                }
            )
            ggproto_parent(ParentLayout, self)$setup_panel_params()
        }
    )
    plot
}
