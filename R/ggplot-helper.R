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
    if (grepl("[^tlbrxy]", axes)) { # nocov start
        cli_abort(sprintf(
            "{.arg axes} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    } # nocov end
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
#'  - `element_vec_fields`: Identify which fields are vectorized. Developers
#'    should implement this when creating new element classes.
#'  - `element_vec`: Apply a custom function `.fn` to vectorized fields.
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
#' @name element_vec
element_vec_fields <- S7::new_generic("element_vec_fields", ".el")

#' @export
#' @rdname element_vec
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

S7::method(element_vec_fields, ggplot2::element_blank) <- function(.el) NULL

S7::method(element_vec_fields, ggplot2::element_polygon) <- function(.el) {
    c("fill", "colour", "linewidth", "linetype")
}

S7::method(element_vec_fields, ggplot2::element_point) <- function(.el) {
    c("colour", "shape", "size", "fill", "stroke")
}

S7::method(element_vec_fields, ggplot2::element_rect) <- function(.el) {
    c("fill", "colour", "linewidth", "linetype")
}

S7::method(element_vec_fields, ggplot2::element_line) <- function(.el) {
    c("colour", "linewidth", "linetype", "lineend")
}

S7::method(element_vec_fields, ggplot2::element_text) <- function(.el) {
    c(
        "family", "face", "colour", "size", "hjust", "vjust",
        "angle", "lineheight"
    )
}

S7::method(element_vec_fields, S7::class_any) <- function(.el) {
    stop_input_type(.el, "an element")
}

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
ggplot_add.ggalign_no_expansion <- function(object, plot, object_name, ...) {
    borders <- .subset2(object, "borders")
    ParentLayout <- plot$layout

    # tricks to ensure remove `coord` won't remove `no_expansion()`
    plot$layout <- ggproto(NULL, ParentLayout,
        setup_panel_params = function(self) {
            ParentCoord <- self$coord
            self$coord <- ggproto(NULL, ParentCoord,
                setup_panel_params = function(self, scale_x, scale_y,
                                              params = list()) {
                    if (!is.null(scale_x)) {
                        expansion <- scale_x$expand %|w|%
                            ggfun("default_expansion")(
                                scale_y,
                                expand = params$expand[c(4, 2)]
                            )
                        if (any(borders == "left")) {
                            expansion[1:2] <- 0
                        }
                        if (any(borders == "right")) {
                            expansion[3:4] <- 0
                        }
                        scale_x$expand <- expansion
                    }
                    if (!is.null(scale_y)) {
                        expansion <- scale_y$expand %|w|%
                            ggfun("default_expansion")(
                                scale_y,
                                expand = params$expand[c(3, 1)]
                            )
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
