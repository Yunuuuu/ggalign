#' @param ans Whether to assign the final results into the 'ans' variable.
#' @noRd
body_append <- function(fn, ..., ans = FALSE) {
    args <- rlang::fn_fmls(fn)
    body <- rlang::fn_body(fn)
    body <- as.list(body)
    if (ans) body[[length(body)]] <- rlang::expr(ans <- !!body[[length(body)]])
    body <- as.call(c(body, rlang::enexprs(...)))
    rlang::new_function(args, body)
}

#############################################################
#' Add components to `Layout`
#'
#' @param e1 A `r rd_layout()`.
#' @param e2 An object to be added to the plot, including [gg][ggplot2::+.gg]
#' elements or [align] object.
#' @return A modified `Layout` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     ggalign() +
#'     geom_point(aes(y = value))
#' @aliases +.Layout +.HeatmapLayout +.ggheatmap +.StackLayout +.ggstack +.QuadLayout
#' @name layout-add
NULL

#' @export
#' @rdname layout-add
methods::setMethod("+", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    if (is.null(e2)) return(e1) # styler: off
    if (inherits(e2, "layout_title")) {
        e1@titles <- update_non_waive(e1@titles, e2)
        return(e1)
    }

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_add(e1, e2, e2name)
})

#' @keywords internal
layout_add <- function(layout, object, object_name) {
    UseMethod("layout_add")
}

#' @export
layout_add.QuadLayout <- function(layout, object, object_name) {
    quad_layout_add(object, layout, object_name)
}

#' @export
layout_add.StackLayout <- function(layout, object, object_name) {
    stack_layout_add(object, layout, object_name)
}

#########################################################
#' Layout operator
#'
#' @details
#' In order to reduce code repetition `ggalign` provides two operators for
#' adding ggplot elements (geoms, themes, facets, etc.) to multiple/all plots in
#' `r rd_layout()`.
#'
#' Like `patchwork`, `&` add the element to all plots in the plot. If the
#' element is a [theme][ggplot2::theme], this will also modify the layout
#' theme.
#'
#' Unlike `patchwork`, the `-` operator adds ggplot2 elements (geoms, themes,
#' facets, etc.) rather than a ggplot plot. The key difference between `&` and
#' `-` is in how they behave in [`heatmap_layout()`]. The `-` operator only
#' applies the element to the current active context in [`heatmap_layout()`].
#' Using `-` might seem unintuitive if you think of the operator as "subtract",
#' the underlying reason is that `-` is the only operator in the same precedence
#' group as `+`.
#'
#' @param e1 A `r rd_layout()`.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @examples
#' mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(mat) +
#'     anno_top() +
#'     align_dendro() &
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#' ggheatmap(mat) +
#'     anno_top() +
#'     align_dendro() -
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#'
#' @aliases &.Layout &.HeatmapLayout &.ggheatmap &.StackLayout &.ggstack &.QuadLayout
#' @name layout-operator
NULL

#' @rdname layout-operator
#' @export
methods::setMethod("&", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }
    if (is.null(e2)) return(e1) # styler: off
    if (inherits(e2, "layout_title")) {
        cli::cli_abort(c(
            "Cannot use {.code &} to change the layout titles",
            i = "Try to use {.code +} instead"
        ))
    }

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    e1 <- layout_and_add(e1, e2, e2name)

    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    if (inherits(e2, "theme")) {
        e1@theme <- e1@theme + e2
    }
    e1
})

#' @keywords internal
layout_and_add <- function(layout, object, object_name) {
    UseMethod("layout_and_add")
}

#' @export
layout_and_add.QuadLayout <- function(layout, object, object_name) {
    quad_layout_and_add(object, layout, object_name)
}

#' @export
layout_and_add.StackLayout <- function(layout, object, object_name) {
    stack_layout_and_add(object, layout, object_name)
}

#########################################################
#' @aliases -.Layout -.HeatmapLayout -.ggheatmap -.StackLayout -.ggstack -.QuadLayout
#' @rdname layout-operator
#' @export
methods::setMethod("-", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code -} with a single argument.",
            "i" = "Did you accidentally put {.code -} on a new line?"
        ))
    }
    if (is.null(e2)) return(e1) # styler: off
    if (inherits(e2, "layout_title")) {
        cli::cli_abort(c(
            "Cannot use {.code -} to change the layout titles",
            i = "Try to use {.code +} instead"
        ))
    }

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_subtract(e1, e2, e2name)
})

#' @keywords internal
layout_subtract <- function(layout, object, object_name) {
    UseMethod("layout_subtract")
}

#' @export
layout_subtract.QuadLayout <- function(layout, object, object_name) {
    quad_layout_subtract(object, layout, object_name)
}

#' @export
layout_subtract.StackLayout <- function(layout, object, object_name) {
    stack_layout_subtract(object, layout, object_name)
}
