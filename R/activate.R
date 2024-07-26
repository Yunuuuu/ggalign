#' Determine the context of subsequent manipulations
#'
#' @param x A [ggheatmap][ggheat] object.
#' @param what What should get activated? Possible values are `"top"`, `"left"`,
#' `"bottom"`, and `"right"`. For `active`, this can be also `NULL`, which
#' means set active context into the `heatmap` itself.
#' @return
#' - `activate`: A object with the same class of `x`, whose active context will
#'               be `set`.
#' @export
activate <- function(x, what) UseMethod("activate")

#' @export
activate.LayoutHeatmap <- function(x, what) {
    what <- match_context(what)
    set_context(x, what)
}

#' @return
#' - `heatmap_active`: A `active` object which can be added into
#'   [LayoutHeatmap][layout_heatmap].
#' @export
#' @rdname activate
heatmap_active <- function(what = NULL) {
    context <- match_context(what)
    structure(context %||% "plot", class = c("heatmap_active", "active"))
}

#' @return
#' - `stack_active`: A `active` object which can be added into
#'   [LayoutStack][layout_stack].
#' @export
#' @rdname activate
stack_active <- function(what) {
    structure(what, class = c("stack_active", "active"))
}

match_context <- function(what) {
    if (!is.null(what)) what <- match.arg(what, GGHEAT_ELEMENTS)
    what
}

GGHEAT_ELEMENTS <- c("top", "left", "bottom", "right")

#' @keywords internal
set_context <- function(x, context) UseMethod("set_context")

#' @export
set_context.LayoutHeatmap <- function(x, context) {
    slot(x, "active") <- context
    x
}

#' @export
set_context.annotations <- function(x, context) {
    attr(x, "active") <- context
    x
}

#' @keywords internal
get_context <- function(x) UseMethod("get_context")

#' @export
get_context.LayoutHeatmap <- function(x) slot(x, "active")

#' @export
get_context.annotations <- function(x) attr(x, "active")
