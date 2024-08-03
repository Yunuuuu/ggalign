#' Determine the context of subsequent manipulations
#'
#' @param x A [ggheatmap][ggheatmap] object.
#' @param what What should get activated? Possible values are follows:
#'    * A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#'    * `NULL`: means set the active context into the `heatmap` itself.
#' @return A object with the same class of `x`, whose active context will be
#' `set`.
#' @export
activate <- function(x, what) UseMethod("activate")

#' @export
activate.LayoutHeatmap <- function(x, what) {
    what <- match_context(what)
    set_context(x, what)
}

#' Determine the context of subsequent manipulations
#'
#' @param what What should get activated? Possible values are follows:
#'    * A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#'    * `NULL`: means set the active context into the `heatmap` itself.
#'    * `missing` or `NA`: don't change the context, use current active context.
#'
#' If the active context is a string of `"top"`, `"left"`, `"bottom"`, or
#' `"right"`. We can also set the stack (heatmap annotation) `size`, `guides`
#' and `align_axis_title`.
#'
#' @param size A [unit][grid::unit] object to set the size of the heatmap
#' annotation.
#' @inheritParams layout_heatmap
#' @return A `active` object which can be added into
#' [LayoutHeatmap][layout_heatmap].
#' @export
hmanno <- function(what, size = NULL, guides = NULL, align_axis_title = NULL) {
    if (missing(what)) {
        what <- NA_character_
    } else if (is.null(what)) { # activate
        what <- "plot"
    } else if (is_scalar(what) && is.na(what)) {
        what <- as.character(what)
    } else {
        what <- match.arg(what, GGHEAT_ELEMENTS)
    }
    if (!is.null(size)) size <- set_size(size)
    structure(what,
        size = size,
        guides = guides,
        align_axis_title = align_axis_title,
        class = c("heatmap_active", "active")
    )
}

# #' @return
# #' - `active`: A `active` object which can be added into
# #'   [LayoutStack][layout_stack].
# #' @export
# #' @rdname hmanno
# active <- function(what) {
#     structure(what, class = c("stack_active", "active"))
# }

match_context <- function(what) {
    if (!is.null(what)) what <- match.arg(what, GGHEAT_ELEMENTS)
    what
}

GGHEAT_ELEMENTS <- c("top", "left", "bottom", "right")

#' @keywords internal
set_context <- function(x, context) UseMethod("set_context")

#' @importFrom methods slot<-
#' @export
set_context.Layout <- function(x, context) {
    slot(x, "active") <- context
    x
}

#' @keywords internal
get_context <- function(x) UseMethod("get_context")

#' @importFrom methods slot
#' @export
get_context.Layout <- function(x) slot(x, "active")
