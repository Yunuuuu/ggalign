#' Determine the context of subsequent manipulations
#'
#' @param x A [ggheatmap][ggheat] object.
#' @param what What should get activated? Possible values are `"top"`, `"left"`,
#' `"bottom"`, and `"right"`. For `active`, this can be also `NULL`, which
#' means set active context into the `heatmap` itself.
#' @return
#' - `activate`/`deactivate`: A object with the same class of `x`, whose active
#'                            context will be `set` or `unset`.
#' @export
activate <- function(x, what) UseMethod("activate")

#' @export
#' @rdname activate
deactivate <- function(x) UseMethod("deactivate")

#' @export
activate.ggheatmap <- function(x, what) {
    if (is.null(what)) {
        cli::cli_abort(paste(
            "{.arg what} must be a string of ",
            oxford_comma(GGHEAT_ELEMENTS, final = "or")
        ))
    }
    what <- match.arg(what, GGHEAT_ELEMENTS)
    set_context(x, what)
}

#' @export
deactivate.ggheatmap <- function(x) {
    set_context(x, NULL)
}

#' @return
#' - `active`: A `active` object which can be added into [ggheatmap][ggheat].
#' @export
#' @rdname activate
active <- function(what = NULL) {
    context <- match_context(what)
    structure(context %||% "heatmap", class = c("active", "ggheat"))
}

match_context <- function(what) {
    if (!is.null(what)) what <- match.arg(what, GGHEAT_ELEMENTS)
    what
}

GGHEAT_ELEMENTS <- c("top", "left", "bottom", "right")

#' @keywords internal
set_context <- function(x, context) UseMethod("set_context")

#' @export
set_context.ggheatmap <- function(x, context) {
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
get_context.ggheatmap <- function(x) slot(x, "active")

#' @export
get_context.annotations <- function(x) attr(x, "active")
