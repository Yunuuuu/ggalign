#' Activate the context of subsequent manipulations
#' @param x A [ggheatmap][ggheat] object.
#' @param what What should get activated? Possible values are `"top"`, `"left"`,
#'` "bottom"`, and `"right"`.
#' @return A object with the same class of `x`, whose active context will be set
#' or unset.
#' @export
activate <- function(x, what) UseMethod("activate")

#' @export
#' @rdname activate
deactivate <- function(x) UseMethod("deactivate")

#' @export
#' @rdname activate
activate.ggheatmap <- function(x, what) {
    what <- match.arg(what, GGHEAT_ELEMENTS)
    active(x) <- what
    x
}

#' @export
#' @rdname activate
deactivate.ggheatmap <- function(x) {
    active(x) <- NULL
    x
}

#' Determine the context of subsequent manipulations
#' @param x A [ggheatmap][ggheat] or `ggAnnotationList`object.
#' @return
#' - `active`: A string of current active context.
#' @export
active <- function(x) UseMethod("active")

#' @export
active.default <- function(x) {
    cli::cli_abort(paste(
        "{.arg x} must be a {.cls ggheatmap}",
        "or a {.cls ggAnnotationList} object"
    ))
}

#' @export
active.ggheatmap <- function(x) slot(x, "active")

#' @export
active.ggAnnotationList <- function(x) attr(x, "active")

#' @param value What should get activated? Possible values are `"top"`,
#' `"left"`, `"bottom"`, and `"right"`.
#' @return
#' - `active<-`: The same with [activate], but won't check the arguments.
#' @export
#' @rdname active
`active<-` <- function(x, value) UseMethod("active<-")

#' @export
`active<-.default` <- function(x, value) {
    cli::cli_abort("{.arg x} must be a {.cls ggheatmap} object")
}

#' @export
#' @rdname active
`active<-.ggheatmap` <- function(x, value) {
    slot(x, "active") <- value
    x
}

#' @export
#' @rdname active
`active<-.ggAnnotationList` <- function(x, value) {
    attr(x, "active") <- value
    x
}

GGHEAT_ELEMENTS <- c("top", "left", "bottom", "right")
