#' Determine the context of subsequent manipulations
#' @param x A [ggheatmap][ggheat] or [ggannotation][gganno] object.
#' @param what What should get activated?
#' - `ggheatmap`: Possible values are "top", "left", "bottom", and "right".
#' - `ggannotation`: A string of name in the annotation.
#' @return
#' - `activate`: A object with the same class of `x`.
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

#' @return
#' - `active`: A string of current active context.
#' @export
#' @rdname activate
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

#' @return
#' - `active<-`: The same with `activate`, but won't check the arguments.
#' @export
#' @rdname activate
`active<-` <- function(x, value) UseMethod("active<-")

#' @export
`active<-.default` <- function(x, value) {
    cli::cli_abort("{.arg x} must be a {.cls ggheatmap} object")
}

#' @export
`active<-.ggheatmap` <- function(x, value) {
    slot(x, "active") <- value
    x
}

#' @export
`active<-.ggAnnotationList` <- function(x, value) {
    attr(x, "active") <- value
    x
}

GGHEAT_ELEMENTS <- c("top", "left", "bottom", "right")
