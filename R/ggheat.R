#' Heatmap with ggplot2
#'
#' @param data A matrix, if it is a simple vector, it will be converted to a
#' one-column matrix. Data.frame will also be coerced into matrix.
#' @param mapping Default list of aesthetic mappings to use for plot.
#' @param ... Additional arguments.
#' @importFrom ggplot2 aes
#' @export
ggheat <- function(data, mapping = aes(), ...) UseMethod("ggheat")

#' @export
ggheat.matrix <- function(data, mapping = aes(), ...) {
    methods::new("ggheatmap",
        matrix = data,
        params = rlang::list2(...),
        heatmap = ggplot2::ggplot(mapping = mapping),
        active = NULL
    )
}

#' @export
ggheat.data.frame <- function(data, mapping = aes(), ...) {
    data <- as.matrix(data)
    ggheat(data = data, mapping = mapping, ...)
}

#' @export
ggheat.atomic <- function(data, mapping = aes(), ...) {
    data <- matrix(matrix, ncol = 1L)
    colnames(data) <- "V1"
    if (rlang::is_named(matrix)) rownames(data) <- names(matrix)
    ggheat(data = data, mapping = mapping, ...)
}

#' @export
ggheat.default <- function(data, mapping = aes(), ...) {
    data <- as.matrix(data)
    ggheat(data = data, mapping = mapping, ...)
}

#' @export
ggheat.NULL <- function(data, mapping = aes(), ...) {
    cli::cli_abort("{.arg data} must be a matrix-like object instead of `NULL`")
}

#' @keywords internal
methods::setClass("ggheat", list(active = "ANY"))

# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#' @keywords internal
methods::setClass(
    "ggheatmap",
    contains = "ggheat",
    list(
        matrix = "matrix",
        params = "list",
        row_slice = "ANY",
        row_order = "ANY",
        column_slice = "ANY",
        column_order = "ANY",
        heatmap = "ANY",
        top = "ANY", left = "ANY",
        bottom = "ANY", right = "ANY"
    ),
    prototype = list(
        row_slice = NULL,
        column_slice = NULL,
        top = NULL, left = NULL,
        bottom = NULL, right = NULL
    )
)

#' @importFrom ggplot2 is.ggplot
#' @importFrom rlang is_string
methods::setValidity("ggheatmap", function(object) {
    if (!is.ggplot(slot(object, "heatmap"))) {
        cli::cli_abort("@heatmap must be a {.cls ggplot} object")
    }
    active <- slot(object, "active")
    if (!is.null(active) &&
        (!is_string(active) || !any(active == GGHEAT_ELEMENTS))) {
        cli::cli_abort(sprintf(
            "@active must be a string of %s",
            oxford_comma(GGHEAT_ELEMENTS, final = "or")
        ))
    }
    TRUE
})

#' @importFrom methods show
#' @export
methods::setMethod("show", "ggheatmap", function(object) print(object))

#' Reports whether x is a ggheatmap object
#'
#' @param x An object to test
#' @export
is.ggheatmap <- function(x) inherits(x, "ggheatmap")
