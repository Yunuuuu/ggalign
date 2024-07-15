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
ggheat.matrix <- function(data, mapping = aes(),
                          width = NULL, height = NULL, ...) {
    methods::new("ggheatmap",
        matrix = data,
        params = rlang::list2(
            width = set_size(width),
            height = set_size(height)
        ),
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
methods::setClass("ggheat")

# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#' @keywords internal
methods::setClass(
    "ggheatmap",
    contains = "ggheat",
    list(
        matrix = "matrix",
        params = "list",
        row_panels = "ANY",
        row_index = "ANY",
        column_panels = "ANY",
        column_index = "ANY",
        heatmap = "ANY", active = "ANY",
        facetted_pos_scales = "ANY",
        top = "ANY", left = "ANY",
        bottom = "ANY", right = "ANY"
    ),
    prototype = list(
        row_index = NULL,
        row_panels = NULL,
        column_panels = NULL,
        column_index = NULL,
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

#' @param object A `ggheatmap` object.
#' @importFrom methods show
#' @export
#' @rdname ggheat
methods::setMethod("show", "ggheatmap", function(object) print(object))

#' Reports whether x is a ggheatmap object
#'
#' @param x An object to test
#' @export
is.ggheatmap <- function(x) methods::is(x, "ggheatmap")
