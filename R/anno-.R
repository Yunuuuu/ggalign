#' Create a anno object.
#'
#' @param Class Sub-class of `anno`.
#' @param ... Additional components of the sub-class.
#' @inheritParams ggheat
#' @param position A string of the annotation position, Possible values are
#' `"top"`, `"left"`, ` "bottom"`, and `"right"`.
#' @param size Annotation size, must be a [unit][grid::unit] object.
#' @param name A string of the annotation name.
#' @param active A logical value of length `1` or `2`.
#' @param order Annotation order, must be an single integer.
#' @return A new `Class` object.
#' @export
new_anno <- function(Class, ..., data = NULL,
                     position = NULL, size = unit(10, "mm"),
                     active = NULL, name = NULL, order = NULL) {
    data <- allow_lambda(data)
    if (!is.null(position)) position <- match.arg(position, GGHEAT_ELEMENTS)
    if (is.na(size) || is.null(size)) size <- unit(1, "null")
    if (!is.unit(size)) size <- unit(size, "null")
    if (is.numeric(order)) {
        order <- as.integer(order)
    } else if (is.null(order)) {
        order <- NA_integer_
    }
    methods::new(
        Class = Class,
        data = data,
        position = position,
        size = size,
        ...,
        active = active %||% TRUE,
        name = name,
        order = order
    )
}

#' @include ggheat.R
#' @keywords internal
methods::setClass(
    "anno",
    contains = "ggheat",
    list(
        data = "ANY",
        facetted_pos_scales = "ANY",
        order = "integer",
        size = "ANY",
        name = "ANY",
        position = "ANY",
        active = "logical"
    )
)

#' @importFrom grid is.unit
#' @importFrom rlang is_string is_bool
methods::setValidity("anno", function(object) {
    active <- slot(object, "active")
    if (length(active) != 1L && length(active) != 2L) {
        cli::cli_abort("@active must be of length 1 or 2")
    }
    name <- slot(object, "name")
    if (!is.null(name) && (!is_string(name) || name == "")) {
        cli::cli_abort("@name must be a single non-empty string")
    }
    position <- slot(object, "position")
    if (!is.null(position) &&
        (!is_string(position) || !any(position == GGHEAT_ELEMENTS))) {
        cli::cli_abort(sprintf(
            "@position must be a string of %s",
            oxford_comma(GGHEAT_ELEMENTS, final = "or")
        ))
    }
    size <- slot(object, "size")
    if (!is_scalar(size)) {
        cli::cli_abort("{@size} must be a single {.cls unit} object.")
    } else if (!is.unit(size)) {
        cli::cli_abort("{@size} must be a single {.cls unit} object.")
    }
    TRUE
})
