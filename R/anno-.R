#' Create a heatmap annotation object
#'
#' @param Class Sub-class of `anno`.
#' @param ... Additional components of the sub-class.
#' @inheritParams ggheat
#' @param position A string of the annotation position, Possible values are
#' `"top"`, `"left"`, ` "bottom"`, and `"right"`. If `NULL`, the active context
#' of the [ggheatmap][ggheat] will be used.
#' @param size Annotation size, can be a [unit][grid::unit] object.
#' @param name A string of the annotation name.
#' @param set_context A logical value of length `2` indicates whether to set the
#' active context to the `position` for the [ggheatmap][ggheat] and whether to
#' set the active context to current annotation for the annotation list in
#' [ggheatmap][ggheat] when added.
#' @param order Annotation order, must be an single integer.
#' @return A new `Class` object.
#' @export
anno <- function(Class, ..., data = NULL,
                 position = NULL, size = NULL,
                 set_context = NULL, order = NULL, name = NULL) {
    data <- allow_lambda(data)
    position <- match_context(position)
    if (is.numeric(order)) {
        order <- as.integer(order)
    } else if (is.null(order)) {
        order <- NA_integer_
    }
    set_context <- set_context %||% TRUE
    methods::new(
        Class = Class,
        data = data,
        position = position,
        size = set_size(size),
        ...,
        set_context = rep_len(set_context, 2L),
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
        size = "ANY",
        name = "ANY",
        facetted_pos_scales = "ANY",
        position = "ANY",
        order = "integer",
        set_context = "logical"
    )
)

#' @export
#' @rdname anno
methods::setMethod("show", "anno", function(object) {
    print(sprintf("A %s object", fclass(object)))
    invisible(object)
})

#' @export
#' @keywords internal
plot.anno <- function(x, ...) {
    cli::cli_abort("You cannot plot {.cls {fclass(x)}} object")
}

#' @importFrom methods slot
#' @importFrom grid is.unit
#' @importFrom rlang is_string
methods::setValidity("anno", function(object) {
    if (length(slot(object, "set_context")) != 2L) {
        cli::cli_abort("@set_context must be of length 2")
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
