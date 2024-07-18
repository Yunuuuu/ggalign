#' Create a heatmap annotation object
#'
#' @param Class Sub-class of `anno`.
#' @param ... Additional components of the sub-class.
#' @param data A matrix, a data frame, or even a simple vector that will be
#' converted into a one-column matrix. If the `data` argument is set to `NULL`,
#' the function will use the heatmap matrix. Additionally, the `data` argument
#' can also accept a function (purrr-like lambda is also okay), which will be
#' applied with the heatmap matrix.
#'
#' It is important to note that all annotations consider the `rows` as the
#' observations. It means the `NROW` function must return the same number as the
#' heatmap parallel axis. So for column annotation, the heatmap matrix will be
#' transposed before using (If data is a `function`, it will be applied with the
#' transposed matrix).
#' @param position A string of the annotation position, possible values are
#' `"top"`, `"left"`, `"bottom"`, and `"right"`. If `NULL`, the active context
#' of the `ggheatmap` will be used.
#' @param size Annotation size, can be a [unit][grid::unit] object.
#' @param labels Labels for axis parallelly with heatmap, the default will use
#' the rownames of the `data`. One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels
#'   - A character vector giving labels (must be same length as the heatmap
#'     axis)
#'   - An expression vector (must be the same length as heatmap axis). See
#'     `?plotmath` for details.
#'   - A function that takes the default labels as the input and returns labels
#'     as output. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param labels_nudge A single numeric or a numeric value of length
#' `nrow(data)`, to nudge each text label away from the center. If `waiver()`,
#' will inherit from the heatmap. If `NULL`, no breaks, in this way labels will
#' be removed too.
#' @param set_context A logical value of length `2` indicates whether to set the
#' active context to the `position` for the [ggheatmap][ggheat] and whether to
#' set the active context to current annotation for the annotation list in
#' [ggheatmap][ggheat] when added.
#' @param order Annotation order, must be an single integer.
#' @param name A string of the annotation name.
#' @param call The `call` used to construct the scale for reporting messages.
#' @return A new `Class` object.
#' @importFrom rlang caller_call current_call
#' @export
anno <- function(Class, ..., data = NULL,
                 position = NULL, size = NULL,
                 labels = NULL, labels_nudge = NULL,
                 set_context = NULL, order = NULL, name = NULL,
                 call = caller_call()) {
    data <- allow_lambda(data)
    position <- match_context(position)
    if (is.numeric(order)) {
        order <- as.integer(order)
    } else if (is.null(order)) {
        order <- NA_integer_
    }
    set_context <- set_context %||% TRUE
    call <- call %||% current_call()
    methods::new(
        Class = Class,
        data = data,
        position = position,
        size = set_size(size),
        labels = labels, labels_nudge = labels_nudge,
        ...,
        set_context = rep_len(set_context, 2L),
        name = name,
        order = order,
        call = call
    )
}

#' @importFrom utils packageName
anno_override_call <- function(call = NULL) {
    if (is.null(call) || is.function(call[[1]])) {
        return(TRUE)
    }
    !identical(
        packageName(environment(eval(.subset2(call, 1L)))),
        pkg_nm()
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
        labels = "ANY",
        labels_nudge = "ANY",
        position = "ANY",
        order = "integer",
        set_context = "logical",
        call = "ANY"
    )
)

#' Show `anno` object
#' @param object A `anno` object.
#' @importFrom methods show
#' @export
#' @keywords internal
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
    call <- slot(object, "call")
    if (length(slot(object, "set_context")) != 2L) {
        cli::cli_abort("{.arg set_context} must be of length 2", call = call)
    }
    name <- slot(object, "name")
    if (!is.null(name) && (!is_string(name) || name == "")) {
        cli::cli_abort(
            "{.arg name} must be a single non-empty string",
            call = call
        )
    }
    position <- slot(object, "position")
    if (!is.null(position) &&
        (!is_string(position) || !any(position == GGHEAT_ELEMENTS))) {
        cli::cli_abort(
            sprintf(
                "{.arg position} must be a string of %s",
                oxford_comma(GGHEAT_ELEMENTS, final = "or")
            ),
            call = call
        )
    }
    size <- slot(object, "size")
    if (!is_scalar(size) || !is.unit(size)) {
        cli::cli_abort("{.arg size} must be a single {.cls unit} object.",
            call = call
        )
    }
    TRUE
})

anno_default_theme <- function() {
    ggplot2::theme(strip.text = ggplot2::element_blank())
}
