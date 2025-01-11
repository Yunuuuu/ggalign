#' Change the shape of the input object
#'
#' @param data An R object.
#' @param shape Usually `NULL` or a string, specifying the new shape for the
#' object. Refer to the detailed method for allowed values.
#' @details
#' In most cases, [`fortify_matrix()`] or [`fortify_data_frame()`] provide full
#' support for transforming objects. However, some objects may require two
#' completely different approaches to be fortified. The `tune` function acts as
#' a helper to create a new class tailored for these objects.
#' @seealso
#' - [`tune.MAF()`]
#' @export
tune <- function(data, shape = NULL) UseMethod("tune")

#' @inherit tune title
#' @description
#' - `new_tune`: Creates a new object by wrapping it in a scalar list with
#'   the specified attributes and class.
#' - `tune_data`: Retrieves the original input data.
#'
#' @param x An R object.
#' @param ... Additional attributes passed to [`structure()`].
#' @param class A character vector specifying the class name to be added.
#' @export
new_tune <- function(x, ..., class = character()) {
    structure(list(x), ..., class = c(class, "ggalign_tune"))
}

#' @export
#' @rdname new_tune
tune_data <- function(x) .subset2(x, 1L)

#' @export
print.ggalign_tune <- function(x, ...) {
    print(tune_data(x))
    invisible(x)
}

#' @export
fortify_matrix.ggalign_tune <- function(data, ...) {
    cli_abort("No {.fn fortify_matrix} method for {.obj_type_friendly {data}}")
}

#' @export
fortify_data_frame.ggalign_tune <- function(data, ...) {
    cli_abort(
        "No {.fn fortify_data_frame} method for {.obj_type_friendly {data}}"
    )
}
