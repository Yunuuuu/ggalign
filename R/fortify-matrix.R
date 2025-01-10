#' Build a Matrix
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function converts various objects into a matrix format.
#'
#' @param data An object to be converted to a matrix.
#' @param ... Arguments passed to methods.
#' @return A matrix.
#' @seealso
#' - [`fortify_matrix.default()`]
#' - [`fortify_matrix.MAF()`]
#' - [`fortify_matrix.GISTIC()`]
#' - [`fortify_matrix.phylo()`]
#' @export
fortify_matrix <- function(data, ...) {
    UseMethod("fortify_matrix")
}

#' @inherit fortify_matrix title description return
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_matrix
#' @details
#' By default, it calls [`as.matrix()`] to build a matrix.
#' @family fortify_matrix methods
#' @importFrom rlang try_fetch
#' @export
fortify_matrix.default <- function(data, ...) {
    rlang::check_dots_empty()
    try_fetch(
        as.matrix(data),
        error = function(cnd) {
            cli_abort(paste0(
                "{.arg data} must be a {.cls matrix}, ",
                "or an object coercible by {.fn fortify_matrix}, or a valid ",
                "{.cls matrix}-like object coercible by {.fn as.matrix}"
            ), parent = cnd)
        }
    )
}

#' @export
fortify_matrix.matrix <- function(data, ...) {
    rlang::check_dots_empty()
    data
}

#' @export
fortify_matrix.waiver <- fortify_matrix.matrix

#' @export
fortify_matrix.NULL <- fortify_matrix.matrix

#' @export
fortify_matrix.function <- fortify_matrix.matrix

#' @export
fortify_matrix.formula <- function(data, ...) {
    rlang::check_dots_empty()
    rlang::as_function(data)
}
