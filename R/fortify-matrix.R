#' Build a Matrix
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function converts various objects into a matrix format.
#'
#' @param data An object to be converted into a matrix.
#' @param ... Additional arguments passed to methods.
#' @param data_arg The argument name for `data`. Developers can use it to
#' improve messages. Not used by the user.
#' @param call The execution environment where `data` and other arguments for
#' the method are collected, e.g., [`caller_env()`][rlang::caller_env()].
#' Developers can use it to improve messages. Not used by the user.
#' @return A matrix.
#' @seealso
#' - [`fortify_matrix.default()`]
#' - [`fortify_matrix.MAF()`]
#' - [`fortify_matrix.GISTIC()`]
#' - [`fortify_matrix.phylo()`]
#' @export
fortify_matrix <- function(data, ..., data_arg = caller_arg(data),
                           call = NULL) {
    UseMethod("fortify_matrix")
}

#' @inheritParams rlang::args_dots_empty
#' @inherit fortify_matrix title description return
#' @inheritParams fortify_matrix
#' @details
#' By default, it calls [`as.matrix()`] to build a matrix.
#' @family fortify_matrix methods
#' @importFrom rlang try_fetch
#' @export
fortify_matrix.default <- function(data, ..., data_arg = caller_arg(data),
                                   call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    try_fetch(
        as.matrix(data),
        error = function(cnd) {
            cli_abort(
                paste0(
                    "{.arg {data_arg}} must be a {.cls matrix}, ",
                    "or an object coercible by {.fn fortify_matrix}, or a valid ",
                    "{.cls matrix}-like object coercible by {.fn as.matrix}"
                ),
                call = call
            )
        }
    )
}

#' @export
fortify_matrix.matrix <- function(data, ..., data_arg = caller_arg(data),
                                  call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    data
}

#' @export
fortify_matrix.waiver <- fortify_matrix.matrix

#' @export
fortify_matrix.NULL <- fortify_matrix.matrix

#' @export
fortify_matrix.function <- fortify_matrix.matrix

#' @export
fortify_matrix.formula <- function(data, ..., data_arg = caller_arg(data),
                                   call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    rlang::as_function(data)
}
