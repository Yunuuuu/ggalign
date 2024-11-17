#' Build data for the stack layout
#'
#' @param data Any objects to be plot with [ggstack()].
#' @inheritParams rlang::args_dots_used
#' @return A matrix or data.frame used by [ggstack()].
#' @export
fortify_stack <- function(data, ...) {
    rlang::check_dots_used()
    UseMethod("fortify_stack")
}

#' @export
fortify_stack.matrix <- function(data, ...) data

#' @export
fortify_stack.data.frame <- fortify_stack.matrix

#' @export
fortify_stack.numeric <- function(data, ...) as.matrix(data)

#' @export
fortify_stack.character <- fortify_stack.numeric

#' @export
fortify_stack.NULL <- function(data, ...) NULL

#' @export
fortify_stack.function <- function(data, ...) {
    cli_abort("Cannot use function in {.field stack} layout")
}

#' @export
fortify_stack.formula <- fortify_stack.function

#' @export
fortify_stack.default <- function(data, ...) {
    cli_abort(paste0(
        "{.arg data} must be a a numeric or character vector, a matrix, ",
        "a data frame, or an object coercible by {.fn fortify_stack}"
    ))
}
