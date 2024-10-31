#' Build a data frame
#'
#' This function converts various objects to a data frame. By default, it calls
#' [`fortify()`][ggplot2::fortify] to perform the conversion.
#'
#' @param data Any objects to be converted to a data frame.
#' @inheritParams rlang::args_dots_used
#' @return A data frame.
#' @seealso [`fortify`][ggplot2::fortify]
#' @export
fortify_data_frame <- function(data, ...) {
    rlang::check_dots_used()
    UseMethod("fortify_data_frame")
}

#' @importFrom vctrs new_data_frame vec_names
#' @export
fortify_data_frame.character <- function(data, ...) {
    ans <- list(name = vec_names(data), value = data)
    if (is.null(.subset2(ans, "name"))) ans$name <- NULL
    new_data_frame(ans)
}

#' @export
fortify_data_frame.numeric <- fortify_data_frame.character

#' @export
fortify_data_frame.default <- function(data, ...) {
    ggplot2::fortify(model = data, ...)
}

#' @export
fortify_data_frame.waiver <- function(data, ...) data

#' @export
fortify_data_frame.NULL <- function(data, ...) data
