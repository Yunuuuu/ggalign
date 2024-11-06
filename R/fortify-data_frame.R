#' Build a data frame
#'
#' This function converts various objects to a data frame. By default, it calls
#' [`fortify()`][ggplot2::fortify] to perform the conversion.
#'
#' When `data` is a matrix, it will automatically be transformed into a
#' long-form data frame, where each row represents a unique combination of
#' matrix indices and their corresponding values.
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

#' @export
fortify_data_frame.default <- function(data, ...) {
    ggplot2::fortify(model = data, ...)
}

#' @export
fortify_data_frame.character <- function(data, ...) {
    ans <- list(.names = vec_names(data), value = data)
    if (is.null(.subset2(ans, ".names"))) ans$.names <- NULL
    new_data_frame(ans)
}

#' @export
fortify_data_frame.numeric <- fortify_data_frame.character

#' @export
fortify_data_frame.waiver <- function(data, ...) data

#' @export
fortify_data_frame.NULL <- function(data, ...) data

#' @export
fortify_data_frame.matrix <- function(data, ...) {
    row_nms <- vec_names(data)
    col_nms <- colnames(data)
    data <- new_data_frame(list(
        .row_index = vec_rep(seq_len(nrow(data)), ncol(data)),
        .column_index = vec_rep_each(seq_len(ncol(data)), nrow(data)),
        value = c(data)
    ))
    if (!is.null(row_nms)) data$.row_names <- row_nms[data$.row_index]
    if (!is.null(col_nms)) data$.column_names <- col_nms[data$.column_index]
    data
}
