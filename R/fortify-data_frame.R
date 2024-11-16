#' Build a data frame
#'
#' This function converts various objects to a data frame.
#'
#' @param data An object to be converted to a data frame.
#' @inheritParams rlang::args_dots_used
#' @return A data frame.
#' @seealso
#' - [`fortify_data_frame.default()`]
#' - [`fortify_data_frame.character()`]
#' - [`fortify_data_frame.numeric()`]
#' - [`fortify_data_frame.matrix()`]
#' @export
fortify_data_frame <- function(data, ...) {
    rlang::check_dots_used()
    UseMethod("fortify_data_frame")
}

#' @inherit fortify_data_frame
#' @description
#' By default, it calls [`fortify()`][ggplot2::fortify] to build the
#' data frame.
#' @family fortify_data_frame methods
#' @export
fortify_data_frame.default <- function(data, ...) {
    ggplot2::fortify(model = data, ...)
}

#' @inherit fortify_data_frame.default
#' @description
#' When `data` is an atomic vector, it'll be converted to a data frame with
#' following columns:
#'
#'  - `.names`: the names for the vector (only applicable if names exist).
#'  - `value`: the actual value of the vector.
#'
#' @family fortify_data_frame methods
#' @export
fortify_data_frame.character <- function(data, ...) {
    ans <- list(.names = vec_names(data), value = data)
    if (is.null(.subset2(ans, ".names"))) ans$.names <- NULL
    new_data_frame(ans)
}

#' @inherit fortify_data_frame.character
#' @family fortify_data_frame methods
#' @export
fortify_data_frame.numeric <- fortify_data_frame.character

#' @inherit fortify_data_frame.character
#' @family fortify_data_frame methods
#' @export
fortify_data_frame.logical <- fortify_data_frame.character

#' @inherit fortify_data_frame.character
#' @family fortify_data_frame methods
#' @export
fortify_data_frame.complex <- fortify_data_frame.character

#' @export
fortify_data_frame.waiver <- function(data, ...) data

#' @export
fortify_data_frame.NULL <- function(data, ...) data

#' @inherit fortify_data_frame.default
#' @description
#' When `data` is a matrix, it will automatically be transformed into a
#' long-form data frame, where each row represents a unique combination of
#' matrix indices and their corresponding values.
#' @family fortify_data_frame methods
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
