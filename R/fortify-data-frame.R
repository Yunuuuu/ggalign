#' Build a data frame
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function converts various objects to a data frame.
#'
#' @param data An object to be converted to a data frame.
#' @param ... Arguments passed to methods.
#' @inheritParams fortify_matrix
#' @return A data frame.
#' @eval
#' rd_collect_family("fortify_data_frame",
#'     "`fortify_data_frame` method collections"
#' )
#' @export
fortify_data_frame <- function(data, ..., data_arg = NULL, call = NULL) {
    UseMethod("fortify_data_frame")
}

#' @inherit fortify_data_frame title description
#' @param ... Additional arguments passed to [`fortify()`][ggplot2::fortify].
#' @inheritParams fortify_data_frame
#' @details
#' By default, it calls [`fortify()`][ggplot2::fortify] to build the
#' data frame.
#' @family fortify_data_frame
#' @export
fortify_data_frame.default <- function(data, ..., data_arg = NULL,
                                       call = NULL) {
    ggplot2::fortify(model = data, ...)
}

#' @inherit fortify_data_frame.default title description
#' @param data An object to be converted to a data frame.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_data_frame
#' @return A data frame with following columns:
#'
#'  - `.names`: the names for the vector (only applicable if names exist).
#'  - `value`: the actual value of the vector.
#'
#' @family fortify_data_frame
#' @export
fortify_data_frame.character <- function(data, ..., data_arg = NULL,
                                         call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    ans <- list(.names = vec_names(data), value = data)
    if (is.null(.subset2(ans, ".names"))) ans$.names <- NULL
    new_data_frame(ans)
}

#' @rdname fortify_data_frame.character
#' @export
fortify_data_frame.numeric <- fortify_data_frame.character

#' @rdname fortify_data_frame.character
#' @export
fortify_data_frame.logical <- fortify_data_frame.character

#' @rdname fortify_data_frame.character
#' @export
fortify_data_frame.complex <- fortify_data_frame.character

#' @export
fortify_data_frame.waiver <- function(data, ..., data_arg = NULL,
                                      call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    data
}

#' @export
fortify_data_frame.NULL <- fortify_data_frame.waiver

#' @inherit fortify_data_frame.default title description
#' @param data A matrix-like object.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_data_frame
#' @return
#' Matrix will be transformed into a long-form data frame, where each row
#' represents a unique combination of matrix indices and their corresponding
#' values. The resulting data frame will contain the following columns:
#'
#'  - `.row_names` and `.row_index`: the row names (only applicable when names
#'  exist) and an integer representing the row index of the original matrix.
#'
#'  - `.column_names` and `.column_index`: the column names (only applicable
#'  when names exist) and column index of the original matrix.
#'
#'  - `value`: the actual value.
#'
#' @family fortify_data_frame
#' @export
fortify_data_frame.matrix <- function(data, ..., data_arg = NULL,
                                      call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
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

#' @export
#' @rdname fortify_data_frame.matrix
fortify_data_frame.DelayedMatrix <- function(data, ...) {
    fortify_data_frame(as.matrix(data), ...)
}

#' @export
#' @rdname fortify_data_frame.matrix
fortify_data_frame.Matrix <- function(data, ...) {
    fortify_data_frame(as.matrix(data), ...)
}
