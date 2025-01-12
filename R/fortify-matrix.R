#' Build a Matrix
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function converts various objects into a matrix format. By default, it
#' calls [`as.matrix()`] to build a matrix.
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
#' - [`fortify_matrix.list_upset()`]
#' - [`fortify_matrix.matrix_upset()`]
#' @export
fortify_matrix <- function(data, ..., data_arg = caller_arg(data),
                           call = NULL) {
    UseMethod("fortify_matrix")
}

#' @inheritParams rlang::args_dots_empty
#' @inherit fortify_matrix title return
#' @description
#' By default, it calls [`as.matrix()`] to build a matrix.
#' @inheritParams fortify_matrix
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
fortify_matrix.waiver <- function(data, ..., data_arg = caller_arg(data),
                                  call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    data
}

#' @export
fortify_matrix.NULL <- fortify_matrix.waiver

#' @export
fortify_matrix.function <- fortify_matrix.waiver

#' @export
fortify_matrix.formula <- function(data, ..., data_arg = caller_arg(data),
                                   call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    rlang::as_function(data)
}

#' Build a matrix from a matrix
#' @param data A matrix object.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams fortify_matrix
#' @section shape:
#'  - `upset`: [`fortify_matrix.matrix_upset`]
#' @family fortify_matrix methods
#' @export
fortify_matrix.matrix <- fortify_matrix.waiver

#' @inherit fortify_matrix.list_upset title
#' @description
#' Converts a matrix suitable for creating an UpSet plot. [`tune.matrix()`]
#' helps convert `matrix` object to a `matrix_upset` object.
#' @param data A matrix where each row represents an element, and each column
#' defines a set. The values in the matrix indicate whether the element is part
#' of the set. Any non-missing value signifies that the element exists in the
#' set.
#' @inheritParams fortify_matrix.list_upset
#' @inheritDotParams fortify_matrix.list_upset
#' @inheritSection fortify_matrix.list_upset ggalign attributes
#' @seealso [`tune.matrix()`]
#' @family fortify_matrix methods
#' @export
fortify_matrix.matrix_upset <- function(data, ..., data_arg = caller_arg(data),
                                        call = NULL) {
    call <- call %||% current_call()
    data <- !is.na(tune_data(data))
    elements <- vec_seq_along(data)
    fortify_matrix.list_upset(
        lapply(seq_len(ncol(data)), function(i) {
            .subset(elements, data[, i, drop = TRUE])
        }),
        ...,
        data_arg = data_arg,
        call = call
    )
}

#' Convert the shape of a matrix for fortify method
#'
#' @param data A matrix.
#' @param shape Not used currently.
#' @seealso
#' - [`fortify_matrix.matrix()`]
#' - [`fortify_matrix.matrix_upset()`]
#' @family tune methods
#' @export
tune.matrix <- function(data, shape = NULL) {
    if (!is.null(shape)) {
        cli_abort("{.arg shape} cannot be used currently for {.cls matrix} object")
    }
    new_tune(data, class = "matrix_upset")
}

#' Build a Matrix for UpSet plot
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function converts a list into a matrix format suitable for creating an
#' UpSet plot. It always returns a matrix for a `horizontal` UpSet plot.
#' @inheritParams rlang::args_dots_empty
#' @param data A list of sets.
#' @param mode A string of `r oxford_or(c("distinct", "intersect", "union"))`
#' indicates the mode to define the set intersections. Check
#' <https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html#upset-mode>
#' for details.
#' @inheritParams fortify_matrix
#' @section ggalign attributes:
#'  - `intersection_sizes`: An integer vector indicating the size of each
#'    intersection.
#'  - `set_sizes`: An integer vector indicating the size of each set.
#' @seealso [`tune.list()`]
#' @family fortify_matrix methods
#' @aliases fortify_matrix.list
#' @export
fortify_matrix.list_upset <- function(data, mode = "distinct", ...,
                                      data_arg = caller_arg(data),
                                      call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    mode <- arg_match0(mode, c("distinct", "intersect", "union"),
        error_call = call
    )
    data <- lapply(tune_data(data), function(x) {
        vec_unique(vec_slice(x, !vec_detect_missing(x)))
    })
    data <- list_drop_empty(data)

    # Based on the explanation from
    # https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html
    action <- switch(mode,
        distinct = function(data, intersection) {
            out <- NULL
            for (i in which(intersection)) {
                if (is.null(out)) {
                    out <- .subset2(data, i)
                } else if (vec_size(out) == 0L) { # early exit for empty items
                    return(out)
                } else {
                    out <- vec_set_intersect(out, .subset2(data, i))
                }
            }
            for (i in which(!intersection)) {
                if (vec_size(out) == 0L) { # early exit for empty items
                    return(out)
                }
                out <- vec_set_difference(out, .subset2(data, i))
            }
            return(out)
        },
        intersect = function(data, intersection) {
            out <- NULL
            for (i in which(intersection)) {
                if (is.null(out)) {
                    out <- .subset2(data, i)
                } else if (vec_size(out) == 0L) { # early exit for empty items
                    return(out)
                } else {
                    out <- vec_set_intersect(out, .subset2(data, i))
                }
            }
            out
        },
        union = function(data, intersection) {
            Reduce(vec_set_union, .subset(data, intersection))
        }
    )

    intersection <- logical(vec_size(data)) # template
    intersection_and_size <- lapply(
        seq_len(vec_size(intersection)),
        function(n) {
            # generate all possible intersections
            utils::combn(vec_size(intersection), n, function(index) {
                intersection[index] <- TRUE
                list(
                    intersection = intersection,
                    # for each intersection, we define the size
                    size = vec_size(action(data, intersection))
                )
            }, simplify = FALSE)
        }
    )

    # https://en.wikipedia.org/wiki/UpSet_plot
    # UpSets can be used horizontally and vertically.
    # In a vertical UpSet plot, the columns of the matrix correspond to the
    # sets, the rows correspond to the intersections.
    # we by default use `horizontal` upset, the rows of the matrix correspond
    # to the sets, the columns correspond to the intersections.
    ans <- list_transpose(unlist(intersection_and_size, FALSE, FALSE))
    intersections <- inject(cbind(!!!.subset2(ans, "intersection")))
    rownames(intersections) <- names(data)
    intersection_sizes <- unlist(.subset2(ans, "size"), FALSE, FALSE)
    keep <- intersection_sizes > 0L # remove intersection without items
    intersections <- intersections[, keep, drop = FALSE]
    intersection_sizes <- intersection_sizes[keep]
    ggalign_data_set(intersections,
        intersection_sizes = intersection_sizes,
        set_sizes = list_sizes(data),
        upset_mode = mode
    )
}

#' Convert the shape of a list for fortify method
#'
#' @param data A list
#' @param shape Not used currently.
#' @seealso [`fortify_matrix.list_upset()`]
#' @family tune methods
#' @export
tune.list <- function(data, shape = NULL) {
    if (!is.null(shape)) {
        cli_abort("{.arg shape} cannot be used currently for {.cls list} object")
    }
    new_tune(data, class = "list_upset")
}
