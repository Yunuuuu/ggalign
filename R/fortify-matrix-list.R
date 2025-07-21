#' Convert the shape of a list for fortify method
#'
#' @param data A list
#' @param shape Not used currently.
#' @seealso [`fortify_matrix.list_upset()`]
#' @family tune
#' @export
tune.list <- function(data, shape = NULL) {
    if (!is.null(shape)) {
        cli_abort("{.arg shape} cannot be used currently for {.cls list} object")
    }
    new_tune(data, class = "list_upset")
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
#' @family fortify_matrix
#' @aliases fortify_matrix.list
#' @export
fortify_matrix.list_upset <- function(data, mode = "distinct", ...,
                                      data_arg = NULL, call = NULL) {
    call <- call %||% current_call()
    rlang::check_dots_empty(call = call)
    mode <- arg_match0(mode, c("distinct", "intersect", "union"),
        error_call = call
    )
    data <- lapply(tune_data(data), function(x) {
        vec_unique(vec_slice(x, !vec_detect_missing(x)))
    })
    data <- list_drop_empty(data)
    if (length(data) == 0L) {
        cli::cli_abort(
            "No valid data: All input lists are either empty or contain only missing values.",
            call = call
        )
    }

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
