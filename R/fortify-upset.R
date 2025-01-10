#' Build a Matrix for UpSet plot
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' This function converts various objects into a matrix format suitable for
#' creating an UpSet plot. It always returns a matrix for a `horizontal` UpSet
#' plot.
#'
#' @param data An object to be converted to a matrix for UpSet  plot.
#' @param mode A string of `r oxford_or(c("distinct", "intersect", "union"))`
#' indicates the mode to define the set intersections. Check
#' <https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html#upset-mode>
#' for details.
#' @inheritParams rlang::args_dots_used
#' @return A matrix.
#' @seealso
#' - [`fortify_upset.list()`]
#' - [`fortify_upset.matrix()`]
#' @export
fortify_upset <- function(data, mode = "distinct", ...) {
    mode <- arg_match0(mode, c("distinct", "intersect", "union"))
    UseMethod("fortify_upset")
}

#' @inherit fortify_upset
#' @param data A list of sets.
#' @param ... Not used.
#' @section ggalign attributes:
#'  - `intersection_sizes`: An integer vector indicating the size of each
#'    intersection.
#'  - `set_sizes`: An integer vector indicating the size of each set.
#' @family fortify_upset methods
#' @export
fortify_upset.list <- function(data, mode = "distinct", ...) {
    rlang::check_dots_empty()
    data <- lapply(data, function(x) {
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

#' @inherit fortify_upset.list
#' @param data A matrix where each row represents an element, and each column
#' defines a set. The values in the matrix indicate whether the element is
#' part of the set. Any non-missing value signifies that the element exists in
#' the set.
#' @family fortify_upset methods
#' @export
fortify_upset.matrix <- function(data, mode = "distinct", ...) {
    data <- !is.na(data)
    elements <- vec_seq_along(data)
    fortify_upset.list(
        lapply(seq_len(ncol(data)), function(i) {
            .subset(elements, data[, i, drop = TRUE])
        }),
        mode = mode,
        ...
    )
}
