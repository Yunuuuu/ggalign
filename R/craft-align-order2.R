#' Reorders layout observations based on specific statistics.
#'
#' @details
#' `r lifecycle::badge('experimental')`
#'
#' The `align_order2()` function differs from `align_order()` in that the
#' `weights` argument in `align_order()` must return atomic weights for each
#' observation. In contrast, the `stat` argument in `align_order2()` can
#' return more complex structures, such as [hclust][stats::hclust] or
#' [dendrogram][stats::as.dendrogram], among others.
#'
#' Typically, you can achieve the functionality of `align_order2()` using
#' `align_order()` by manually extracting the ordering information from
#' the statistic.
#'
#' @param stat A statistical function which accepts a data and returns the
#' statistic, which we'll call [`order2()`] to extract the ordering information.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' function provided in `stat` argument.
#' @param data A `matrix`, `data frame`, or atomic vector used as the input for
#' the `stat` function. Alternatively, you can specify a `function` (including
#' purrr-like lambda syntax) that will be applied to the layout matrix,
#' transforming it as necessary for statistic calculations. By default, it will
#' inherit from the layout matrix.
#' @inheritParams align_order
#' @inheritSection align Discrete Axis Alignment
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_left() +
#'     align_order2(hclust2)
#' @seealso [order2()]
#' @importFrom ggplot2 waiver
#' @importFrom rlang list2
#' @export
align_order2 <- function(stat, ..., reverse = FALSE,
                         strict = TRUE, data = NULL,
                         active = NULL) {
    stat <- rlang::as_function(stat)
    assert_bool(strict)
    assert_bool(reverse)
    assert_active(active)
    active <- active_update(active(use = FALSE), active)
    align(
        align = AlignOrder2,
        stat = stat,
        params = list2(...),
        reverse = reverse,
        strict = strict,
        active = active,
        data = data
    )
}

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject
AlignOrder2 <- ggproto("AlignOrder2", CraftAlign,
    interact_layout = function(self, layout) {
        layout <- ggproto_parent(CraftAlign, self)$interact_layout(layout)
        layout_data <- layout@data
        if (is.null(input_data <- self$input_data) ||
            is.waive(input_data)) { # inherit from the layout
            if (is.null(data <- layout_data)) {
                cli_abort(c(
                    sprintf(
                        "you must provide {.arg data} in %s",
                        object_name(self)
                    ),
                    i = sprintf("no data was found in %s", self$layout_name)
                ))
            }
        } else if (is.function(input_data)) {
            if (is.null(layout_data)) {
                cli_abort(c(
                    sprintf(
                        "{.arg data} in %s cannot be a function",
                        object_name(self)
                    ),
                    i = sprintf("no data was found in %s", self$layout_name)
                ))
            }
            data <- input_data(layout_data)
        } else {
            data <- input_data
        }

        design <- layout@design
        layout_nobs <- .subset2(design, "nobs")

        # we always regard rows as the observations
        if (is.null(layout_nobs)) {
            layout_nobs <- vec_size(data)
            if (layout_nobs == 0L) {
                cli_abort("{.arg data} cannot be empty", call = self$call)
            }
            design["nobs"] <- list(layout_nobs)
            layout@design <- design
        } else if (vec_size(data) != layout_nobs) {
            cli_abort(sprintf(
                "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
                object_name, vec_size(data), layout_name, layout_nobs
            ))
        }

        # save the labels
        self$labels <- vec_names(data) %||% vec_names(layout_data)
        self$data <- ggalign_data_restore(data, layout_data)
        layout
    },
    compute = function(self, panel, index) {
        inject(self$stat(self$data, !!!self$params))
    },
    align = function(self, panel, index) {
        index <- vec_cast(
            order2(self$statistics), integer(),
            x_arg = "stat", call = self$call
        )
        assert_mismatch_nobs(
            self, vec_size(self$data), vec_size(index),
            arg = "stat"
        )
        if (self$reverse) index <- rev(index)
        assert_reorder(self, panel, index, self$strict)
        list(panel, index)
    },
    summary_align = function(self) c(TRUE, FALSE)
)

#' Ordering Permutation
#'
#' `order2` returns a permutation which rearranges its first argument into
#' ascending order.
#' @param x Any objects can be extracting ordering.
#' @return An integer vector unless any of the inputs has `2^31` or more
#' elements, when it is a double vector.
#' @examples
#' order2(hclust2(matrix(rnorm(100L), nrow = 10L)))
#' @export
order2 <- function(x) UseMethod("order2")

#' @export
#' @rdname order2
order2.hclust <- function(x) x$order

#' @importFrom stats order.dendrogram
#' @export
#' @rdname order2
order2.dendrogram <- function(x) order.dendrogram(x)

#' @export
#' @rdname order2
order2.ser_permutation_vector <- function(x) {
    rlang::check_installed(
        "seriation", "to extract order from `ser_permutation_vector`"
    )
    getExportedValue("seriation", "get_order")(x)
}

#' @export
#' @rdname order2
order2.ser_permutation <- function(x) {
    rlang::check_installed(
        "seriation", "to extract order from `ser_permutation`"
    )
    getExportedValue("seriation", "get_order")(x)
}

#' @export
#' @rdname order2
order2.phylo <- function(x) {
    second <- x$edge[, 2L, drop = TRUE]
    second[second <= length(x$tip.label)]
}
