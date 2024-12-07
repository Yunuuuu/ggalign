#' Reorders layout observations based on specific statistics.
#'
#' @details
#' `r lifecycle::badge('experimental')`
#'
#' The `align_reorder()` function differs from `align_order()` in that the `wts`
#' argument in `align_order()` must return atomic weights for each observation.
#' In contrast, the `stat` argument in `align_reorder()` can return more complex
#' structures, such as [hclust][stats::hclust] or
#' [dendrogram][stats::as.dendrogram], among others.
#'
#' Typically, you can achieve the functionality of `align_reorder()` using
#' `align_order()` by manually extracting the ordering information from
#' the statistic.
#'
#' @param stat A summary function which accepts a data and returns the
#' statistic, which we'll call [`order2()`] to extract the ordering information.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' function provided in `stat` argument.
#' @param data A `matrix`, `data frame`, or atomic vector used as the input for
#' the `stat` function. Alternatively, you can specify a `function` (including
#' purrr-like lambda syntax) that will be applied to the layout matrix,
#' transforming it as necessary for statistic calculations. By default, it will
#' inherit from the layout matrix.
#' @inheritParams align_order
#' @inheritSection align Axis Alignment for Observations
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_left() +
#'     align_reorder(hclust2)
#' @seealso [order2()]
#' @importFrom ggplot2 waiver
#' @importFrom rlang list2
#' @export
align_reorder <- function(stat, ..., reverse = FALSE,
                          strict = TRUE, data = NULL,
                          active = NULL, set_context = deprecated(),
                          name = deprecated()) {
    stat <- rlang::as_function(stat)
    assert_bool(strict)
    assert_bool(reverse)
    assert_active(active)
    active <- update_active(active, new_active(
        use = FALSE, order = NA_integer_, name = NA_character_
    ))
    active <- deprecate_active(active, "align_order",
        set_context = set_context, name = name
    )
    align(
        align = AlignReorder,
        params = list(
            stat = stat,
            stat_params = list2(...),
            reverse = reverse,
            strict = strict
        ),
        active = active,
        check.param = TRUE,
        data = data %||% waiver()
    )
}

#' @export
summary.AlignOrder <- function(object, ...) c(TRUE, FALSE)

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject
AlignReorder <- ggproto("AlignReorder", Align,
    compute = function(self, panel, index, stat, stat_params, strict) {
        assert_reorder(self, panel, strict)
        data <- .subset2(self, "data")
        inject(stat(data, !!!stat_params))
    },
    layout = function(self, panel, index, reverse) {
        index <- vec_cast(
            order2(.subset2(self, "statistics")), integer(),
            x_arg = "order2", call = .subset2(self, "call")
        )
        assert_mismatch_nobs(
            self, nrow(.subset2(self, "data")), length(index),
            msg = "must return a statistic with",
            arg = "stat"
        )
        if (reverse) index <- rev(index)
        list(panel, index)
    }
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

#' @importFrom utils getFromNamespace
#' @export
#' @rdname order2
order2.ser_permutation_vector <- function(x) {
    rlang::check_installed(
        "seriation", "to extract order from `ser_permutation_vector`"
    )
    getFromNamespace("get_order", "seriation")(x)
}

#' @importFrom utils getFromNamespace
#' @export
#' @rdname order2
order2.ser_permutation <- function(x) {
    rlang::check_installed(
        "seriation", "to extract order from `ser_permutation`"
    )
    getFromNamespace("get_order", "seriation")(x)
}
