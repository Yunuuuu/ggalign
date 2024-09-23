#' Reorders layout observations based on weights or summary statistics.
#'
#' @param fun A summary function. It should take a data and return the
#' statistic, which we'll call [order2()] to extract the order information.
#' @param ... Additional arguments passed to `fun`.
#' @param strict A boolean value indicates whether the order should be strict.
#' If previous groups has been established, and strict is `FALSE`, this will
#' reorder the observations in each group.
#' @param reverse A boolean value. Should the sort order be in reverse?
#' @inheritParams align
#' @inherit align return
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("l") +
#'     align_reorder()
#' @export
align_reorder <- function(fun = rowMeans, ..., strict = TRUE,
                          reverse = FALSE, data = NULL,
                          set_context = FALSE, name = NULL) {
    fun <- rlang::as_function(fun)
    assert_bool(strict)
    assert_bool(reverse)
    align(
        align_class = AlignReorder,
        params = list(
            fun = fun,
            fun_params = rlang::list2(...),
            reverse = reverse,
            strict = strict
        ),
        set_context = set_context,
        name = name, order = NULL,
        check.param = TRUE,
        data = data %||% waiver()
    )
}

#' @importFrom ggplot2 ggproto
AlignReorder <- ggproto("AlignReorder", Align,
    compute = function(self, panel, index, fun, fun_params, strict) {
        data <- .subset2(self, "data")
        assert_reorder(self, panel, strict)
        ans <- rlang::inject(fun(data, !!!fun_params))
        ans <- order2(ans)
        assert_mismatch_nobs(self,
            nrow(data), length(ans),
            msg = "must return a statistic with",
            arg = "fun"
        )
        ans
    },
    layout = function(self, panel, index, reverse) {
        index <- .subset2(self, "statistics")
        if (reverse) index <- rev(index)
        list(panel, index)
    }
)
