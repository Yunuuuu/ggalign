#' Reorders layout observations based on weights or summary statistics.
#'
#' @param order A summary function. It should take a data and return the
#' statistic, which we'll call [order2()] to extract the order information. You
#' can also provide the ordering integer index and wrap it with [I()].
#' @param ... Additional arguments passed to function provided in `order`
#' argument.
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
#' @seealso [order2()]
#' @export
align_reorder <- function(order = rowMeans, ..., strict = TRUE,
                          reverse = FALSE, data = NULL,
                          set_context = FALSE, name = NULL) {
    if (!inherits(order, "AsIs")) {
        order <- rlang::as_function(order)
    } else if (!is.numeric(order) || anyNA(order) || anyDuplicated(order)) {
        cli::cli_abort(
            "{.arg order} must be a numeric without missing value or ties"
        )
    }
    assert_bool(strict)
    assert_bool(reverse)
    align(
        align_class = AlignReorder,
        params = list(
            order = order,
            order_params = rlang::list2(...),
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
    compute = function(self, panel, index, order, order_params, strict) {
        data <- .subset2(self, "data")
        assert_reorder(self, panel, strict)
        if (inherits(order, "AsIs")) {
            ans <- as.integer(order)
            if (any(ans < 1L) || any(ans > nrow(data))) {
                cli::cli_abort(
                    "Outliers found in the provided ordering index",
                    call = .subset2(self, "call")
                )
            }
            msg <- "must be an ordering integer index of"
            index <- ans
        } else {
            ans <- rlang::inject(order(data, !!!order_params))
            index <- as.integer(order2(ans))
            msg <- "must return a statistic with"
        }
        assert_mismatch_nobs(
            self, nrow(data), length(ans),
            msg = smg, arg = "order"
        )
        self$index <- index
        ans
    },
    layout = function(self, panel, index, reverse) {
        index <- .subset2(self, "index")
        if (reverse) index <- rev(index)
        list(panel, index)
    }
)
