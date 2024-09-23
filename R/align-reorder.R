#' Reorders layout observations based on weights or summary statistics.
#'
#' @param order A summary function. It should take a data and return the
#' statistic, which we'll call [order2()] to extract the order information. You
#' can also provide the ordering integer index or character.
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
#' @importFrom ggplot2 waiver
#' @export
align_reorder <- function(order = rowMeans, ..., strict = TRUE,
                          reverse = FALSE, data = NULL,
                          set_context = FALSE, name = NULL) {
    if (is.numeric(order) || is.character(order)) {
        if (anyNA(order) || anyDuplicated(order)) {
            cli::cli_abort(paste(
                "{.arg order} must be an ordering numeric or character",
                "without missing value or ties"
            ))
        } else if (!is.null(data) && !is.waive(data)) {
            cli::cli_warn(c(
                "{.arg data} won't be used",
                i = "{.arg order} is not a {.cls function}"
            ))
            data <- waiver()
        }
    } else {
        order <- rlang::as_function(order)
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
        if (is.function(order)) {
            ans <- rlang::inject(order(data, !!!order_params))
            index <- as.integer(order2(ans))
            msg <- "must return a statistic with"
        } else {
            if (is.numeric(order)) {
                ans <- as.integer(order)
                if (any(ans < 1L) || any(ans > nrow(data))) {
                    cli::cli_abort(
                        paste(
                            "Outliers found in the provided ordering",
                            "integer index {.arg order}"
                        ),
                        call = .subset2(self, "call")
                    )
                } else if (anyDuplicated(ans)) {
                    cli::cli_abort(
                        "find ties when coercing {.arg order} into integer",
                        call = .subset2(self, "call")
                    )
                }
            } else if (is.null(layout_nms <- rownames(data))) {
                cli::cli_abort(c(
                    sprintf(
                        "No names found in layout %s-axis",
                        to_coord_axis(.subset2(self, "direction"))
                    ),
                    i = "Cannot use ordering character index {.arg order}"
                ), call = .subset2(self, "call"))
            } else {
                ans <- match(order, layout_nms)
                if (anyNA(ans)) {
                    cli::cli_abort(sprintf(
                        "{.arg order} contains invalid names: %s",
                        style_val(order[is.na(ans)])
                    ), call = .subset2(self, "call"))
                }
            }
            msg <- "must be an ordering integer index or character of"
            index <- ans
        }
        assert_mismatch_nobs(
            self, nrow(data), length(index),
            msg = msg, arg = "order"
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
