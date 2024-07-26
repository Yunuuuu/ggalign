#' Reorder Heatmap rows/columns
#'
#' @param fun A summary function. It should take a data and return the weights
#' for the heatmap rows/columns.
#' @param ... Additional arguments passed to `fun`.
#' @param strict A boolean value indicates whether the order should be strict.
#' If previous groups exist, and strict is `FALSE`, this will reorder the
#' heatmap rows/columns in each group.
#' @param decreasing A boolean value. Should the sort order be increasing or
#' decreasing?
#' @inheritParams htanno
#' @inherit htanno return
#' @examples
#' ggheat(matrix(rnorm(81), nrow = 9)) +
#'     htanno_reorder(position = "top")
#' @export
htanno_reorder <- function(fun = rowMeans, ..., strict = TRUE,
                           decreasing = FALSE,
                           data = NULL,
                           set_context = NULL, name = NULL,
                           position = NULL) {
    fun <- rlang::as_function(fun)
    assert_bool(strict)
    assert_bool(decreasing)
    htanno(
        htanno_class = HtannoReorder,
        position = position,
        params = list(
            fun = fun,
            fun_params = rlang::list2(...),
            decreasing = decreasing,
            strict = strict
        ),
        set_context = set_context %||% c(TRUE, FALSE),
        name = name, order = NULL,
        check.param = TRUE, data = data
    )
}

HtannoReorder <- ggplot2::ggproto("HtannoReorder", Align,
    setup_data = function(self) .subset2(self, "data"),
    compute = function(self, panels, index, fun, fun_params, strict) {
        data <- .subset2(self, "data")
        position <- .subset2(self, "position")
        if (!is.null(panels) && strict) {
            axis <- to_matrix_axis(position)
            cli::cli_abort(c(
                paste(
                    "{.fn {snake_class(self)}} cannot reordering heatmap",
                    "since group of heatmap {axis} exists"
                ),
                i = "try to set `strict = FALSE` to reorder within each group"
            ), call = .subset2(self, "call"))
        }
        weights <- rlang::inject(fun(data, !!!fun_params))
        if (nrow(data) != length(weights)) {
            cli::cli_abort(paste(
                "{.arg fun} of {.fn {snake_class(self)}} must return an atomic",
                sprintf(
                    "vector with the same length of heatmap %s axis (%d)",
                    to_matrix_axis(position), nrow(data)
                )
            ), call = .subset2(self, "call"))
        }
        weights
    },
    layout = function(self, panels, index, decreasing) {
        list(
            panels,
            order(.subset2(self, "statistics"), decreasing = decreasing)
        )
    }
)
