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
#' @export
htanno_reorder <- function(fun = rowMeans, ..., strict = TRUE,
                           decreasing = FALSE,
                           data = NULL, position = NULL,
                           set_context = NULL, name = NULL) {
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
        check.param = TRUE
    )
}

HtannoReorder <- ggplot2::ggproto("HtannoReorder", HtannoProto,
    layout = function(self, data, panels, index, position,
                      fun, fun_params, strict, decreasing) {
        axis <- to_matrix_axis(position)
        if (!is.null(panels) && strict) {
            cli::cli_abort(c(
                paste(
                    "{.fn {snake_class(self)}} cannot reordering heatmap",
                    "since group of heatmap {axis} exists"
                ),
                i = "try to set `strict = FALSE` to reorder within each group"
            ), call = self$call)
        }
        weights <- rlang::inject(fun(data, !!!fun_params))
        if (nrow(data) != length(weights)) {
            cli::cli_abort(paste(
                "{.arg fun} of {.fn {snake_class(self)}} must return an",
                sprintf(
                    "integer with the same length of heatmap %s axis (%d)",
                    to_matrix_axis(position), nrow(data)
                )
            ), call = self$call)
        }
        list(panels, order(weights, decreasing = decreasing))
    }
)
