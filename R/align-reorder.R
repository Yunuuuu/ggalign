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
#' @inheritParams align
#' @inherit align return
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("l") +
#'     align_reorder()
#' @export
align_reorder <- function(fun = rowMeans, ..., strict = TRUE,
                          decreasing = FALSE,
                          data = NULL,
                          set_context = FALSE, name = NULL) {
    fun <- rlang::as_function(fun)
    assert_bool(strict)
    assert_bool(decreasing)
    align(
        align_class = AlignReorder,
        params = list(
            fun = fun,
            fun_params = rlang::list2(...),
            decreasing = decreasing,
            strict = strict
        ),
        set_context = set_context,
        name = name, order = NULL,
        check.param = TRUE, data = data
    )
}

AlignReorder <- ggplot2::ggproto("AlignReorder", Align,
    setup_data = function(self, data, params) data,
    compute = function(self, panels, index, fun, fun_params, strict) {
        data <- .subset2(self, "data")
        direction <- .subset2(self, "direction")
        if (!is.null(panels) && strict) {
            axis <- to_matrix_axis(direction)
            cli::cli_abort(c(
                paste(
                    "{.fn {snake_class(self)}} cannot reordering {axis}-axis",
                    "since group of layout {axis}-axis exists"
                ),
                i = "try to set `strict = FALSE` to reorder within each group"
            ), call = .subset2(self, "call"))
        }
        weights <- rlang::inject(fun(data, !!!fun_params))
        if (nrow(data) != length(weights)) {
            cli::cli_abort(paste(
                "{.arg fun} of {.fn {snake_class(self)}} must return an atomic",
                sprintf(
                    "vector with the same length of %s-axis (%d)",
                    to_matrix_axis(direction), nrow(data)
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
