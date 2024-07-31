#' Group rows/columns
#' @param group A character define the groups, this will split the
#' heatmap into different panels.
#' @inheritParams align
#' @inherit align return
#' @examples
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(small_mat) +
#'   hmanno("top") +
#'   align_group(sample(letters[1:4], ncol(small_mat), replace = TRUE))
#' @export
align_group <- function(group, set_context = FALSE, name = NULL) {
    align(
        align_class = AlignGroup,
        params = list(group = group),
        set_context = set_context,
        name = name, order = NULL,
        check.param = TRUE
    )
}

AlignGroup <- ggplot2::ggproto("AlignGroup", Align,
    setup_params = function(self, data, params) {
        if (nrow(data) != length(group <- .subset2(params, "group"))) {
            cli::cli_abort(paste(
                "{.arg group} of {.fn {snake_class(self)}} must be ",
                sprintf(
                    "the same length of heatmap %s axis (%d)",
                    to_matrix_axis(.subset2(self, "direction")),
                    nrow(data)
                )
            ), call = .subset2(self, "call"))
        }
        params
    },
    layout = function(self, panels, index, group) {
        if (!is.null(panels)) {
            direction <- .subset2(self, "direction")
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = sprintf(
                    "group of heatmap %s already exists",
                    to_matrix_axis(direction)
                )
            ), call = self$call)
        }
        list(group, index)
    }
)
