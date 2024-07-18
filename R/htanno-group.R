#' Group Heatmap rows/columns
#' @param group A character define the heatmap groups, this will split the
#' heatmap into different panels.
#' @inheritParams htanno
#' @inherit htanno return
#' @examples
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheat(small_mat) +
#'     htanno_group(
#'         sample(letters[1:4], ncol(small_mat), replace = TRUE),
#'         position = "top"
#'     )
#' @export
htanno_group <- function(group, position = NULL,
                         set_context = NULL, name = NULL,
                         check.param = TRUE) {
    htanno(
        htanno_class = HtannoGroup,
        position = position,
        params = list(group = group),
        set_context = set_context %||% c(TRUE, FALSE),
        name = name, order = NULL,
        check.param = check.param
    )
}

HtannoGroup <- ggplot2::ggproto("HtannoGroup", HtannoProto,
    setup_params = function(self, data, params, position) {
        if (nrow(data) != length(group <- .subset2(params, "group"))) {
            cli::cli_abort(paste(
                "{.arg group} of {.fn {snake_class(self)}} must be ",
                sprintf(
                    "the same length of heatmap %s axis (%d)",
                    to_matrix_axis(position), nrow(data)
                )
            ), call = self$call)
        }
        params
    },
    layout = function(self, data, panels, index, position, group) {
        if (!is.null(panels)) {
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = "group of heatmap {to_matrix_axis(position)} already exists"
            ), call = self$call)
        }
        list(group, index)
    }
)
