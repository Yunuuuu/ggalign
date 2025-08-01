#' Split observations by k-means clustering groups.
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Aligns and groups observations based on k-means clustering, enabling
#' observation splits by cluster groups.
#'
#' @inheritDotParams stats::kmeans -x -centers
#' @param data A numeric matrix to be used by k-means. By default, it will
#' inherit from the layout matrix.
#' @inheritParams align
#' @inheritSection align Discrete Axis Alignment
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_kmeans(3L)
#' @importFrom rlang list2
#' @export
align_kmeans <- function(..., data = NULL, active = NULL) {
    assert_active(active)
    active <- active_update(active(use = FALSE), active)
    align(
        align = AlignKmeans,
        params = list2(...),
        active = active,
        data = data
    )
}

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject
AlignKmeans <- ggproto("AlignKmeans", CraftAlign,
    interact_layout = function(self, layout) {
        ggproto_parent(AlignOrder2, self)$interact_layout(layout)
    },
    compute = function(self, panel, index) {
        inject(stats::kmeans(x = self$data, !!!self$params))
    },
    align = function(self, panel, index) {
        list(.subset2(self$statistics, "cluster"), index)
    },
    summary_align = function(self) c(FALSE, TRUE)
)
