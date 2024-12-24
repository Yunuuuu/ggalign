#' Split observations by k-means clustering groups.
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Aligns and groups observations based on k-means clustering, enabling
#' observation splits by cluster groups.
#'
#' @inheritParams stats::kmeans
#' @inheritDotParams stats::kmeans -x -centers
#' @inheritParams align_discrete
#' @inheritSection align_discrete Discrete Axis Alignment
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_kmeans(3L)
#' @importFrom rlang list2
#' @export
align_kmeans <- function(centers, ..., data = NULL, active = NULL) {
    assert_active(active)
    active <- update_active(active, new_active(use = FALSE))
    align_discrete(
        align = AlignKmeans,
        params = list(centers = centers, params = list2(...)),
        active = active,
        data = data %||% waiver()
    )
}

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject
AlignKmeans <- ggproto("AlignKmeans", AlignDiscrete,
    setup_data = function(self, params, data) {
        ans <- fortify_matrix(data)
        assert_(
            ans, is.numeric, "a numeric matrix",
            arg = "data", call = .subset2(self, "call")
        )
        ans
    },
    compute = function(self, panel, index, centers, params) {
        data <- .subset2(self, "data")
        inject(stats::kmeans(x = data, centers = centers, !!!params))
    },
    align = function(self, panel, index) {
        list(.subset2(.subset2(self, "statistics"), "cluster"), index)
    },
    summary_align = function(self) c(FALSE, TRUE)
)
