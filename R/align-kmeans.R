#' Split layout by k-means clustering groups.
#'
#' @inheritParams stats::kmeans
#' @inheritDotParams stats::kmeans -x -centers
#' @inheritParams align
#' @inherit align return
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("t") +
#'     align_kmeans(3L)
#' @export
align_kmeans <- function(centers, ...,
                         data = NULL, set_context = FALSE, name = NULL) {
    align(
        align_class = AlignKmeans,
        params = list(centers = centers, params = rlang::list2(...)),
        set_context = set_context,
        name = name, order = NULL,
        data = data %||% waiver()
    )
}

#' @importFrom ggplot2 ggproto
AlignKmeans <- ggproto("AlignKmeans", Align,
    setup_data = function(self, params, data) {
        ans <- as.matrix(data)
        assert_(
            ans, is.numeric, "a numeric matrix",
            arg = "data", call = .subset2(self, "call")
        )
        ans
    },
    compute = function(self, panel, index, centers, params) {
        data <- .subset2(self, "data")
        rlang::inject(stats::kmeans(x = data, centers = centers, !!!params))
    },
    layout = function(self, panel, index) {
        list(.subset2(.subset2(self, "statistics"), "cluster"), index)
    }
)
