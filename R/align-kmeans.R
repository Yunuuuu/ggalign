#' Group Heatmap rows/columns by kmeans
#' @inheritParams stats::kmeans
#' @inheritParams align
#' @inherit align return
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("t") +
#'     align_kmeans(3L)
#' @export
align_kmeans <- function(centers, iter.max = 10, nstart = 1,
                         algorithm = c(
                             "Hartigan-Wong", "Lloyd", "Forgy",
                             "MacQueen"
                         ), trace = FALSE,
                         data = NULL, set_context = FALSE, name = NULL) {
    algorithm <- match.arg(algorithm)
    align(
        align_class = AlignKmeans,
        params = list(
            centers = centers,
            iter.max = iter.max,
            nstart = nstart,
            algorithm = algorithm,
            trace = trace
        ),
        set_context = set_context,
        name = name, order = NULL, data = data
    )
}

AlignKmeans <- ggplot2::ggproto("AlignKmeans", Align,
    setup_data = function(self, data, params) {
        ans <- as.matrix(data)
        assert_(
            ans, is.numeric, "a numeric matrix",
            arg = "data", call = .subset2(self, "call")
        )
        ans
    },
    compute = function(self, panels, index,
                       centers, iter.max, nstart, algorithm, trace) {
        data <- .subset2(self, "data")
        stats::kmeans(data, centers, iter.max, nstart, algorithm, trace)
    },
    layout = function(self, panels, index) {
        assert_sub_split(self, panels)
        list(.subset2(.subset2(self, "statistics"), "cluster"), index)
    }
)
