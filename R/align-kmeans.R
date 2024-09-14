#' Split layout by k-means clustering groups.
#'
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
    compute = function(self, panel, index,
                       centers, iter.max, nstart, algorithm, trace) {
        data <- .subset2(self, "data")
        stats::kmeans(data, centers, iter.max, nstart, algorithm, trace)
    },
    layout = function(self, panel, index) {
        assert_sub_split(self, panel)
        list(.subset2(.subset2(self, "statistics"), "cluster"), index)
    }
)
