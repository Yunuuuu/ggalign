#' Group Heatmap rows/columns by kmeans
#' @inheritParams stats::kmeans
#' @inheritParams align
#' @inherit align return
#' @examples
#' ggheat(matrix(rnorm(81), nrow = 9)) +
#'     align_kmeans(3L, position = "top")
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
    setup_data = function(self) {
        ans <- as.matrix(.subset2(self, "data"))
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
        if (!is.null(panels)) {
            position <- .subset2(self, "position")
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = "group of heatmap {to_matrix_axis(position)} already exists"
            ), call = self$call)
        }
        list(.subset2(.subset2(self, "statistics"), "cluster"), index)
    }
)
