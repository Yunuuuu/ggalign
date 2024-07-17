#' Group Heatmap rows/columns by kmeans
#' @inheritParams stats::kmeans
#' @inheritParams htanno
#' @inherit htanno return
#' @export
htanno_kmeans <- function(centers, iter.max = 10, nstart = 1,
                          algorithm = c(
                              "Hartigan-Wong", "Lloyd", "Forgy",
                              "MacQueen"
                          ), trace = FALSE,
                          position = NULL,
                          set_context = NULL, name = NULL,
                          check.param = TRUE) {
    algorithm <- match.arg(algorithm)
    htanno(
        htanno_class = HtannKmeans,
        position = position,
        params = list(
            centers = centers,
            iter.max = iter.max,
            nstart = nstart,
            algorithm = algorithm,
            trace = trace
        ),
        set_context = set_context %||% c(TRUE, FALSE),
        name = name, order = NULL,
        check.param = check.param
    )
}

HtannKmeans <- ggplot2::ggproto("HtannKmeans", HtannoProto,
    setup_data = function(self, data, params, position) {
        ans <- as.matrix(data)
        assert_(ans, function(x) is.numeric(x),
            arg = "data", call = self$call
        )
        ans
    },
    compute = function(data, panels, index, position,
                       centers, iter.max, nstart, algorithm, trace) {
        stats::kmeans(data, centers, iter.max, nstart, algorithm, trace)
    },
    layout = function(self, data, statistics, panels, index, position) {
        if (!is.null(panels)) {
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = "group of heatmap {to_matrix_axis(position)} already exists"
            ), call = self$call)
        }
        if (!is.null(index)) {
            cli::cli_abort(sprintf(
                "{.fn {snake_class(self)}} will disrupt the previously %s %s",
                "established order of the heatmap",
                to_matrix_axis(position)
            ), call = self$call)
        }
        list(statistics$cluster, index)
    }
)
