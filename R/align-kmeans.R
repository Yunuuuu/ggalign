#' Split layout by k-means clustering groups.
#'
#' @inheritParams stats::kmeans
#' @inheritDotParams stats::kmeans -x -centers
#' @inheritParams align_dendro
#' @return A `"AlignKmeans"` object.
#' @inheritSection align Aligned Axis
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_kmeans(3L)
#' @export
align_kmeans <- function(centers, ..., data = NULL,
                         context = NULL, set_context = deprecated(),
                         name = deprecated()) {
    assert_s3_class(context, "plot_context", null_ok = TRUE)
    context <- update_context(context, new_context(
        active = FALSE, order = NA_integer_, name = NA_character_
    ))
    context <- deprecate_context(context, "align_group",
        set_context = set_context, name = name
    )
    align(
        align_class = AlignKmeans,
        params = list(centers = centers, params = rlang::list2(...)),
        context = context,
        data = data %||% waiver()
    )
}

#' @importFrom ggplot2 ggproto
AlignKmeans <- ggproto("AlignKmeans", Align,
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
        rlang::inject(stats::kmeans(x = data, centers = centers, !!!params))
    },
    layout = function(self, panel, index) {
        list(.subset2(.subset2(self, "statistics"), "cluster"), index)
    }
)
