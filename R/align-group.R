#' Group and align layout based on categorical or factor levels.
#'
#' @param group A character define the groups, this will split the
#' axis into different panel.
#' @inheritParams align
#' @inherit align return
#' @examples
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(small_mat) +
#'     hmanno("top") +
#'     align_group(sample(letters[1:4], ncol(small_mat), replace = TRUE))
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
    nobs = function(self, params) {
        length(.subset2(params, "group"))
    },
    setup_params = function(self, nobs, params) {
        assert_mismatch_nobs(self, nobs,
            length(.subset2(params, "group")),
            arg = "group", 
            msg = "must be an atomic vector"
        )
        params
    },
    layout = function(self, panel, index, group) {
        assert_sub_split(self, panel)
        list(group, index)
    }
)
