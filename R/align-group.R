#' Group and align layout based on categorical or factor levels.
#'
#' @param group A character define the groups of the observations.
#' @inheritParams align_gg
#' @return A `"AlignGroup"` object.
#' @examples
#' set.seed(1L)
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(small_mat) +
#'     anno_top() +
#'     align_group(sample(letters[1:4], ncol(small_mat), replace = TRUE))
#' @export
align_group <- function(group, active = NULL, set_context = deprecated(),
                        name = deprecated()) {
    assert_active(active)
    active <- update_active(active, new_active(
        use = FALSE, order = NA_integer_, name = NA_character_
    ))
    active <- deprecate_active(active, "align_group",
        set_context = set_context, name = name
    )
    align(
        align_class = AlignGroup,
        params = list(group = group),
        data = NULL, active = active,
        check.param = TRUE
    )
}

#' @importFrom ggplot2 ggproto
AlignGroup <- ggproto("AlignGroup", Align,
    nobs = function(self, params) length(.subset2(params, "group")),
    setup_params = function(self, nobs, params) {
        assert_mismatch_nobs(self, nobs,
            length(.subset2(params, "group")),
            msg = "must be an atomic vector",
            arg = "group"
        )
        params
    },
    layout = function(self, panel, index, group) list(group, index)
)
