#' Group and align observations based on a group vector
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Splits observations into groups, with slice ordering based on group levels.
#'
#' @param group A character define the groups of the observations.
#' @inheritParams align_gg
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
    active <- update_active(active, new_active(use = FALSE))
    active <- deprecate_active(active, "align_group",
        set_context = set_context, name = name
    )
    align(
        align = AlignGroup,
        params = list(group = group),
        data = NULL, active = active,
        check.param = TRUE
    )
}

#' @export
summary.AlignGroup <- function(object, ...) c(FALSE, TRUE)

#' @importFrom ggplot2 ggproto
AlignGroup <- ggproto("AlignGroup", Align,
    nobs = function(self, params) vec_size(.subset2(params, "group")),
    setup_params = function(self, nobs, params) {
        assert_mismatch_nobs(
            self, nobs, self$nobs(params),
            msg = "must be an atomic vector",
            arg = "group"
        )
        params
    },
    align = function(self, panel, index, group) list(group, index)
)
