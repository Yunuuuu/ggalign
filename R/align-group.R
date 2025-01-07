#' Group and align observations based on a group vector
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Splits observations into groups, with slice ordering based on group levels.
#'
#' @param group A character define the groups of the observations.
#' @inheritParams align
#' @examples
#' set.seed(1L)
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(small_mat) +
#'     anno_top() +
#'     align_group(sample(letters[1:4], ncol(small_mat), replace = TRUE))
#' @export
align_group <- function(group, active = NULL) {
    assert_active(active)
    if (vec_size(group) == 0L) {
        cli_abort("{.arg group} cannot be empty")
    }
    active <- update_active(active, new_active(use = FALSE))
    align(
        align = AlignGroup,
        group = group,
        active = active,
        check.param = TRUE
    )
}

#' @importFrom ggplot2 ggproto
AlignGroup <- ggproto("AlignGroup", Align,
    interact_layout = function(self, layout) {
        layout <- ggproto_parent(Align, self)$interact_layout(layout)
        if (is.null(layout_nobs <- .subset2(layout@design, "nobs"))) {
            layout@design["nobs"] <- list(vec_size(self$group))
        } else {
            assert_mismatch_nobs(
                self, layout_nobs, vec_size(self$group),
                arg = "group"
            )
        }
        layout
    },
    align = function(self, panel, index) list(self$group, index),
    summary_align = function(self) c(FALSE, TRUE)
)
