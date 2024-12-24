#########################################################
#' Determine the active context of circle layout
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams circle_discrete
#' @param what What should get activated for the [`circle_layout()`]?
#' `r rd_chain_what()`.
#' @return A `circle_switch` object which can be added to [`circle_layout()`].
#' @examples
#' circle_discrete(matrix(rnorm(56), nrow = 7L)) +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3) +
#'     circle_switch(coord_radial(inner.radius = 0.5, expand = FALSE))
#' @export
circle_switch <- function(radial = waiver(), what = waiver(), ...) {
    rlang::check_dots_empty()
    if (!is.waive(radial)) {
        assert_s3_class(radial, "CoordRadial", allow_null = TRUE)
    }
    if (!is.waive(radial) &&
        !is.null(radial) &&
        abs(diff(radial$arc)) < pi / 2L) {
        cli_abort("Cannot use circle of acute angle < 90 in {.arg radial}")
    }
    if (!is.waive(what)) what <- check_stack_context(what)
    structure(list(what = what, radial = radial), class = "circle_switch")
}
