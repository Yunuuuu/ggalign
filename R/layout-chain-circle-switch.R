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
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#' colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#' circle_discrete(small_mat) +
#'     ggalign() +
#'     geom_tile(aes(y = .column_index, fill = value)) +
#'     scale_fill_viridis_c() +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     scale_color_brewer(palette = "Dark2")
#' @export
circle_switch <- function(radial = waiver(), direction = NULL,
                          what = waiver(), ...) {
    rlang::check_dots_empty()
    if (!is_waiver(radial) && !is.null(radial)) {
        assert_s3_class(radial, "CoordRadial")
        if (abs(diff(radial$arc)) < pi / 2L) {
            cli_abort("Cannot use circle of acute angle < 90 in {.arg radial}")
        }
    }
    if (!is.null(direction)) {
        direction <- arg_match0(direction, c("inward", "outward"))
    }
    if (!is_waiver(what)) what <- check_stack_context(what)
    structure(list(what = what, radial = radial, direction = direction),
        class = "circle_switch"
    )
}
