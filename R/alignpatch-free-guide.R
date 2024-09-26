#' @inheritParams align_plots
#' @return
#' - `free_guide`: A modified version of `plot` with a `free_guide` class.
#' @export
#' @rdname free
free_guide <- function(plot, guides = "tlbr") {
    UseMethod("free_guide")
}

#' @export
free_guide.ggplot <- function(plot, guides = "tlbr") {
    assert_position(guides)
    attr(plot, "free_guides") <- guides
    add_class(plot, "free_guide")
}

#' @export
free_guide.alignpatches <- free_guide.ggplot

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_guide <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeGuide", Parent,
        free_guides = setup_pos(attr(x, "free_guides")),
        collect_guides = function(self, guides, gt = self$gt) {
            ggproto_parent(Parent, self)$collect_guides(
                guides = self$free_guides, gt = gt
            )
        }
    )
}
