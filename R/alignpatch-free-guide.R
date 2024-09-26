#' @param guides `r rd_guides()`
#' @return
#' - `free_guide`: A modified version of `plot` with a `free_guide` class.
#' @export
#' @rdname free
free_guide <- function(plot, guides = "tlbr") {
    UseMethod("free_guide")
}

#' @export
free_guide.ggplot <- function(plot, guides = "tlbr") {
    if (!is.null(guides)) assert_position(guides)
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
        set_guides = function(self, guides) {
            union(
                self$free_guides,
                # we always collect guides in the border
                intersect(guides, .subset2(self, "borders"))
            )
        }
    )
}
