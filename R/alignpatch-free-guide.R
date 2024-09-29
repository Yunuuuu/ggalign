#' @param guides A string containing one or more of `r rd_values(.tlbr)`
#' indicates which side of guide legends should be collected for the plot. If
#' `NULL`, no guide legends will be collected.
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

#' @export
free_guide.free_guide <- function(plot, guides = "tlbr") {
    if (is.null(guides)) {
        attr(plot, "free_guides") <- NULL
    } else {
        assert_position(guides)
        if (!is.null(old <- attr(plot, "free_guides"))) {
            attr(plot, "free_guides") <- union_position(old, guides)
        } else {
            attr(plot, "free_guides") <- guides
        }
    }
    plot
}

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_guide <- function(x) {
    Parent <- NextMethod()
    if (!is.null(free_guides <- attr(x, "free_guides"))) {
        free_guides <- setup_pos(free_guides)
    }
    ggproto(
        "PatchFreeGuide", Parent,
        free_guides = free_guides,
        set_guides = function(self, guides) {
            union(
                self$free_guides,
                # we always collect guides in the border
                intersect(guides, .subset2(self, "borders"))
            )
        }
    )
}
