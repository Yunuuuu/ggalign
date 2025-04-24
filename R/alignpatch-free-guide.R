#' @param guides A string containing one or more of
#' `r oxford_and(c(.tlbr, "i"))` indicates which side of guide legends should be
#' collected for the plot. If `NULL`, no guide legends will be collected.
#' @return
#' - `free_guide`: A modified version of `plot` with a `free_guide` class.
#' @export
#' @rdname free
free_guide <- function(plot, guides = "tlbr") {
    UseMethod("free_guide")
}

#' @export
free_guide.ggplot <- function(plot, guides = "tlbr") {
    if (!is.null(guides)) assert_guides(guides)
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
        assert_guides(guides)
        if (is.null(old <- attr(plot, "free_guides", exact = TRUE))) {
            attr(plot, "free_guides") <- guides
        } else {
            attr(plot, "free_guides") <- union_position(old, guides)
        }
    }
    plot
}

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_guide <- function(x) {
    Parent <- NextMethod()
    if (!is.null(free_guides <- attr(x, "free_guides", exact = TRUE))) {
        free_guides <- setup_guides(free_guides)
    }
    ggproto("PatchFreeGuide", Parent,
        set_guides = function(self, guides) free_guides
    )
}
