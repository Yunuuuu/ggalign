#' @param guides A string containing one or more of
#' `r oxford_and(c(.tlbr, "i"))` indicates which side of guide legends should be
#' collected for the plot. If `NULL`, no guide legends will be collected.
#' @return
#' - `free_guide`: A modified version of `plot` with a `ggalign_free_guide`
#'   class.
#' @export
#' @rdname free
free_guide <- function(plot, guides = "tlbr") {
    if (!is.null(guides)) assert_guides(guides)
    UseMethod("free_guide")
}

#' @export
free_guide.default <- function(plot, guides = "tlbr") {
    attr(plot, "ggalign_free_guides") <- guides
    add_class(plot, "ggalign_free_guide")
}

#' @export
free_guide.ggalign_free_guide <- function(plot, guides = "tlbr") {
    if (is.null(guides)) {
        attr(plot, "ggalign_free_guides") <- NULL
    } else {
        if (is.null(old <- attr(plot, "ggalign_free_guides", exact = TRUE))) {
            attr(plot, "ggalign_free_guides") <- guides
        } else {
            attr(plot, "ggalign_free_guides") <- union_position(old, guides)
        }
    }
    plot
}

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
patch.ggalign_free_guide <- function(x) {
    Parent <- NextMethod()
    if (!is.null(guides <- attr(x, "ggalign_free_guides", exact = TRUE))) {
        guides <- setup_guides(guides)
    }
    ggproto("PatchFreeGuide", Parent,
        free_guides = guides,
        setup_options = function(self, options) {
            options <- ggproto_parent(Parent, self)$setup_options(options)
            prop(options, "guides", check = FALSE) <- self$free_guides
            options
        }
    )
}
