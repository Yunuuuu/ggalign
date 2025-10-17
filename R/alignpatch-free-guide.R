#' @param guides A string containing one or more of
#' `r oxford_and(c(.tlbr, "i"))` indicates which side of guide legends should be
#' collected for the plot. If `NULL`, no guide legends will be collected.
#' @return
#' - `free_guide`: A modified version of `plot` with a `FreeGuide` class.
#' @export
#' @rdname free
free_guide <- S7::new_generic(
    "free_guide", "plot",
    function(plot, guides = "tlbr") S7_dispatch()
)

FreeGuide <- S7::new_class(
    "FreeGuide",
    properties = list(
        plot = S7::class_any,
        guides = S7::new_property(
            S7::new_union(S7::class_character, NULL),
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single string")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
                if (grepl("[^tlbri]", value)) {
                    return(sprintf(
                        "can only contain the %s characters",
                        oxford_and(c(.tlbr, "i"))
                    ))
                }
            }
        )
    )
)

S7::method(free_guide, S7::class_any) <- function(plot, guides = "tlbr") {
    FreeGuide(plot, guides)
}

#' @importFrom S7 prop
S7::method(free_guide, FreeGuide) <- function(plot, guides = "tlbr") {
    old <- prop(plot, "guides")
    prop(plot, "guides") <- guides # will validate the input guides
    if (!is.null(guides) && !is.null(old)) {
        prop(plot, "guides", check = FALSE) <- union_position(old, guides)
    }
    plot
}

################################################################
#' @importFrom ggplot2 ggproto
S7::method(alignpatch, FreeGuide) <- function(x) {
    Parent <- alignpatch(prop(x, "plot"))
    if (!is.null(guides <- prop(x, "guides"))) guides <- setup_guides(guides)
    ggproto("PatchFreeGuide", Parent,
        free_guides = guides,
        guides = function(self, guides) self$free_guides
    )
}
