#' @param spaces Which border spaces should be removed? A string containing one
#' or more of `r oxford_and(.tlbr)`.
#' @return
#' - `free_space`: A modified version of `plot` with a `FreeSpace` class.
#' @export
#' @rdname free
free_space <- S7::new_generic(
    "free_space", "plot",
    function(plot, spaces = "tlbr") S7_dispatch()
)

FreeSpace <- S7::new_class(
    "FreeSpace",
    properties = list(
        plot = S7::class_any,
        spaces = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single string")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
                if (grepl("[^tlbr]", value)) {
                    return(sprintf(
                        "can only contain the %s characters",
                        oxford_and(.tlbr)
                    ))
                }
            }
        )
    )
)

S7::method(free_space, S7::class_any) <- function(plot, spaces = "tlbr") {
    FreeSpace(plot, spaces)
}

#' @importFrom S7 prop prop<-
S7::method(free_space, FreeSpace) <- function(plot, spaces = "tlbr") {
    old <- prop(plot, "spaces")
    prop(plot, "spaces") <- spaces # will validate the input spaces
    prop(plot, "spaces", check = FALSE) <- union_position(old, spaces)
    plot
}

##########################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid unit
S7::method(alignpatch, FreeSpace) <- function(x) {
    Parent <- alignpatch(prop(x, "plot"))
    ggproto(
        "PatchFreeSpace", Parent,
        spaces = split_position(prop(x, "spaces")),
        get_sizes = function(self, gt, free = NULL) {
            ggproto_parent(Parent, self)$get_sizes(gt, union(free, self$spaces))
        }
    )
}
