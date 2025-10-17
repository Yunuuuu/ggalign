#' @param axes Which axes shouldn't be aligned? A string containing
#' one or more of `r oxford_and(.tlbr)`.
#' @return
#' - `free_align`: A modified version of `plot` with a `FreeAlign` class.
#' @importFrom S7 S7_dispatch
#' @export
#' @rdname free
free_align <- S7::new_generic(
    "free_align", "plot",
    function(plot, axes = "tlbr") S7_dispatch()
)

FreeAlign <- S7::new_class(
    "FreeAlign",
    properties = list(
        plot = S7::class_any,
        axes = S7::new_property(
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

S7::method(free_align, S7::class_any) <- function(plot, axes = "tlbr") {
    FreeAlign(plot, axes)
}

#' @importFrom S7 prop
S7::method(free_align, FreeAlign) <- function(plot, axes = "tlbr") {
    old <- prop(plot, "axes")
    prop(plot, "axes") <- axes # will validate the input axes
    prop(plot, "axes", check = FALSE) <- union_position(old, axes)
    plot
}

#' @importFrom ggplot2 ggproto ggproto_parent
S7::method(alignpatch, FreeAlign) <- function(x) {
    Parent <- alignpatch(prop(x, "plot"))
    ggproto(
        "PatchFreeAlign", Parent,
        axes = split_position(prop(x, "axes")),
        get_sizes = function(self, gt, free = NULL) {
            ggproto_parent(Parent, self)$get_sizes(gt, union(free, self$axes))
        },
        align_border = function(self, gt,
                                t = NULL, l = NULL, b = NULL, r = NULL) {
            for (axis in self$axes) {
                assign(x = axis, value = NULL, envir = environment())
            }
            ggproto_parent(Parent, self)$align_border(gt, t, l, b, r)
        }
    )
}
