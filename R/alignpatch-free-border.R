#' @param borders Which border shouldn't be aligned? A string containing one or
#' more of `r oxford_and(.tlbr)`.
#' @return
#' - `free_border`: A modified version of `plot` with a `FreeBorder` class.
#' @importFrom S7 S7_dispatch
#' @export
#' @rdname free
free_border <- S7::new_generic(
    "free_border", "plot",
    function(plot, borders = "tlbr") S7_dispatch()
)

FreeBorder <- S7::new_class(
    "FreeBorder",
    properties = list(
        plot = S7::class_any,
        borders = S7::new_property(
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

S7::method(free_border, S7::class_any) <- function(plot, borders = "tlbr") {
    FreeBorder(plot, borders)
}

#' @importFrom S7 prop
S7::method(free_border, FreeBorder) <-
    function(plot, borders = "tlbr") {
        old <- prop(plot, "borders")
        prop(plot, "borders") <- borders # will validate the input borders
        prop(plot, "borders", check = FALSE) <- union_position(old, borders)
        plot
    }

free_border.free_lab <- function(plot, borders = "tlbr") {
    assert_position(borders)
    free_labs <- setdiff_position(attr(plot, "free_labs"), borders)
    if (nzchar(free_labs)) {
        attr(plot, "free_labs") <- free_labs
    } else {
        attr(plot, "free_labs") <- NULL
        plot <- remove_class(plot, "free_lab")
    }
    NextMethod()
}

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom gtable is.gtable
S7::method(alignpatch, FreeBorder) <- function(x) {
    Parent <- alignpatch(prop(x, "plot"))
    ggproto(
        "PatchFreeBorder", Parent,
        borders = split_position(prop(x, "borders")),
        align_border = function(self, gt, t = NULL, l = NULL,
                                b = NULL, r = NULL) {
            if (is.gtable(gt)) {
                for (border in self$borders) {
                    # Adjust the gtable margins to match the aligned border sizes
                    if (border == "t") {
                        # skip if no border sizes defined
                        if (is.null(t)) next
                        top <- gt$heights[seq_along(t)]
                        top[1L] <- top[1L] + sum(t) - sum(top)
                        t <- top
                    }
                    if (border == "l") {
                        if (is.null(l)) next
                        left <- gt$widths[seq_along(l)]
                        left[1L] <- left[1L] + sum(l) - sum(left)
                        l <- left
                    }
                    if (border == "b") {
                        if (is.null(b)) next
                        n_row <- nrow(gt)
                        bottom <- gt$heights[seq(n_row - length(b) + 1L, n_row)]
                        bottom[length(b)] <- bottom[length(b)] +
                            sum(b) - sum(bottom)
                        b <- bottom
                    }
                    if (border == "r") {
                        if (is.null(r)) next
                        n_col <- ncol(gt)
                        right <- gt$widths[seq(n_col - length(r) + 1L, n_col)]
                        right[length(r)] <- right[length(r)] +
                            sum(r) - sum(right)
                        r <- right
                    }
                }
            }
            ggproto_parent(Parent, self)$align_border(gt, t, l, b, r)
        }
    )
}
