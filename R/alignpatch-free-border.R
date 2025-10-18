#' @param borders Which border shouldn't be aligned? A string containing one or
#' more of `r oxford_and(.tlbr)`.
#' @return
#' - `free_border`: A modified version of `plot` with a `ggalign_free_border`
#'   class.
#' @export
#' @rdname free
free_border <- function(plot, borders = "tlbr") {
    assert_position(borders)
    UseMethod("free_border")
}

#' @export
free_border.default <- function(plot, borders = "tlbr") {
    attr(plot, "ggalign_free_borders") <- borders
    add_class(plot, "ggalign_free_border")
}

#' @export
free_border.ggalign_free_align <- function(plot, borders = "tlbr") {
    borders <- setdiff_position(
        borders,
        attr(plot, "ggalign_free_axes", exact = TRUE)
    )
    if (!nzchar(borders)) {
        return(plot)
    }
    NextMethod()
}

#' @export
free_border.ggalign_free_lab <- function(plot, borders = "tlbr") {
    labs <- setdiff_position(
        attr(plot, "ggalign_free_labs", exact = TRUE),
        borders
    )
    if (nzchar(labs)) {
        attr(plot, "ggalign_free_labs") <- labs
    } else {
        attr(plot, "ggalign_free_labs") <- NULL
        plot <- remove_class(plot, "ggalign_free_lab")
    }
    NextMethod()
}

#' @export
free_border.ggalign_free_border <- function(plot, borders = "tlbr") {
    attr(plot, "ggalign_free_borders") <- union_position(
        attr(plot, "ggalign_free_borders", exact = TRUE), borders
    )
    plot
}

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom gtable is.gtable
#' @export
alignpatch.ggalign_free_border <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeBorder", Parent,
        borders = split_position(attr(x, "ggalign_free_borders", exact = TRUE)),
        align_border = function(self, gt, t = NULL, l = NULL,
                                b = NULL, r = NULL) {
            if (is.gtable(gt)) {
                for (border in self$borders) {
                    # Adjust the gtable margins to match the aligned border
                    # sizes
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
