#' @return
#' - `free_space`: A modified version of `plot` with a `free_space` class.
#' @export
#' @rdname free
free_space <- function(plot, borders = "tlbr") {
    UseMethod("free_space")
}

#' @export
free_space.default <- function(plot, borders = "tlbr") {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_space.ggplot <- function(plot, borders = "tlbr") {
    assert_position(borders)
    attr(plot, "free_spaces") <- borders
    add_class(plot, "free_space")
}

#' @export
free_space.free_space <- function(plot, borders = "tlbr") {
    assert_position(borders)
    attr(plot, "free_spaces") <- union_position(
        attr(plot, "free_spaces"), borders
    )
    plot
}

#' @export
free_space.wrapped_plot <- free_space.default

##########################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid unit
#' @export
alignpatch.free_space <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeSpace", Parent,
        free_spaces = split_position(attr(x, "free_spaces")),
        widths = function(self, gt = self$gt) {
            ans <- ggproto_parent(Parent, self)$widths(gt = gt)
            free <- .subset(list(
                l = seq_len(LEFT_BORDER),
                r = seq(LEFT_BORDER + 2L, length.out = RIGHT_BORDER)
            ), self$free_spaces)
            if (length(free <- unlist(free, FALSE, FALSE))) {
                ans[free] <- unit(0, "mm")
            }
            ans
        },
        heights = function(self, gt = self$gt) {
            ans <- ggproto_parent(Parent, self)$heights(gt = gt)
            free <- .subset(list(
                t = seq_len(TOP_BORDER),
                b = seq(TOP_BORDER + 2L, length.out = BOTTOM_BORDER)
            ), self$free_spaces)
            if (length(free <- unlist(free, FALSE, FALSE))) {
                ans[free] <- unit(0, "mm")
            }
            ans
        }
    )
}
