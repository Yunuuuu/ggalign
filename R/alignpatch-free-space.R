#' @param spaces Which border spaces should be removed? A string containing one
#' or more of `r rd_values(.tlbr)`.
#' @return
#' - `free_space`: A modified version of `plot` with a `free_space` class.
#' @export
#' @rdname free
free_space <- function(plot, spaces = "tlbr") {
    UseMethod("free_space")
}

#' @export
free_space.default <- function(plot, spaces = "tlbr") {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_space.ggplot <- function(plot, spaces = "tlbr") {
    assert_position(spaces)
    attr(plot, "free_spaces") <- spaces
    add_class(plot, "free_space")
}

#' @export
free_space.alignpatches <- free_space.ggplot

#' @export
free_space.free_align <- function(plot, spaces = "tlbr") {
    assert_position(spaces)
    spaces <- setdiff_position(spaces, attr(plot, "free_axes"))
    if (nchar(spaces) == 0L) {
        return(plot)
    }
    NextMethod()
}

#' @export
free_space.free_space <- function(plot, spaces = "tlbr") {
    assert_position(spaces)
    attr(plot, "free_spaces") <- union_position(
        attr(plot, "free_spaces"), spaces
    )
    plot
}

##########################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid unit
#' @export
alignpatch.free_space <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeSpace", Parent,
        free_spaces = split_position(attr(x, "free_spaces")),
        get_sizes = function(self, free = NULL, gt = self$gt) {
            ggproto_parent(Parent, self)$get_sizes(
                union(free, self$free_spaces),
                gt = gt
            )
        }
    )
}
