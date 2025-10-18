#' @param spaces Which border spaces should be removed? A string containing one
#' or more of `r oxford_and(.tlbr)`.
#' @return
#' - `free_space`: A modified version of `plot` with a `ggalign_free_space`
#'   class.
#' @export
#' @rdname free
free_space <- function(plot, spaces = "tlbr") {
    assert_position(spaces)
    UseMethod("free_space")
}

#' @export
free_space.default <- function(plot, spaces = "tlbr") {
    attr(plot, "ggalign_free_spaces") <- spaces
    add_class(plot, "ggalign_free_space")
}

#' @export
free_space.ggalign_free_align <- function(plot, spaces = "tlbr") {
    spaces <- setdiff_position(
        spaces,
        attr(plot, "ggalign_free_axes", exact = TRUE)
    )
    if (!nzchar(spaces)) {
        return(plot)
    }
    NextMethod()
}

#' @export
free_space.ggalign_free_space <- function(plot, spaces = "tlbr") {
    attr(plot, "ggalign_free_spaces") <- union_position(
        attr(plot, "ggalign_free_spaces", exact = TRUE), spaces
    )
    plot
}

##########################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid unit
#' @export
alignpatch.ggalign_free_space <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeSpace", Parent,
        spaces = split_position(attr(x, "ggalign_free_spaces", exact = TRUE)),
        get_sizes = function(self, gt, free = NULL) {
            ggproto_parent(Parent, self)$get_sizes(gt, union(free, self$spaces))
        }
    )
}
