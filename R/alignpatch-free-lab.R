#' @param labs Which axis labs to be free? A string containing one or more of
#' `r rd_values(.tlbr)`.
#' @return
#' - `free_lab`: A modified version of `plot` with a `free_lab` class.
#' @export
#' @rdname free
free_lab <- function(plot, labs = "tlbr") {
    UseMethod("free_lab")
}

#' @export
free_lab.ggplot <- function(plot, labs = "tlbr") {
    assert_position(labs)
    attr(plot, "free_labs") <- labs
    add_class(plot, "free_lab")
}

#' @export
free_lab.alignpatches <- free_lab.ggplot

#' @export
free_lab.free_space <- free_lab.ggplot

#' @export
free_lab.free_align <- function(plot, labs = "tlbr") {
    assert_position(labs)
    labs <- setdiff_position(labs, attr(plot, "free_axes"))
    if (nchar(labs) == 0L) return(plot) # styler: off
    NextMethod()
}

#' @export
free_lab.free_borders <- function(plot, labs = "tlbr") {
    assert_position(labs)
    labs <- setdiff_position(labs, attr(plot, "free_borders"))
    if (nchar(labs) == 0L) return(plot) # styler: off
    NextMethod()
}

#' @export
free_lab.free_lab <- function(plot, labs = "tlbr") {
    assert_position(labs)
    attr(plot, "free_labs") <- union_position(attr(plot, "free_labs"), labs)
    plot
}

#' @export
free_lab.default <- function(plot, labs = "tlbr") {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_lab.wrapped_plot <- free_lab.default

####################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_lab <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeLab", Parent,
        free_labs = setup_position(attr(x, "free_labs")),
        patch_gtable = function(self, guides, plot = self$plot) {
            ans <- ggproto_parent(Parent, self)$patch_gtable(
                guides = guides, plot = plot
            )
            ggproto_parent(Parent, self)$free_lab(
                labs = self$free_labs, gt = ans
            )
        },
        free_lab = function(self, labs, gt = self$gt) {
            if (length(labs <- setdiff(labs, self$free_labs))) {
                ggproto_parent(Parent, self)$free_lab(
                    labs = labs, gt = gt
                )
            } else {
                gt
            }
        }
    )
}
