#' @param borders Which border shouldn't be aligned? A string containing
#' one or more of `r rd_values(.tlbr)`.
#' @return
#' - `free_border`: A modified version of `plot` with a `free_border` class.
#' @export
#' @rdname free
free_border <- function(plot, borders = "tlbr") {
    UseMethod("free_border")
}

#' @export
free_border.ggplot <- function(plot, borders = "tlbr") {
    assert_position(borders)
    attr(plot, "free_borders") <- borders
    add_class(plot, "free_border")
}

#' @export
free_border.alignpatches <- free_border.ggplot

#' @export
free_border.free_align <- function(plot, borders = "tlbr") {
    assert_position(borders)
    borders <- setdiff_position(borders, attr(plot, "free_axes"))
    if (nchar(borders) == 0L) {
        return(plot)
    }
    NextMethod()
}

#' @export
free_border.free_lab <- function(plot, borders = "tlbr") {
    assert_position(borders)
    free_labs <- setdiff_position(attr(plot, "free_labs"), borders)
    if (nchar(free_labs) == 0L) {
        attr(plot, "free_labs") <- NULL
        class(plot) <- setdiff(class(plot), "free_lab")
    } else {
        attr(plot, "free_labs") <- free_labs
    }
    NextMethod()
}

#' @export
free_border.free_border <- function(plot, borders = "tlbr") {
    assert_position(borders)
    attr(plot, "free_borders") <- union_position(
        attr(plot, "free_borders"), borders
    )
    plot
}

#' @export
free_border.default <- function(plot, borders = "tlbr") {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_border.wrapped_plot <- free_border.default

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_border <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeBorder", Parent,
        free_borders = setup_position(attr(x, "free_borders")),
        patch_gtable = function(self, guides, plot = self$plot) {
            ans <- ggproto_parent(Parent, self)$patch_gtable(
                guides = guides, plot = plot
            )
            ggproto_parent(Parent, self)$free_border(
                guides = guides, borders = self$free_borders, gt = ans
            )
        },
        free_border = function(self, guides, borders, gt = self$gt) {
            if (length(borders <- setdiff(borders, self$free_borders))) {
                ggproto_parent(Parent, self)$free_border(
                    guides = guides, borders = borders, gt = gt
                )
            } else {
                gt
            }
        }
    )
}
