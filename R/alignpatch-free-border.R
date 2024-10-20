#' @param borders Which border shouldn't be aligned? A string containing one or
#' more of `r rd_values(.tlbr)`.
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

################################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_border <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeBorder", Parent,
        free_borders = setup_pos(attr(x, "free_borders")),
        collect_guides = function(self, guides = self$guides, gt = self$gt) {
            ans <- ggproto_parent(Parent, self)$collect_guides(
                guides = guides, gt = gt
            )
            self$gt <- ggproto_parent(Parent, self)$free_border(
                borders = self$free_borders, gt = self$gt
            )
            ans
        },
        align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                                gt = self$gt) {
            gt <- ggproto_parent(Parent, self)$align_border(
                t = t, l = l, b = b, r = r, gt = gt
            )
            ggproto_parent(Parent, self)$align_free_border(
                borders = self$free_borders,
                t = t, l = l, b = b, r = r, gt = gt
            )
        },

        #' @importFrom vctrs vec_set_difference
        free_border = function(self, borders, gt = self$gt) {
            borders <- vec_set_difference(borders, self$free_borders)
            if (length(borders)) {
                gt <- ggproto_parent(Parent, self)$free_border(
                    borders = borders, gt = gt
                )
            }
            gt
        },
        align_free_border = function(self, borders,
                                     t = NULL, l = NULL, b = NULL, r = NULL,
                                     gt = self$gt) {
            borders <- vec_set_difference(borders, self$free_borders)
            if (length(borders)) {
                gt <- ggproto_parent(Parent, self)$align_free_border(
                    borders = borders,
                    t = t, l = l, b = b, r = r, gt = gt
                )
            }
            gt
        }
    )
}
