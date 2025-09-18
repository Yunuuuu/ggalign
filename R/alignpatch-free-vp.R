#' @inheritParams grid::viewport
#' @inheritDotParams grid::viewport -x -y -width -height
#' @return
#' - `free_vp`: A modified version of `plot` with a `free_vp` class.
#' @importFrom grid viewport
#' @export
#' @rdname free
free_vp <- function(plot, x = 0.5, y = 0.5, width = NA, height = NA, ...) {
    UseMethod("free_vp")
}

#' @export
free_vp.default <- function(plot, x = 0.5, y = 0.5,
                            width = NA, height = NA, ...) {
    cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_vp.ggplot <- function(plot, x = 0.5, y = 0.5,
                           width = NA, height = NA, ...) {
    attr(plot, "vp") <- viewport(
        x = x, y = y, width = width, height = height, ...,
    )
    add_class(plot, "free_vp")
}

#' @export
`free_vp.ggalign::alignpatches` <- free_vp.ggplot

####################################################
#' @importFrom gtable gtable_width gtable_height
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_vp <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeViewport", Parent,
        vp = attr(x, "vp"),
        align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                                gt = self$gt) {
            ans <- ggproto_parent(Parent, self)$align_border(
                t = t, l = l, b = b, r = r, gt = gt
            )
            vp <- self$vp

            if (!any(is_null_unit(widths <- .subset2(ans, "widths")))) {
                horizontal_just <- TRUE
                vp$width <- sum(widths)
            } else if (!is.na(as.numeric(vp$width))) {
                # we guess the width from the gtable
                horizontal_just <- TRUE
                vp$width <- max(vp$width, sum(widths))
            } else {
                vp$width <- unit(1, "npc")
                horizontal_just <- FALSE
            }
            if (!any(is_null_unit(heights <- .subset2(ans, "heights")))) {
                vertical_just <- TRUE
                vp$height <- sum(heights)
            } else if (!is.na(as.numeric(vp$height))) {
                # we guess the height from the gtable
                vertical_just <- TRUE
                vp$height <- max(vp$height, sum(heights))
            } else {
                vp$height <- unit(1, "npc")
                vertical_just <- FALSE
            }
            if (horizontal_just || vertical_just) ans$vp <- vp
            ans
        }
    )
}
