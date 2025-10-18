#' @inheritParams grid::viewport
#' @inheritDotParams grid::viewport -x -y -width -height
#' @return
#' - `free_vp`: A modified version of `plot` with a `ggalign_free_vp` class.
#' @export
#' @rdname free
free_vp <- function(plot, x = 0.5, y = 0.5, width = NA, height = NA, ...) {
    UseMethod("free_vp")
}

#' @importFrom grid viewport
#' @export
free_vp.default <- function(plot, x = 0.5, y = 0.5,
                            width = NA, height = NA, ...) {
    attr(plot, "ggalign_free_vp") <- viewport(
        x = x, y = y, width = width, height = height, ...,
    )
    add_class(plot, "ggalign_free_vp")
}

#' @importFrom grid viewport
#' @export
free_vp.ggalign_free_vp <- function(plot, x = 0.5, y = 0.5,
                                    width = NA, height = NA, ...) {
    attr(plot, "ggalign_free_vp") <- viewport(
        x = x, y = y, width = width, height = height, ...,
    )
    plot
}

####################################################
#' @importFrom gtable gtable_width gtable_height
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid unit editGrob
#' @export
patch.ggalign_free_vp <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeViewport", Parent,
        vp = attr(x, "ggalign_free_vp", exact = TRUE),
        place = function(self, gtable, gt, t, l, b, r, i, bg_z, plot_z) {
            if (is.grob(gt)) {
                vp <- self$vp

                if (!any(is_null_unit(widths <- .subset2(gt, "widths")))) {
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
                if (!any(is_null_unit(heights <- .subset2(gt, "heights")))) {
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
                if (horizontal_just || vertical_just) {
                    gt <- editGrob(gt, vp = vp)
                }
            }
            ggproto_parent(Parent, self)$place(
                gtable, gt, t, l, b, r, i, bg_z, plot_z
            )
        }
    )
}
