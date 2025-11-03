#' @inheritParams grid::viewport
#' @inheritDotParams grid::viewport -x -y -width -height
#' @param resize A logical value. If `TRUE`, the viewport will be resized to
#'   accommodate the underlying gtable. This only applies when both the viewport
#'   width/height and the underlying gtable widths/heights are specified using
#'   absolute units. If `TRUE` and the viewport width/height is `NA`, the
#'   width/height will be set to match the gtable's widths/heights. Otherwise,
#'   the width/height will be set to `unit(1, "npc")`. See
#'   [`absolute.size()`][grid::absolute.size] for absolute unit.
#' @return
#' - `free_vp`: A modified version of `plot` with a `ggalign_free_vp` class.
#' @export
#' @rdname free
free_vp <- function(plot, x = 0.5, y = 0.5, width = NA, height = NA, ...,
                    resize = TRUE) {
    UseMethod("free_vp")
}

#' @importFrom grid viewport
#' @export
free_vp.default <- function(plot, x = 0.5, y = 0.5,
                            width = NA, height = NA, ...,
                            resize = TRUE) {
    vp <- viewport(x = x, y = y, width = width, height = height, ..., )
    attr(plot, "ggalign_free_vp") <- list(vp = vp, resize = resize)
    add_class(plot, "ggalign_free_vp")
}

#' @importFrom grid viewport
#' @export
free_vp.ggalign_free_vp <- function(plot, x = 0.5, y = 0.5,
                                    width = NA, height = NA, ...,
                                    resize = TRUE) {
    vp <- viewport(x = x, y = y, width = width, height = height, ..., )
    attr(plot, "ggalign_free_vp") <- list(vp = vp, resize = resize)
    plot
}

####################################################
#' @importFrom gtable gtable_width gtable_height
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid unit editGrob
#' @export
patch.ggalign_free_vp <- function(x) {
    Parent <- NextMethod()
    viewport <- attr(x, "ggalign_free_vp", exact = TRUE)
    ggproto(
        "PatchFreeViewport", Parent,
        place = function(self, gtable, gt, t, l, b, r, i, bg_z, plot_z) {
            if (is.grob(gt)) {
                vp <- viewport$vp
                widths <- .subset2(gt, "widths")
                heights <- .subset2(gt, "heights")
                if (isTRUE(viewport$resize)) {
                    if (is.na(as.numeric(vp$width))) {
                        # we guess the width from the gtable
                        if (all(is_absolute_unit(widths))) {
                            vp$width <- sum(widths)
                        } else {
                            vp$width <- unit(1, "npc")
                        }
                    } else if (is_absolute_unit(vp$width)) {
                        vp$width <- max(vp$width, sum(widths))
                    }
                    if (is.na(as.numeric(vp$height))) {
                        # we guess the height from the gtable
                        if (all(is_absolute_unit(heights))) {
                            vp$height <- sum(heights)
                        } else {
                            vp$height <- unit(1, "npc")
                        }
                    } else if (is_absolute_unit(vp$height)) {
                        vp$height <- max(vp$height, sum(heights))
                    }
                } else {
                    if (is.na(as.numeric(vp$width))) {
                        vp$width <- unit(1, "npc")
                    }
                    if (is.na(as.numeric(vp$height))) {
                        vp$height <- unit(1, "npc")
                    }
                }
                gt <- editGrob(gt, vp = vp)
            }
            ggproto_parent(Parent, self)$place(
                gtable, gt, t, l, b, r, i, bg_z, plot_z
            )
        }
    )
}
