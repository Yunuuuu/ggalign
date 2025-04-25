#' Wrap Arbitrary Graphics to ggplot
#'
#' The `ggwrap()` function allows non-ggplot2 elements to be converted into a
#' compliant representation for use with [`align_plots()`]. This is useful for
#' adding any graphics that can be converted into a [`grob`][grid::grob] with
#' the [`patch()`] method.
#'
#' @param plot Any graphic that can be converted into a [`grob`][grid::grob]
#' using [`patch()`].
#' @param ... Additional arguments passed to the [`patch()`] method.
#' @param align A string specifying the area to place the plot: `"full"` for the
#' full area, `"plot"` for the full plotting area (including the axis label), or
#' `"panel"` for only the actual area where data is drawn.
#' @param clip A single boolean value indicating whether the grob should be
#' clipped if they expand outside their designated area.
#' @param on_top A single boolean value indicates whether the graphic plot
#' should be put frontmost. Note: the graphic plot will always put above the
#' background.
#' @param vp A [`viewport`][grid::viewport] object, you can use this to define
#' the plot area.
#' @return A `wrapped_plot` object that can be directly placed into
#' [`align_plots()`].
#' @inherit patch seealso
#' @examples
#' library(grid)
#' ggwrap(rectGrob(gp = gpar(fill = "goldenrod")), align = "full") +
#'     inset(rectGrob(gp = gpar(fill = "steelblue")), align = "panel") +
#'     inset(textGrob("Here are some text", gp = gpar(color = "black")),
#'         align = "panel"
#'     )
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp)) +
#'     ggtitle("Plot 1")
#' align_plots(p1, ggwrap(
#'     ~ plot(mtcars$mpg, mtcars$disp),
#'     mar = c(0, 2, 0, 0), bg = NA
#' ))
#'
#' @importFrom ggplot2 theme element_blank ggplot
#' @importFrom grid is.grob
#' @export
ggwrap <- function(plot, ..., align = "panel", on_top = FALSE,
                   clip = TRUE, vp = NULL) {
    patch <- ggplot() +
        theme(
            plot.background = element_blank(),
            panel.background = element_blank()
        )
    inset <- make_inset(
        plot = plot, ..., align = align, on_top = on_top,
        clip = clip, vp = vp
    )
    make_wrap(patch, inset)
}

make_wrap <- function(patch, inset) UseMethod("make_wrap")

make_wrapped_plot <- function(patch, inset) {
    if (.subset2(inset, "on_top")) {
        patch$ggalign_wrapped_insets_above <- c(
            patch$ggalign_wrapped_insets_above, list(inset)
        )
    } else {
        patch$ggalign_wrapped_insets_under <- c(
            patch$ggalign_wrapped_insets_under, list(inset)
        )
    }
    add_class(patch, "wrapped_plot")
}

#' @export
make_wrap.ggplot <- function(patch, inset) {
    patch <- add_class(patch, "patch_ggplot")
    make_wrap(patch, inset)
}

#' @export
make_wrap.patch_ggplot <- make_wrapped_plot

#' @export
make_wrap.alignpatches <- make_wrapped_plot

#################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.wrapped_plot <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchWrapped", Parent,
        ggalign_wrapped_insets_under = x$ggalign_wrapped_insets_under,
        ggalign_wrapped_insets_above = x$ggalign_wrapped_insets_above,
        patch_gtable = function(self, theme, guides, plot = Parent$plot) {
            ans <- ggproto_parent(Parent, self)$patch_gtable(
                theme = theme, guides = guides, plot = plot
            )
            ans <- add_wrapped_insets(
                ans, self$ggalign_wrapped_insets_under, FALSE
            )
            add_wrapped_insets(ans, self$ggalign_wrapped_insets_above, TRUE)
        }
    )
}

# For wrapped plot -------------------
#' @export
alignpatch.grob <- function(x) alignpatch(ggwrap(x))

#' @export
alignpatch.gList <- alignpatch.grob

#' @export
alignpatch.formula <- alignpatch.grob

#' @export
alignpatch.function <- alignpatch.grob

#' @export
alignpatch.recordedplot <- alignpatch.grob

#' @export
alignpatch.trellis <- alignpatch.grob

#' @export
alignpatch.Heatmap <- function(x) alignpatch(ggwrap(x, align = "full"))

#' @export
alignpatch.HeatmapList <- alignpatch.Heatmap

#' @export
alignpatch.HeatmapAnnotation <- alignpatch.Heatmap

#' @export
alignpatch.pheatmap <- function(x) alignpatch(ggwrap(x, align = "full"))

################################################## 3
add_wrapped_insets <- function(gt, insets, on_top) {
    if (is.null(insets)) return(gt) # styler: off
    for (i in seq_along(insets)) {
        gt <- add_wrapped_inset(gt, .subset2(insets, i), on_top, i)
    }
    gt
}

#' @importFrom gtable gtable is.gtable gtable_add_grob
add_wrapped_inset <- function(gt, inset, on_top, i) {
    align <- .subset2(inset, "align")
    clip <- .subset2(inset, "clip")
    layout <- .subset2(gt, "layout")
    grob <- .subset2(inset, "grob")
    if (on_top) {
        z <- Inf
    } else {
        background <- .subset2(layout, "name") == "background"
        z <- .subset2(layout, "z")[background] + 1L
        gt$layout$z[layout$z >= z] <- layout$z[layout$z >= z] + 1L
    }

    # add the grob to the gtable
    if (align == "full") {
        gt <- gtable_add_grob(gt,
            list(grob), 1L, 1L, nrow(gt), ncol(gt),
            clip = clip, name = sprintf("wrap-full-%d", i), z = z
        )
    } else {
        panel_loc <- find_panel(gt)
        gt <- switch(align,
            plot = gtable_add_grob(gt,
                list(grob),
                .subset2(panel_loc, "t") - 3L,
                .subset2(panel_loc, "l") - 3L,
                .subset2(panel_loc, "b") + 3L,
                .subset2(panel_loc, "r") + 3L,
                clip = clip, name = sprintf("wrap-plot-%d", i), z = z
            ),
            panel = gtable_add_grob(gt,
                list(grob),
                .subset2(panel_loc, "t"),
                .subset2(panel_loc, "l"),
                .subset2(panel_loc, "b"),
                .subset2(panel_loc, "r"),
                clip = clip, name = sprintf("wrap-panel-%d", i), z = z
            )
        )
    }
    gt
}
