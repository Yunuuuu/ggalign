#' Wrap Arbitrary Graphics to ggplot
#'
#' The `ggwrap()` function allows non-ggplot2 elements to be converted into a
#' compliant representation for use with [`align_plots()`]. This is useful for
#' adding any graphics that can be converted into a [`grob`][grid::grob] with
#' the [`as_grob()`] method.
#'
#' @inheritParams inset
#' @return A `wrapped_plot` object that can be directly placed into
#' [`align_plots()`].
#' @inherit as_grob seealso
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
#' @export
ggwrap <- function(plot, ..., align = "panel", on_top = FALSE,
                   clip = TRUE, vp = NULL) {
    patch <- ggplot() +
        theme(
            plot.background = element_blank(),
            panel.background = element_blank()
        )
    patch_inset <- inset(
        plot = plot, ...,
        align = align, on_top = on_top,
        clip = clip, vp = vp
    )
    make_wrap(patch, patch_inset)
}

make_wrap <- function(patch, patch_inset) UseMethod("make_wrap")

make_wrapped_plot <- function(patch, patch_inset) {
    if (prop(patch_inset, "on_top")) {
        attr(patch, "ggalign_wrapped_insets_above") <- c(
            attr(patch, "ggalign_wrapped_insets_above", exact = TRUE),
            list(patch_inset)
        )
    } else {
        attr(patch, "ggalign_wrapped_insets_under") <- c(
            attr(patch, "ggalign_wrapped_insets_under", exact = TRUE),
            list(patch_inset)
        )
    }
    add_class(patch, "wrapped_plot")
}

S7::method(make_wrap, ggplot2::class_ggplot) <- function(patch, patch_inset) {
    patch <- add_class(patch, "patch_ggplot")
    make_wrap(patch, patch_inset)
}

#' @export
make_wrap.patch_ggplot <- make_wrapped_plot

S7::method(make_wrap, alignpatches) <- make_wrapped_plot

#################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
patch.wrapped_plot <- function(x) {
    under <- attr(x, "ggalign_wrapped_insets_under", exact = TRUE)
    attr(x, "ggalign_wrapped_insets_under") <- NULL
    above <- attr(x, "ggalign_wrapped_insets_above", exact = TRUE)
    attr(x, "ggalign_wrapped_insets_above") <- NULL
    Parent <- NextMethod()
    ggproto(
        "PatchWrapped", Parent,
        under = under,
        above = above,
        gtable = function(self, theme = NULL, guides = NULL,
                          tagger = NULL) {
            ans <- ggproto_parent(Parent, self)$gtable(theme, guides, tagger)
            ans <- add_wrapped_insets(ans, self$under, FALSE)
            add_wrapped_insets(ans, self$above, TRUE)
        }
    )
}

# For wrapped plot -------------------
#' @export
patch.grob <- function(x) patch(ggwrap(x))

#' @export
patch.gList <- patch.grob

#' @export
patch.formula <- patch.grob

#' @export
patch.function <- patch.grob

#' @export
patch.recordedplot <- patch.grob

#' @export
patch.trellis <- patch.grob

#' @export
patch.Heatmap <- function(x) patch(ggwrap(x, align = "full"))

#' @export
patch.HeatmapList <- patch.Heatmap

#' @export
patch.HeatmapAnnotation <- patch.Heatmap

#' @export
patch.pheatmap <- function(x) patch(ggwrap(x, align = "full"))

################################################## 3
add_wrapped_insets <- function(gt, insets, on_top) {
    if (is.null(insets)) return(gt) # styler: off
    for (i in seq_along(insets)) {
        gt <- add_wrapped_inset(gt, .subset2(insets, i), on_top, i)
    }
    gt
}

#' @importFrom S7 prop
#' @importFrom grid editGrob
#' @importFrom gtable gtable is.gtable gtable_add_grob
add_wrapped_inset <- function(gt, patch_inset, on_top, i) {
    align <- prop(patch_inset, "align")
    clip <- prop(patch_inset, "clip")
    layout <- .subset2(gt, "layout")
    grob <- prop(patch_inset, "grob")
    if (!is.null(vp <- prop(patch_inset, "vp"))) grob <- editGrob(grob, vp = vp)
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
