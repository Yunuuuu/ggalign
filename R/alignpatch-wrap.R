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
    inset <- inset(
        plot = plot, ...,
        align = align, on_top = on_top,
        clip = clip, vp = vp
    )
    update_ggplot(inset, patch)
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
