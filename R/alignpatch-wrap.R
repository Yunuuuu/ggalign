#' Wrap Arbitrary Graphics for Alignment
#'
#' The `wrap()` function allows non-ggplot2 elements to be converted into a
#' compliant representation for use with [align_plots()]. This is useful for
#' adding any graphics that can be converted into a [grob][grid::grob] with the
#' [patch()] method.
#'
#' @param plot Any graphic that can be converted into a [grob][grid::grob] using
#' [patch()].
#' @param ... Additional arguments passed to the [patch()] method.
#' @param align A string specifying the area to place the plot: `"full"` for the
#' full area, `"plot"` for the full plotting area (including the axis label), or
#' `"panel"` for only the actual area where data is drawn.
#' @param clip A single boolean value indicating whether the grob should be
#' clipped if they expand outside their designated area.
#' @param on_top A single boolean value indicates whether the graphic plot
#' should be put frontmost. Note: the graphic plot will always put above the
#' background.
#' @param vp A [viewport][grid::viewport] object, you can use this to define the
#' plot area.
#' @return A `wrapped_plot` object that can be directly placed into
#' [align_plots()].
#' @importFrom ggplot2 ggplot theme element_blank
#' @importFrom grid is.grob
#' @export
wrap <- function(plot, ..., align = "panel", on_top = TRUE,
                 clip = TRUE, vp = NULL) {
    patch <- ggplot() +
        theme(
            plot.background = element_blank(),
            panel.background = element_blank()
        )
    grob <- make_inset(
        plot = plot, ..., align = align, on_top = on_top,
        clip = clip, vp = vp
    )
    make_wrap(patch, grob)
}

make_wrap <- function(patch, grob) UseMethod("make_wrap")

#' @export
make_wrap.ggplot <- function(patch, grob) {
    add_class(make_wrap.wrapped_plot(patch, grob), "wrapped_plot")
}

#' @export
make_wrap.wrapped_plot <- function(patch, grob) {
    if (attr(grob, "on_top")) {
        patch$wrapped_grobs_above <- c(
            list(grob), .subset2(patch, "wrapped_grobs_above")
        )
    } else {
        patch$wrapped_grobs_under <- c(
            .subset2(patch, "wrapped_grobs_under"), list(grob)
        )
    }
    patch
}

#' Convert Object into a Grob for Wrapping
#'
#' The `patch()` function is used by [wrap()] to convert objects into a
#' [grob][grid::grob], enabling their alignment within [align_plots()].
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @param ... Additional arguments passed to specific methods.
#' @return A [grob][grid::grob] object.
#' @export
#' @keywords internal
patch <- function(x, ...) UseMethod("patch")

#' @export
#' @rdname patch
patch.grob <- function(x, ...) x

#' @export
#' @rdname patch
patch.ggplot <- function(x, ...) patch_gtable(x)

#' @inheritParams gridGraphics::echoGrob
#' @export
#' @rdname patch
patch.formula <- function(x, ..., device = NULL) {
    rlang::check_installed("gridGraphics", "to add base plots")
    gp <- graphics::par(no.readonly = TRUE)
    force(x)
    plot_call <- function() {
        old_gp <- graphics::par(no.readonly = TRUE)
        graphics::par(gp)
        on.exit(try(graphics::par(old_gp)))
        suppressMessages(eval(x[[2]], attr(x, ".Environment")))
        invisible(NULL)
    }
    gridGraphics::echoGrob(plot_call,
        name = "base_plot",
        device = device %||% offscreen
    )
}

offscreen <- function(width, height) {
    grDevices::pdf(NULL, width = width, height = height)
    grDevices::dev.control("enable")
}

#' @importFrom utils getFromNamespace
#' @export
#' @rdname patch
patch.Heatmap <- function(x, ..., device = NULL) {
    draw <- getFromNamespace("draw", "ComplexHeatmap")
    grid::grid.grabExpr(expr = draw(x, ...), device = device %||% offscreen)
}

#' @export
#' @rdname patch
patch.HeatmapList <- patch.Heatmap

#' @export
#' @rdname patch
patch.HeatmapAnnotation <- patch.HeatmapList

#################################################
#' @export
#' @rdname alignpatch
#' @order 5
patch_gtable.wrapped_plot <- function(patch, guides) {
    ans <- NextMethod() # will call ggplot2 method
    ans <- add_wrapped_grobs(
        ans, .subset2(patch, "wrapped_grobs_under"), FALSE
    )
    ans <- add_wrapped_grobs(
        ans, .subset2(patch, "wrapped_grobs_above"), TRUE
    )
    add_class(ans, "gtable_wrapped_plot")
}

add_wrapped_grobs <- function(gt, grobs, under) {
    if (is.null(grobs)) return(gt) # styler: off
    for (grob in grobs) gt <- add_wrapped_grob(gt, grob, under)
    gt
}

#' @importFrom gtable gtable is.gtable gtable_add_grob
add_wrapped_grob <- function(gt, grob, on_top) {
    align <- attr(grob, "align")
    clip <- attr(grob, "clip")
    layout <- .subset2(gt, "layout")
    panels <- layout[grep("^panel", .subset2(layout, "name")), , drop = FALSE]
    if (on_top) {
        z <- Inf
    } else {
        background <- .subset2(layout, "name") == "background"
        z <- .subset2(layout, "z")[background] + 1L
        gt$layout$z[layout$z >= z] <- layout$z[layout$z >= z] + 1L
    }

    # if we set the viewport, the grob must be a gtable object
    if (!is.null(vp <- attr(grob, "vp"))) {
        if (is.gtable(grob)) {
            grob$vp <- vp
        } else {
            container <- gtable(unit(1L, "null"), unit(1L, "null"), vp = vp)
            grob <- gtable_add_grob(container, list(grob), 1L, 1L, clip = FALSE)
        }
    }
    # add the grob to the gtable
    if (align == "full") {
        gt <- gtable_add_grob(gt,
            list(grob), 1L, 1L, nrow(gt), ncol(gt),
            clip = clip, name = "wrap_full", z = z
        )
    } else {
        panel_loc <- list(
            t = min(.subset2(panels, "t")),
            l = min(.subset2(panels, "l")),
            b = max(.subset2(panels, "b")),
            r = max(.subset2(panels, "r"))
        )
        gt <- switch(align,
            plot = gtable_add_grob(gt,
                list(grob),
                .subset2(panel_loc, "t") - 3L,
                .subset2(panel_loc, "l") - 3L,
                .subset2(panel_loc, "b") + 3L,
                .subset2(panel_loc, "r") + 3L,
                clip = clip, name = "wrap_plot", z = z
            ),
            panel = gtable_add_grob(gt,
                list(grob),
                .subset2(panel_loc, "t"),
                .subset2(panel_loc, "l"),
                .subset2(panel_loc, "b"),
                .subset2(panel_loc, "r"),
                clip = clip, name = "wrap_panel", z = z
            )
        )
    }
    gt
}

#' @export
print.wrapped_plot <- print.alignpatches

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.wrapped_plot <- grid.draw.alignpatches
