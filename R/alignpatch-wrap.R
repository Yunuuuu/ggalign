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
#' @inherit patch seealso
#' @examples
#' library(grid)
#' wrap(rectGrob(gp = gpar(fill = "goldenrod")), align = "full") +
#'     inset(rectGrob(gp = gpar(fill = "steelblue")), align = "panel") +
#'     inset(textGrob("Here are some text", gp = gpar(color = "black")),
#'         align = "panel"
#'     )
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
    if (!inherits(patch, "patch_ggplot")) {
        patch <- add_class(patch, "patch_ggplot")
    }
    patch <- add_class(patch, "wrapped_plot")
    make_wrap(patch, grob)
}

#' @export
make_wrap.wrapped_plot <- function(patch, grob) {
    if (attr(grob, "on_top")) {
        attr(patch, "wrapped_grobs_above") <- c(
            attr(patch, "wrapped_grobs_above"), list(grob)
        )
    } else {
        attr(patch, "wrapped_grobs_under") <- c(
            attr(patch, "wrapped_grobs_under"), list(grob)
        )
    }
    patch
}

#' Convert Object into a Grob
#'
#' The `patch()` function is used by [wrap()] to convert objects into a
#' [grob][grid::grob], enabling their alignment within [align_plots()].
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @param ... Additional arguments passed to specific methods.
#' @return A [grob][grid::grob] object.
#' @seealso
#' - [patch.grob] / [patch.gList]
#' - [patch.ggplot]
#' - [patch.patch_ggplot]
#' - [patch.patchwork]
#' - [patch.patch]
#' - [patch.trellis]
#' - [patch.formula] / [patch.function]
#' - [patch.recordedplot]
#' - [patch.Heatmap]
#' - [patch.HeatmapList]
#' - [patch.HeatmapAnnotation]
#' - [patch.pheatmap]
#' @export
#' @keywords internal
patch <- function(x, ...) UseMethod("patch")

# Following methods much are copied from `cowplot` or `ggplotify`
#' @export
patch.default <- function(x, ...) {
    cli::cli_abort("Cannot make grob from {.obj_type_friendly {x}}")
}

#' @inherit patch title description return
#' @inheritParams patch
#' @param ... Not used currently.
#' @export
patch.grob <- function(x, ...) {
    rlang::check_dots_empty()
    x
}

#' @export
#' @rdname patch.grob
patch.gList <- function(x, ...) {
    rlang::check_dots_empty()
    # gLists need to be wrapped in a grob tree
    grid::grobTree(plot)
}

#' @inherit patch.grob
#' @seealso [ggplot][ggplot2::ggplot]
#' @export
patch.ggplot <- function(x, ...) {
    rlang::check_dots_empty()
    ggplot2::ggplotGrob(x)
}

#' @inherit patch.grob
#' @seealso
#' - [patch_titles]
#' - [inset]
#' - [wrap]
#' @export
patch.patch_ggplot <- function(x, ...) {
    rlang::check_dots_empty()
    ggalignGrob(x)
}

#' @inherit patch.grob
#' @seealso [alignpatches][align_plots]
#' @export
patch.alignpatches <- function(x, ...) {
    rlang::check_dots_empty()
    ggalignGrob(x)
}

#' @inherit patch.grob
#' @seealso [patchwork][patchwork::patchworkGrob]
#' @export
patch.patchwork <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patchwork")
    rlang::check_dots_empty()
    patchwork::patchworkGrob(x)
}

#' @inherit patch.grob
#' @seealso [patch][patchwork::patchGrob]
#' @export
patch.patch <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patch")
    rlang::check_dots_empty()
    patchwork::patchGrob(x)
}

#' @inherit patch.grob
#' @inheritDotParams graphics::par -no.readonly
#' @inheritParams gridGraphics::echoGrob
#' @seealso [plot]
#' @export
patch.formula <- function(x, ..., device = NULL, name = NULL) {
    rlang::check_installed("gridGraphics", "to make grob from base plot")
    gp <- graphics::par(..., no.readonly = TRUE)
    gridGraphics::echoGrob(
        function() {
            old_gp <- graphics::par(no.readonly = TRUE)
            graphics::par(gp)
            on.exit(try(graphics::par(old_gp)))
            suppressMessages(eval(x[[2]], attr(x, ".Environment")))
            invisible(NULL)
        },
        name = name,
        device = device %||% offscreen
    )
}

#' @export
#' @rdname patch.formula
patch.function <- function(x, ..., device = NULL, name = NULL) {
    rlang::check_installed("gridGraphics", "to make grob from base plot")
    gp <- graphics::par(..., no.readonly = TRUE)
    gridGraphics::echoGrob(
        function() {
            old_gp <- graphics::par(no.readonly = TRUE)
            graphics::par(gp)
            on.exit(try(graphics::par(old_gp)))
            suppressMessages(x())
            invisible(NULL)
        },
        name = name,
        device = device %||% offscreen
    )
}

#' @inherit patch.grob
#' @inheritParams gridGraphics::echoGrob
#' @seealso [recordPlot][grDevices::recordPlot]
#' @export
patch.recordedplot <- function(x, ..., device = NULL) {
    rlang::check_installed("gridGraphics", "to make grob from recordedplot")
    rlang::check_dots_empty()
    gridGraphics::echoGrob(x, device = device %||% offscreen)
}

offscreen <- function(width, height) {
    grDevices::pdf(NULL, width = width, height = height)
    grDevices::dev.control("enable")
}

#' @inherit patch.grob
#' @inheritDotParams grid::grid.grabExpr -expr -device
#' @inheritParams grid::grid.grabExpr
#' @seealso [trellis][lattice::trellis.object]
#' @export
patch.trellis <- function(x, ..., device = NULL) {
    grid::grid.grabExpr(expr = print(x), ..., device = device %||% offscreen)
}

#' @inherit patch.grob
#' @param ... Additional arguments passed to [draw()][ComplexHeatmap::draw].
#' @inheritParams grid::grid.grabExpr
#' @seealso
#'  - [Heatmap][ComplexHeatmap::Heatmap]
#'  - [HeatmapAnnotation][ComplexHeatmap::HeatmapAnnotation]
#' @importFrom utils getFromNamespace
#' @export
patch.Heatmap <- function(x, ..., device = NULL) {
    rlang::check_installed(
        "ComplexHeatmap",
        sprintf("to make grob from {%s} plot", obj_type_friendly(x))
    )
    draw <- getFromNamespace("draw", "ComplexHeatmap")
    grid::grid.grabExpr(expr = draw(x, ...), device = device %||% offscreen)
}

#' @export
#' @rdname patch.Heatmap
patch.HeatmapList <- patch.Heatmap

#' @export
#' @rdname patch.Heatmap
patch.HeatmapAnnotation <- patch.HeatmapList

#' @inherit patch.grob
#' @seealso [pheatmap][pheatmap::pheatmap]
#' @export
#' @rdname patch.pheatmap
patch.pheatmap <- function(x, ...) {
    .subset2(x, "gtable")
}

#################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.wrapped_plot <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchWrapped", Parent,
        wrapped_grobs_under = attr(x, "wrapped_grobs_under"),
        wrapped_grobs_above = attr(x, "wrapped_grobs_above"),
        patch_gtable = function(self, guides, plot = Parent$plot) {
            ans <- ggproto_parent(Parent, self)$patch_gtable(
                guides = guides, plot = plot
            )
            ans <- add_wrapped_grobs(ans, self$wrapped_grobs_under, FALSE)
            add_wrapped_grobs(ans, self$wrapped_grobs_above, TRUE)
        }
    )
}

# For wrapped plot -------------------
#' @export
alignpatch.grob <- function(x) alignpatch(wrap(x))

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
alignpatch.Heatmap <- function(x) alignpatch(wrap(x, align = "full"))

#' @export
alignpatch.HeatmapList <- alignpatch.Heatmap

#' @export
alignpatch.HeatmapAnnotation <- alignpatch.Heatmap

#' @export
alignpatch.pheatmap <- function(x) alignpatch(wrap(x, align = "full"))

################################################## 3
add_wrapped_grobs <- function(gt, grobs, on_top) {
    if (is.null(grobs)) return(gt) # styler: off
    for (i in seq_along(grobs)) {
        gt <- add_wrapped_grob(gt, .subset2(grobs, i), on_top, i)
    }
    gt
}

#' @importFrom gtable gtable is.gtable gtable_add_grob
add_wrapped_grob <- function(gt, grob, on_top, i) {
    align <- attr(grob, "align")
    clip <- attr(grob, "clip")
    layout <- .subset2(gt, "layout")
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
            clip = clip, name = sprintf("wrap-full-%d", i), z = z
        )
    } else {
        panels <- layout[
            grep("^panel", .subset2(layout, "name")), ,
            drop = FALSE
        ]
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
