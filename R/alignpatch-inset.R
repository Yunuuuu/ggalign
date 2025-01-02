#' Create a ggplot inset
#'
#' @inheritParams ggwrap
#' @return A `patch_inset` object, which can be added in ggplot.
#' @inherit patch seealso
#' @examples
#' library(grid)
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p1 + inset(p2, vp = viewport(0.6, 0.6,
#'     just = c(0, 0), width = 0.4, height = 0.4
#' ))
#' @export
inset <- function(plot, ..., align = "panel", on_top = TRUE,
                  clip = TRUE, vp = NULL) {
    make_inset(
        plot = plot, ..., align = align, on_top = on_top,
        clip = clip, vp = vp
    )
}

#' @importFrom grid editGrob
#' @importFrom rlang arg_match0
make_inset <- function(plot, ..., align, on_top, clip, vp,
                       call = caller_call()) {
    assert_bool(on_top, call = call)
    align <- arg_match0(align, c("panel", "plot", "full"), error_call = call)
    assert_bool(clip, call = call)
    assert_s3_class(vp, "viewport", allow_null = TRUE, call = call)
    if (!is.grob(grob <- patch(x = plot, ...))) {
        cli_abort("{.fn patch} must return a {.cls grob}", call = call)
    }
    if (!is.null(vp)) grob <- editGrob(grob, vp = vp)
    structure(
        list(
            grob = grob,
            align = align,
            clip = if (clip) "on" else "off",
            on_top = on_top
        ),
        class = "patch_inset"
    )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.patch_inset <- function(object, plot, object_name) {
    make_wrap(plot, object)
}

############################################################
#' Convert Object into a Grob
#'
#' The `patch()` function is used by [`ggwrap()`] and [inset()] to convert
#' objects into a [`grob`][grid::grob].
#'
#' @param x An object to be converted into a [`grob`][grid::grob].
#' @param ... Additional arguments passed to specific methods.
#' @return A [`grob`][grid::grob] object.
#' @seealso
#' - [`patch.grob()`] / [`patch.gList()`]
#' - [`patch.ggplot()`]
#' - [`patch.patch_ggplot()`]
#' - [`patch.patchwork()`]
#' - [`patch.patch()`]
#' - [`patch.trellis()`]
#' - [`patch.formula()`] / [`patch.function()`]
#' - [`patch.recordedplot()`]
#' - [`patch.Heatmap()`]
#' - [`patch.HeatmapList()`]
#' - [`patch.HeatmapAnnotation()`]
#' - [`patch.pheatmap()`]
#' @export
#' @keywords internal
patch <- function(x, ...) {
    rlang::check_dots_used()
    UseMethod("patch")
}

# Following methods much are copied from `cowplot` or `ggplotify`
#' @export
patch.default <- function(x, ...) {
    cli_abort("Cannot make grob from {.obj_type_friendly {x}}")
}

#' @inherit patch title description return
#' @inheritParams patch
#' @param ... Not used currently.
#' @family patch methods
#' @export
patch.grob <- function(x, ...) {
    x
}

#' @export
#' @rdname patch.grob
patch.gList <- function(x, ...) {
    # gLists need to be wrapped in a grob tree
    grid::grobTree(plot)
}

#' @importFrom ggplot2 ggplotGrob
#' @inherit patch.grob
#' @seealso [ggplot][ggplot2::ggplot]
#' @family patch methods
#' @export
patch.ggplot <- function(x, ...) {
    ggplotGrob(x)
}

#' @inherit patch.grob
#' @seealso
#' - [`patch_titles()`]
#' - [`inset()`]
#' - [`ggwrap()`]
#' @family patch methods
#' @export
patch.patch_ggplot <- function(x, ...) {
    ggalignGrob(x)
}

#' @inherit patch.grob
#' @seealso [`alignpatches`][align_plots]
#' @family patch methods
#' @export
patch.alignpatches <- function(x, ...) {
    ggalignGrob(x)
}

#' @inherit patch.grob
#' @seealso [`patchwork`][patchwork::patchworkGrob]
#' @family patch methods
#' @export
patch.patchwork <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patchwork")
    patchwork::patchworkGrob(x)
}

#' @inherit patch.grob
#' @seealso [`patch`][patchwork::patchGrob]
#' @family patch methods
#' @export
patch.patch <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patch")
    patchwork::patchGrob(x)
}

#' @inherit patch.grob
#' @param ... Graphical Parameters passed on to [par()][graphics::par].
#' @inheritParams gridGraphics::echoGrob
#' @seealso [`plot()`]
#' @family patch methods
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
#' @seealso [`recordPlot()`][grDevices::recordPlot]
#' @family patch methods
#' @export
patch.recordedplot <- function(x, ..., device = NULL) {
    rlang::check_installed("gridGraphics", "to make grob from recordedplot")
    gridGraphics::echoGrob(x, device = device %||% offscreen)
}

offscreen <- function(width, height) {
    if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_capture(width = width, height = height, units = "in")
        grDevices::dev.control("enable")
    } else {
        grDevices::pdf(NULL, width = width, height = height)
        grDevices::dev.control("enable")
    }
}

#' @inherit patch.grob
#' @inheritDotParams grid::grid.grabExpr -expr -device
#' @inheritParams grid::grid.grabExpr
#' @seealso [`trellis`][lattice::trellis.object]
#' @family patch methods
#' @export
patch.trellis <- function(x, ..., device = NULL) {
    grid::grid.grabExpr(expr = print(x), ..., device = device %||% offscreen)
}

#' @inherit patch.grob
#' @param ... Additional arguments passed to [draw()][ComplexHeatmap::draw].
#' @inheritParams grid::grid.grabExpr
#' @seealso
#'  - [`Heatmap()`][ComplexHeatmap::Heatmap]
#'  - [`HeatmapAnnotation()`][ComplexHeatmap::HeatmapAnnotation]
#' @family patch methods
#' @importFrom utils getFromNamespace
#' @export
patch.Heatmap <- function(x, ..., device = NULL) {
    rlang::check_installed(
        "ComplexHeatmap",
        sprintf("to make grob from %s plot", obj_type_friendly(x))
    )
    draw <- getFromNamespace("draw", "ComplexHeatmap")
    grid::grid.grabExpr(
        expr = draw(object = x, ...),
        device = device %||% offscreen
    )
}

#' @export
#' @rdname patch.Heatmap
patch.HeatmapList <- patch.Heatmap

#' @export
#' @rdname patch.Heatmap
patch.HeatmapAnnotation <- patch.HeatmapList

#' @inherit patch.grob
#' @seealso [`pheatmap()`][pheatmap::pheatmap]
#' @family patch methods
#' @export
patch.pheatmap <- function(x, ...) .subset2(x, "gtable")
