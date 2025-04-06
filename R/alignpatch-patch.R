#' Convert Object into a Grob
#'
#' The `patch()` function is used by [`ggwrap()`] and [inset()] to convert
#' objects into a [`grob`][grid::grob].
#'
#' @param x An object to be converted into a [`grob`][grid::grob].
#' @param ... Additional arguments passed to specific methods.
#' @return A [`grob`][grid::grob] object.
#' @eval rd_collect_family("patch", "`patch` method collections")
#' @export
#' @keywords internal
patch <- function(x, ...) {
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
#' @family patch
#' @export
patch.grob <- function(x, ...) {
    rlang::check_dots_empty()
    x
}

#' @export
#' @rdname patch.grob
patch.gList <- function(x, ...) {
    rlang::check_dots_empty()
    # gLists need to be wrapped in a gTree
    grid::gTree(children = x)
}

#' @importFrom ggplot2 ggplotGrob
#' @inherit patch.grob
#' @seealso [ggplot][ggplot2::ggplot]
#' @family patch
#' @export
patch.ggplot <- function(x, ...) {
    ggplotGrob(x, ...)
}

#' @inherit patch.grob
#' @seealso
#' - [`patch_titles()`]
#' - [`inset()`]
#' - [`ggwrap()`]
#' @family patch
#' @export
patch.patch_ggplot <- function(x, ...) {
    ggalignGrob(x, ...)
}

#' @inherit patch.grob
#' @seealso [`alignpatches`][align_plots]
#' @family patch
#' @export
patch.alignpatches <- function(x, ...) {
    ggalignGrob(x, ...)
}

#' @inherit patch.grob
#' @seealso [`patchwork`][patchwork::patchworkGrob]
#' @family patch
#' @export
patch.patchwork <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patchwork")
    patchwork::patchworkGrob(x, ...)
}

#' @inherit patch.grob
#' @seealso [`patch`][patchwork::patchGrob]
#' @family patch
#' @export
patch.patch <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patch")
    patchwork::patchGrob(x, ...)
}

#' @inherit patch.grob
#' @param ... Graphical Parameters passed on to [par()][graphics::par].
#' @inheritParams gridGraphics::echoGrob
#' @seealso [`plot()`]
#' @family patch
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
#' @family patch
#' @export
patch.recordedplot <- function(x, ..., device = NULL) {
    rlang::check_installed("gridGraphics", "to make grob from recordedplot")
    rlang::check_dots_empty()
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
#' @family patch
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
#' @family patch
#' @export
patch.Heatmap <- function(x, ..., device = NULL) {
    rlang::check_installed(
        "ComplexHeatmap",
        sprintf("to make grob from %s plot", obj_type_friendly(x))
    )
    draw <- getExportedValue("ComplexHeatmap", "draw")
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
#' @family patch
#' @export
patch.pheatmap <- function(x, ...) {
    rlang::check_dots_empty()
    .subset2(x, "gtable")
}
