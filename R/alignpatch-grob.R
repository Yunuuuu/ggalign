#' Convert Object into a Grob
#'
#' @param x An object to be converted into a [`grob`][grid::grob].
#' @param ... Additional arguments passed to specific methods.
#' @return A [`grob`][grid::grob] object.
#' @eval rd_collect_family("as_grob", "`as_grob` method collections")
#' @export
#' @keywords internal
as_grob <- function(x, ...) UseMethod("as_grob")

# Following methods much are copied from `cowplot` or `ggplotify`
#' @export
as_grob.default <- function(x, ...) {
    cli_abort("Cannot make grob from {.obj_type_friendly {x}}")
}

#' @inherit as_grob title description return
#' @inheritParams as_grob
#' @param ... Not used currently.
#' @family as_grob
#' @export
as_grob.grob <- function(x, ...) {
    rlang::check_dots_empty()
    x
}

#' @inherit as_grob.grob
#' @param ... Additional arguments passed on to [`gTree`][grid::gTree].
#' @family as_grob
#' @importFrom grid gTree
#' @export
as_grob.gList <- function(x, ...) {
    # gLists need to be wrapped in a gTree
    gTree(children = x, ...)
}

#' @inherit as_grob.grob
#' @family as_grob
#' @export
#' @keywords internal
as_grob.patch_ggplot <- function(x, ...) {
    rlang::check_dots_empty()
    ggalignGrob(x)
}

#' @importFrom ggplot2 ggplotGrob
#' @inherit as_grob.grob
#' @seealso [ggplot][ggplot2::ggplot]
#' @family as_grob
#' @export
as_grob.ggplot <- as_grob.patch_ggplot

#' @inherit as_grob.grob
#' @seealso [`align_plots()`]
#' @family as_grob
#' @export
`as_grob.ggalign::alignpatches` <- as_grob.patch_ggplot

#' @inherit as_grob
#' @inheritDotParams patchwork::patchworkGrob
#' @seealso [`patchwork`][patchwork::patchworkGrob]
#' @family as_grob
#' @export
as_grob.patchwork <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patchwork")
    patchwork::patchworkGrob(x, ...)
}

#' @inherit as_grob
#' @inheritDotParams patchwork::patchGrob
#' @seealso [`patch`][patchwork::patchGrob]
#' @family as_grob
#' @export
as_grob.patch <- function(x, ...) {
    rlang::check_installed("patchwork", "to make grob from patch")
    patchwork::patchGrob(x, ...)
}

#' @inherit as_grob
#' @param ... Graphical Parameters passed on to [par()][graphics::par].
#' @inheritParams gridGraphics::echoGrob
#' @seealso [`plot()`]
#' @family as_grob
#' @export
as_grob.formula <- function(x, ..., device = NULL, name = NULL) {
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
#' @rdname as_grob.formula
as_grob.function <- function(x, ..., device = NULL, name = NULL) {
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

#' @inherit as_grob
#' @inheritParams gridGraphics::echoGrob
#' @seealso [`recordPlot()`][grDevices::recordPlot]
#' @family as_grob
#' @export
as_grob.recordedplot <- function(x, ..., device = NULL) {
    rlang::check_installed("gridGraphics", "to make grob from recordedplot")
    rlang::check_dots_empty()
    gridGraphics::echoGrob(x, device = device %||% offscreen)
}

offscreen <- function(width, height) {
    if (requireNamespace("ragg", quietly = TRUE)) {
        ragg::agg_capture(width = width, height = height, units = "in")
    } else {
        grDevices::pdf(NULL, width = width, height = height)
    }
    grDevices::dev.control("enable")
}

#' @inherit as_grob
#' @inheritDotParams grid::grid.grabExpr -expr -device
#' @inheritParams grid::grid.grabExpr
#' @seealso [`trellis`][lattice::trellis.object]
#' @family as_grob
#' @export
as_grob.trellis <- function(x, ..., device = NULL) {
    grid::grid.grabExpr(expr = print(x), ..., device = device %||% offscreen)
}

#' @inherit as_grob
#' @param ... Additional arguments passed to [draw()][ComplexHeatmap::draw].
#' @inheritParams grid::grid.grabExpr
#' @seealso
#'  - [`Heatmap()`][ComplexHeatmap::Heatmap]
#'  - [`HeatmapAnnotation()`][ComplexHeatmap::HeatmapAnnotation]
#' @family as_grob
#' @export
as_grob.Heatmap <- function(x, ..., device = NULL) {
    rlang::check_installed(
        "ComplexHeatmap",
        sprintf("to make grob from %s", obj_type_friendly(x))
    )
    draw <- getExportedValue("ComplexHeatmap", "draw")
    grid::grid.grabExpr(
        expr = draw(object = x, ...),
        device = device %||% offscreen
    )
}

#' @export
#' @rdname as_grob.Heatmap
as_grob.HeatmapList <- as_grob.Heatmap

#' @export
#' @rdname as_grob.Heatmap
as_grob.HeatmapAnnotation <- as_grob.HeatmapList

#' @inherit as_grob
#' @seealso [`pheatmap()`][pheatmap::pheatmap]
#' @family as_grob
#' @export
as_grob.pheatmap <- function(x, ...) {
    rlang::check_dots_empty()
    .subset2(x, "gtable")
}
