#' Create a ggplot inset
#'
#' @inheritParams wrap
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
    grob <- make_inset(
        plot = plot, ..., align = align, on_top = on_top,
        clip = clip, vp = vp
    )
    add_class(grob, "patch_inset")
}

#' @importFrom rlang arg_match0
make_inset <- function(plot, ..., align, on_top, clip, vp,
                       call = caller_call()) {
    assert_bool(on_top, call = call)
    align <- arg_match0(align, c("panel", "plot", "full"), error_call = call)
    assert_bool(clip, call = call)
    assert_s3_class(vp, "viewport", null_ok = TRUE, call = call)
    if (!is.grob(grob <- patch(x = plot, ...))) {
        cli::cli_abort("{.fn patch} must return a {.cls grob}", call = call)
    }
    attr(grob, "align") <- align
    attr(grob, "clip") <- if (clip) "on" else "off"
    attr(grob, "vp") <- vp
    attr(grob, "on_top") <- on_top
    grob
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.patch_inset <- function(object, plot, object_name) {
    make_wrap(plot, object)
}

############################################################
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

#' @importFrom ggplot2 ggplotGrob
#' @inherit patch.grob
#' @seealso [ggplot][ggplot2::ggplot]
#' @export
patch.ggplot <- function(x, ...) {
    rlang::check_dots_empty()
    ggplotGrob(x)
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
#' @seealso [pheatmap][pheatmap::pheatmap]
#' @export
#' @rdname patch.pheatmap
patch.pheatmap <- function(x, ...) {
    .subset2(x, "gtable")
}
