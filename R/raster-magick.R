#' Rasterize the input object
#'
#' The function rasterizes input graphical objects (e.g., grob, layer, ggplot)
#' and optionally processes the resulting raster using magick, a powerful image
#' manipulation library. This allows for advanced graphical transformations
#' directly within the plotting pipeline.
#'
#' @param x An object to rasterize, can be a [`grob()`][grid::grob],
#' [`layer()`][ggplot2::layer], [`ggplot()`][ggplot2::ggplot], or a list of such
#' objects.
#'
#' @param magick A function (purrr-style formula is accepted) that takes an
#' [`image_read()`][magick::image_read] object as input and returns an object
#' compatible with [`as.raster()`][grDevices::as.raster]. You can use any of
#' the `image_*()` functions from the **magick** package to process the raster
#' image.
#'
#' @param ... Not used currently.
#' @param res An integer sets the desired resolution in pixels.
#' @inheritParams grid::rasterGrob
#' @examples
#' # data generated code was copied from `ComplexHeatmap`
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#' colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#' ggheatmap(small_mat, aes(.x, .y), filling = NULL) +
#'     raster_magick(geom_tile(aes(fill = value)), res = 20)
#'
#' ggheatmap(small_mat, aes(.x, .y), filling = NULL) +
#'     # Use `magick::filter_types()` to check available `filter` arguments
#'     raster_magick(geom_tile(aes(fill = value)),
#'         magick = function(image) {
#'             magick::image_resize(image,
#'                 geometry = "50%x", filter = "Lanczos"
#'             )
#'         }
#'     )
#' @return An object with the same class of the input.
#' @export
raster_magick <- function(x, magick = NULL, ...,
                          res = NULL, interpolate = FALSE) {
    rlang::check_installed("magick", "to use `raster_magick()`")
    if (!is.null(magick) && !is.function(magick <- allow_lambda(magick))) {
        cli_abort("{.arg magick} must be a function")
    }
    assert_number_whole(res, min = 1, allow_null = TRUE)
    assert_bool(interpolate)
    rlang::check_dots_used()
    .raster_magick(
        x = x, ..., magick = magick,
        res = res, interpolate = interpolate
    )
}

# modified from `ggrastr::rasterize`
# Used to do the actual process, but won't check the arguments
#' @inherit raster_magick title return
#' @description
#' An internal function designed to implement the functionality of
#' `raster_magick()`. It assumes the input arguments are valid and does not
#' perform any additional checks.
#' @inheritParams raster_magick
#' @keywords internal
.raster_magick <- function(x, magick = NULL, ...) {
    UseMethod(".raster_magick")
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
.raster_magick.Layer <- function(x, magick = NULL, ...) {
    ggproto(
        NULL, x,
        draw_geom = function(self, data, layout) {
            grobs <- ggproto_parent(x, self)$draw_geom(data, layout)
            if (!inherits(layout$coord, "CoordCartesian")) {
                cli_warn(
                    "{.fn raster_magick} only works with {.fn coord_cartesian}."
                )
                return(grobs)
            }
            lapply(grobs, .raster_magick, ..., magick = magick)
        }
    )
}

#' @export
.raster_magick.list <- function(x, magick = NULL, ...) {
    lapply(x, .raster_magick, ..., magick = magick)
}

#' @export
.raster_magick.ggplot <- function(x, magick = NULL, ...) {
    x$layers <- lapply(x$layers, .raster_magick, ..., magick = magick)
    x
}

#' @importFrom grid grob
#' @export
.raster_magick.grob <- function(x, magick = NULL, ...,
                                res = NULL, interpolate = FALSE) {
    grob(
        grob = x,
        magick = magick,
        res = res,
        interpolate = interpolate,
        cl = "ggalignRasterMagick"
    )
}

#' @importFrom grid grob
#' @export
.raster_magick.gList <- .raster_magick.grob

#' @export
.raster_magick.ggalignRasterMagick <- function(x, magick = NULL, ...,
                                               res = NULL,
                                               interpolate = FALSE) {
    x["magick"] <- list(magick)
    x["res"] <- list(res)
    x["interpolate"] <- list(interpolate)
    x
}

#' @export
.raster_magick.zeroGrob <- function(x, magick = NULL, ...) x

#' @export
.raster_magick.default <- function(x, magick = NULL, ...) x

# preDraw:
#  - makeContext
#  - pushvpgp
#  - preDrawDetails: by default, do noting
# makeContent:
# drawDetails:
# postDraw:
#  - postDrawDetails: by default, do noting
#  - popgrobvp
#' @importFrom grid makeContent unit convertHeight convertWidth viewport
#' @export
makeContent.ggalignRasterMagick <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "pt", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "pt", valueOnly = TRUE)

    # Grab grob metadata
    plot_res <- convertWidth(unit(1, "inch"), "pt", valueOnly = TRUE)
    res <- .subset2(x, "res") %||% plot_res

    magick <- .subset2(x, "magick")
    interpolate <- .subset2(x, "interpolate")

    # Track current device
    old_dev <- grDevices::dev.cur()

    # Reset current device upon function exit
    on.exit(grDevices::dev.set(old_dev), add = TRUE)

    # open the magick raster device
    image <- magick::image_graph(
        width = width * res / plot_res,
        height = height * res / plot_res,
        bg = NA_character_, res = res,
        clip = FALSE, antialias = FALSE
    )

    # Render the grob
    grid::pushViewport(viewport())

    # Clean up the grob for rendering
    grid::grid.draw(.subset2(x, "grob")) # should respect the viewport of `x`
    grid::popViewport()
    grDevices::dev.off()
    on.exit(magick::image_destroy(image), add = TRUE)

    # run `magick` when necessary
    if (!is.null(magick)) image <- magick(image)

    # Use native raster instead
    raster <- grDevices::as.raster(image, native = TRUE)

    # Forward raster grob
    grid::rasterGrob(
        raster, # should contain current area of full viewport
        x = 0.5, y = 0.5,
        height = unit(height, "pt"),
        width = unit(width, "pt"),
        default.units = "npc",
        just = "center",
        interpolate = interpolate
    )
}
