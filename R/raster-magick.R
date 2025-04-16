#' Rasterize the ggplot layers
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
#' @inheritParams rlang::args_dots_empty
#' @inheritParams magickGrob
#' @examples
#' # Currently, `magick` package require R >= 4.1.0
#' if (requireNamespace("magick")) {
#'     # data generated code was copied from `ComplexHeatmap`
#'     set.seed(123)
#'     small_mat <- matrix(rnorm(56), nrow = 7)
#'     rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#'     colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#'     ggheatmap(small_mat, aes(.x, .y), filling = NULL) +
#'         raster_magick(geom_tile(aes(fill = value)), res = 20)
#'
#'     ggheatmap(small_mat, aes(.x, .y), filling = NULL) +
#'         # Use `magick::filter_types()` to check available `filter` arguments
#'         raster_magick(
#'             geom_tile(aes(fill = value)),
#'             magick = function(image) {
#'                 magick::image_resize(image,
#'                     geometry = "50%x", filter = "Lanczos"
#'                 )
#'             }
#'         )
#' }
#' @return An object with the same class of the input.
#' @seealso [`magickGrob()`]
#' @export
raster_magick <- function(x, magick = NULL, ...,
                          res = NULL, interpolate = FALSE,
                          vp = NULL) {
    rlang::check_installed("magick", "to use `raster_magick()`")
    if (!is.null(magick) && !is.function(magick <- allow_lambda(magick))) {
        cli_abort("{.arg magick} must be a function")
    }
    assert_number_whole(res, min = 1, allow_null = TRUE)
    assert_bool(interpolate)
    raster_magick0(
        x = x, ..., magick = magick,
        res = res, interpolate = interpolate,
        vp = vp
    )
}

# Used to do the actual process, but won't check the arguments
#' @keywords internal
raster_magick0 <- function(x, ...) {
    UseMethod("raster_magick0")
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
raster_magick0.Layer <- function(x, ...) {
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
            raster_magick0(grobs, ...)
        }
    )
}

#' @export
raster_magick0.ggplot <- function(x, ...) {
    x$layers <- lapply(x$layers, raster_magick0, ...)
    x
}

#' @export
raster_magick0.list <- function(x, ...) lapply(x, raster_magick0, ...)

#' @export
raster_magick0.grob <- function(x, magick = NULL, ...,
                                res = NULL, interpolate = FALSE,
                                vp = NULL) {
    rlang::check_dots_empty()
    magickGrob0(
        grob = x, magick = magick,
        res = res, interpolate = interpolate, vp = vp
    )
}

#' @export
raster_magick0.gList <- raster_magick0.grob

#' @export
raster_magick0.default <- function(x, ...) {
    cli_abort("Cannot rasterize {.obj_type_friendly {x}}")
}
