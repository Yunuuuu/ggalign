#' Raster Rectangles Image Processed by Magick
#'
#' @description
#' A specialized version of [`geom_raster()`][ggplot2::geom_raster] that
#' integrates post-processing of raster images using the **magick** package.
#' This enables flexible resizing and transformation of raster images directly
#' within a ggplot2 pipeline.
#'
#' The following commonly used operations are provided (you can also define
#' custom transformations using the `image` argument):
#'
#' - `geom_raster_resize`: [`image_resize()`][magick::image_resize()].
#' - `geom_raster_sample`: [`image_sample()`][magick::image_sample()].
#' - `geom_raster_scale`: [`image_scale()`][magick::image_scale()].
#'
#' @param magick A function (purrr-style formula is accepted) that takes an
#' [`image_read()`][magick::image_read] object as input and returns an object
#' compatible with [`as.raster()`][grDevices::as.raster]. You can use any of
#' the `image_*()` functions from the **magick** package to process the raster
#' rectangle.
#'
#' @inheritParams ggplot2::geom_raster
#' @eval rd_gg_aesthetics("geom", "raster_magick")
#' @examples
#' # data generated code was copied from `ComplexHeatmap`
#' set.seed(123)
#' mat <- matrix(nrow = 5000, ncol = 50)
#' for (i in 1:5000) {
#'     mat[i, ] <- runif(50) + c(
#'         sort(abs(rnorm(50)))[1:25],
#'         rev(sort(abs(rnorm(50)))[1:25])
#'     ) * i / 1000
#' }
#'
#' # Use `magick::filter_types()` to check available `filter` arguments in
#' # `geom_raster_resize()`
#' plots <- lapply(
#'     c(
#'         "tile", "raster", "Lanczos", "Lanczos2",
#'         "Lanczos2Sharp", "LanczosRadius", "LanczosSharp"
#'     ),
#'     function(filter) {
#'         ggheatmap(mat, filling = FALSE) +
#'             switch(filter,
#'                 tile = geom_tile(aes(fill = value)),
#'                 raster = geom_raster(aes(fill = value)),
#'                 geom_raster_resize(aes(fill = value), filter = filter)
#'             ) +
#'             scale_fill_gradientn(
#'                 colours = rev(
#'                     scales::pal_brewer("div", palette = "Spectral")(11L)
#'                 ),
#'                 guide = "none"
#'             ) +
#'             ggtitle(switch(filter,
#'                 tile = "geom_tile()",
#'                 raster = "geom_raster()",
#'                 sprintf("magick: filter = %s", filter)
#'             ))
#'     }
#' )
#' align_plots(!!!plots)
#'
#' @importFrom rlang list2
#' @export
geom_raster_magick <- function(magick, mapping = NULL, data = NULL,
                               stat = "identity", position = "identity",
                               ...,
                               hjust = 0.5,
                               vjust = 0.5,
                               interpolate = FALSE,
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE) {
    if (override_call(call <- caller_call())) {
        call <- current_call()
    }
    rlang::check_installed(
        "magick", sprintf("to use `%s()`", deparse(.subset2(call, 1L))),
        call = call
    )
    assert_number_decimal(hjust, call = call)
    assert_number_decimal(vjust, call = call)
    if (!is.function(magick <- allow_lambda(magick))) {
        cli_abort("{.arg magick} must be a function", call = call)
    }
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRasterMagick,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            magick = magick,
            hjust = hjust,
            vjust = vjust,
            interpolate = interpolate,
            na.rm = na.rm,
            ...
        )
    )
}

#' @inheritParams magick::image_resize
#' @export
#' @rdname geom_raster_magick
geom_raster_resize <- function(..., filter = NULL, geometry = NULL) {
    force(filter)
    force(geometry)
    geom_raster_magick(
        ...,
        magick = function(image) {
            magick::image_resize(image, geometry = geometry, filter = filter)
        }
    )
}

#' @inheritParams magick::image_sample
#' @export
#' @rdname geom_raster_magick
geom_raster_sample <- function(..., geometry = NULL) {
    force(geometry)
    geom_raster_magick(
        ...,
        magick = function(image) {
            magick::image_sample(image, geometry = geometry)
        }
    )
}

#' @inheritParams magick::image_scale
#' @export
#' @rdname geom_raster_magick
geom_raster_scale <- function(..., geometry = NULL) {
    force(geometry)
    geom_raster_magick(
        ...,
        magick = function(image) {
            magick::image_scale(image, geometry = geometry)
        }
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
GeomRasterMagick <- ggproto(
    "GeomRasterMagick",
    ggplot2::GeomRaster,
    draw_panel = function(self, data, panel_params, coord,
                          magick, interpolate = FALSE,
                          hjust = 0.5, vjust = 0.5) {
        grob <- ggproto_parent(ggplot2::GeomRaster, self)$draw_panel(
            data = data, panel_params = panel_params, coord = coord,
            interpolate = interpolate, hjust = hjust, vjust = vjust
        )
        if (!inherits(coord, "CoordCartesian")) {
            return(grob)
        }
        image <- magick(magick::image_read(.subset2(grob, "raster")))
        grob$raster <- grDevices::as.raster(image)
        grob
    },
)
