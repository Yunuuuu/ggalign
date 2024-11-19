#' Raster Image Processed by Magick
#'
#' @description
#' A specialized version of [`geom_raster()`][ggplot2::geom_raster] that
#' includes post-processing of raster images using
#' [`image_resize()`][magick::image_resize()] from the **magick** package.
#' This allows for flexible resizing of raster images directly within a ggplot2
#' workflow.
#'
#' @inheritParams ggplot2::geom_raster
#' @inheritParams magick::image_resize
#' @eval rd_gg_aesthetics("geom", "magick")
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
#' # Use `magick::filter_types()` to check available filter arguments
#' plots <- lapply(
#'     c("tile", "raster", magick::filter_types()),
#'     function(filter) {
#'         ggheatmap(mat, filling = FALSE) +
#'             switch(filter,
#'                 tile = geom_tile(aes(fill = value)),
#'                 raster = geom_raster(aes(fill = value)),
#'                 geom_magick(aes(fill = value), filter = filter)
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
geom_magick <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        filter = NULL, geometry = NULL,
                        hjust = 0.5,
                        vjust = 0.5,
                        interpolate = FALSE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
    rlang::check_installed("magick", "to use `geom_magick()`")
    assert_number_decimal(hjust)
    assert_number_decimal(vjust)
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomMagick,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            geometry = geometry,
            filter = filter,
            hjust = hjust,
            vjust = vjust,
            interpolate = interpolate,
            na.rm = na.rm,
            ...
        )
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
GeomMagick <- ggproto(
    "GeomMagick",
    ggplot2::GeomRaster,
    draw_panel = function(self, data, panel_params, coord,
                          geometry = NULL, filter = NULL, interpolate = FALSE,
                          hjust = 0.5, vjust = 0.5) {
        grob <- ggproto_parent(ggplot2::GeomRaster, self)$draw_panel(
            data = data, panel_params = panel_params, coord = coord,
            interpolate = interpolate, hjust = hjust, vjust = vjust
        )
        if (!inherits(coord, "CoordCartesian")) {
            return(grob)
        }
        image <- magick::image_resize(
            magick::image_read(.subset2(grob, "raster")),
            geometry = geometry,
            filter = filter
        )
        grob$raster <- grDevices::as.raster(image)
        grob
    },
)
