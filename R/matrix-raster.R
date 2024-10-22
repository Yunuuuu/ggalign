# Copyright 2022 Sergio Oller Moreno <sergioller@gmail.com> This file is part of
# the ggmatrix package and it is distributed under the MIT license terms.  Check
# the ggmatrix package license information for further details.
# Modified from: https://github.com/zeehio/ggmatrix/blob/main/R/geom_matrix_raster.R
#' Raster a matrix as a rectangle, efficiently
#'
#' @param matrix The matrix we want to render in the plot
#' @inheritParams ggplot2::geom_raster
#' @keywords internal
#' @noRd
geom_matrix_raster <- function(
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    ..., interpolate = FALSE, na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
        data = new_data_frame(list(value = c(data))),
        mapping = mapping, stat = stat, geom = GeomMatrixRaster,
        position = position, show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            matrix = data, interpolate = interpolate, na.rm = na.rm, ...
        )
    )
}

#' @importFrom ggplot2 ggproto
GeomMatrixRaster <- ggproto(
    "GeomMatrixRaster", ggplot2::GeomRaster,
    non_missing_aes = c("fill"),
    required_aes = c("fill"),
    draw_panel = function(self, data, panel_params, coord,
                          matrix, interpolate) {
        if (!inherits(coord, "CoordCartesian")) {
            cli::cli_abort(c(
                "{.fn geom_raster} only works with {.fn coord_cartesian}."
            ))
        }
        if (inherits(coord, "CoordFlip")) {
            byrow <- TRUE
            mat_nr <- ncol(matrix)
            mat_nc <- nrow(matrix)
        } else {
            byrow <- FALSE
            mat_nr <- nrow(matrix)
            mat_nc <- ncol(matrix)
        }
        nr_dim <- c(mat_nr, mat_nc)
        mat <- matrix(
            farver::encode_native(
                scales::alpha(.subset2(data, "fill"), .subset2(data, "alpha"))
            ),
            nrow = mat_nr,
            ncol = mat_nc,
            byrow = byrow
        )
        nr <- structure(
            mat,
            dim = nr_dim,
            class = "nativeRaster",
            channels = 4L
        )
        grid::rasterGrob(
            nr, x_rng[1], y_rng[1],
            diff(x_rng), diff(y_rng),
            default.units = "native",
            just = c("left", "bottom"), interpolate = interpolate
        )
    }
)
