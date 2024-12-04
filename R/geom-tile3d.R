#' Add z-aesthetic for geom_tile
#'
#' @section new aesthetics:
#'  - `z`: the third dimention (in the z direction).
#'  - `theta`: Angle between x-axis and z-axis.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_path
#' @eval rd_gg_aesthetics("geom", "tile3d")
#' @export
geom_tile3d <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", ...,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomTile3d,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre,
            na.rm = na.rm, ...
        )
    )
}

#' @importFrom ggplot2 ggproto
GeomTile3d <- ggproto(
    "GeomTile3d",
    ggplot2::GeomTile,
    default_aes = ggplot2::GeomTile$default_aes,
    required_aes = c(ggplot2::GeomTile$required_aes, "z"),
    non_missing_aes = c(ggplot2::GeomTile$non_missing_aes, "z", "theta"),
    setup_data = function(self, data, params) {
        if (any(data$z %||% .subset2(params, "z") < 0)) {
            cli_abort("value mapped to {.field z} aesthetic must >= 0")
        }
        theta <- data$theta %||% .subset2(params, "theta") %||% 60
        if (!is.null(theta) && any(theta <= 0 || theta >= 90)) {
            cli_abort("value mapped to {.field theta} aesthetic must > 0 and < 90.")
        }
        data$theta <- theta
        data <- ggproto_parent(ggplot2::GeomTile, self)$setup_data(
            data, params
        )
        data <- vec_slice(data, order(
            .subset2(data, "xmin"), .subset2(data, "ymin"),
            decreasing = TRUE
        ))
        coords <- .mapply(
            function(xmin, xmax, ymin, ymax, z, theta, ...) {
                if (z == 0L) {
                    # fallback to tile
                    data_frame0(
                        x = vec_c(xmin, xmax, xmax, xmin),
                        y = vec_rep_each(c(ymin, ymax), 2L)
                    )
                } else {
                    offset_x <- z * cos(theta / 180 * pi)
                    z_xmin <- xmin + offset_x
                    z_xmax <- xmax + offset_x
                    offset_y <- z * sin(theta / 180 * pi)
                    z_ymin <- ymin + offset_y
                    z_ymax <- ymax + offset_y
                    data_frame0(
                        x = vec_c(
                            xmin, z_xmin, z_xmax, xmax, xmin, xmin,
                            z_xmin, z_xmin, z_xmin, z_xmax, z_xmax, z_xmin
                        ),
                        y = vec_c(
                            ymin, z_ymin, z_ymin, ymin, ymin, ymax,
                            z_ymax, z_ymin, z_ymax, z_ymax, z_ymin, z_ymin
                        )
                    )
                }
            }, data,
            MoreArgs = NULL
        )
        vec_cbind(
            vec_rbind(!!!coords),
            vec_rep_each(
                data[setdiff(
                    names(data),
                    c("x", "xmin", "xmax", "y", "ymin", "ymax", "z")
                )],
                list_sizes(coords)
            ),
            polygon_id = vec_rep_each(seq_along(coords), list_sizes(coords))
        )
    },
    draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                          linejoin = "round", linemitre = 10) {
        # Transform to viewport coords
        coords <- coord$transform(data, panel_params)

        # collapse the gpar value
        data <- .subset2(vec_split(
            data[setdiff(names(data), c("x", "y", "width", "height"))],
            .subset2(data, "polygon_id")
        ), "val")
        data <- vec_rbind(!!!lapply(data, vec_unique))

        # Draw as grob
        grid::polygonGrob(
            x = coords$x,
            y = coords$y,
            id = coords$polygon_id,
            default.units = "native",
            gp = gpar(
                col = data$colour,
                fill = try_fetch(
                    # for version >= 3.5.0
                    ggplot2::fill_alpha(data$fill, data$alpha),
                    error = function(cnd) {
                        # for version < 3.5.0
                        ggplot2::alpha(data$fill, data$alpha)
                    }
                ),
                lwd = data$linewidth,
                lty = data$linetype,
                lineend = lineend,
                linejoin = linejoin,
                linemitre = linemitre
            )
        )
    }
)
