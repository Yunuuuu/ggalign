#' Pie charts
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_path
#' @param steps An integer indicating the number of steps to generate the pie
#' chart radian. Increasing this value results in a smoother pie circular.
#' @eval ggfun("rd_aesthetics")("geom", "pie")
#' @examples
#' ggplot(data.frame(x = 1:10, y = 1:10, value = 1:10 / sum(1:10))) +
#'     geom_pie(aes(x, y, radian = value * 2 * pi))
#' @export
geom_pie <- function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ...,
                     lineend = "butt", linejoin = "round", linemitre = 10,
                     steps = 100, na.rm = FALSE,
                     show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomPie,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            lineend = lineend,
            linejoin = linejoin,
            linemitre = linemitre,
            na.rm = na.rm, steps = steps, ...
        )
    )
}

#' @importFrom ggplot2 ggproto aes .pt resolution
#' @importFrom rlang set_names
#' @importFrom vctrs vec_slice<- vec_cbind vec_rbind vec_rep_each
GeomPie <- ggproto("GeomPie",
    ggplot2::GeomPolygon,
    default_aes = set_names(
        ggplot2::GeomPolygon$default_aes,
        function(nms) {
            nms <- set_names(nms)
            vec_slice(nms, "subgroup") <- "radius"
            nms
        }
    ),
    non_missing_aes = c("x", "y", "radian", "radius"),
    required_aes = c("x", "y", "radian"),
    handle_na = ggplot2::Geom$handle_na,
    setup_params = function(data, params) {
        params$steps <- max(as.integer(.subset2(params, "steps")), 1L) + 1L
        params
    },
    setup_data = function(data, params) {
        # use the same strategy of geom_bar
        data$radius <- data$radius %||%
            params$radius %||% (
                min(
                    vapply(
                        split(data$x, data$PANEL, drop = TRUE),
                        resolution, numeric(1),
                        zero = FALSE
                    ),
                    vapply(
                        split(data$y, data$PANEL, drop = TRUE),
                        resolution, numeric(1),
                        zero = FALSE
                    )
                ) * 0.45)
        data
    },
    draw_panel = function(data, panel_params, coord, steps = 100L,
                          lineend = "butt", linejoin = "round",
                          linemitre = 10) {
        # Expand x, y, radius data to points along circle
        circular_data <- Map(function(x, y, radius, radian) {
            radians <- seq(0, radian, length.out = steps)[-1L]
            data_frame0(
                x = c(x, cos(radians) * radius + x),
                y = c(y, sin(radians) * radius + y)
            )
        }, x = data$x, y = data$y, radius = data$radius, radian = data$radian)
        circular_data <- vec_cbind(
            vec_rbind(!!!circular_data),
            vec_rep_each(
                data[setdiff(names(data), c("x", "y", "radius", "radian"))],
                times = steps
            )
        )
        # Transform to viewport coords
        circular_data <- coord$transform(circular_data, panel_params)

        # Draw as grob
        grid::polygonGrob(
            x = circular_data$x,
            y = circular_data$y,
            id.lengths = rep_len(steps, nrow(data)),
            default.units = "native",
            gp = grid::gpar(
                col = circular_data$colour,
                fill = ggplot2::fill_alpha(
                    circular_data$fill,
                    circular_data$alpha
                ),
                lwd = circular_data$linewidth,
                lty = circular_data$linetype,
                lineend = lineend,
                linejoin = linejoin,
                linemitre = linemitre
            )
        )
    }
)
