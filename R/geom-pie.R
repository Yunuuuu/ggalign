#' Pie charts
#' @section new aesthetics:
#'  - `angle`: the pie circle angle.
#'  - `angle0`: the initial pie circle angle.
#'  - `radius`: the circle radius.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_path
#' @param steps An integer indicating the number of steps to generate the pie
#' chart radian. Increasing this value results in a smoother pie circular.
#' @param clockwise A single boolean value indicates clockwise or not.
#' @eval ggfun("rd_aesthetics")("geom", "pie")
#' @examples
#' ggplot(data.frame(x = 1:10, y = 1:10, value = 1:10 / sum(1:10))) +
#'     geom_pie(aes(x, y, angle = value * 360))
#' @export
geom_pie <- function(mapping = NULL, data = NULL, stat = "identity",
                     position = "identity", ...,
                     clockwise = TRUE, steps = 100,
                     lineend = "butt", linejoin = "round", linemitre = 10,
                     na.rm = FALSE,
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
            clockwise = clockwise,
            na.rm = na.rm, steps = steps, ...
        )
    )
}

#' @importFrom ggplot2 ggproto aes .pt resolution
#' @importFrom rlang set_names
GeomPie <- ggproto("GeomPie",
    ggplot2::GeomPolygon,
    default_aes = aes(
        !!!set_names(
            ggplot2::GeomPolygon$default_aes,
            function(nms) {
                nms <- set_names(nms)
                vec_slice(nms, "subgroup") <- "radius"
                nms
            }
        ),
        angle0 = 0
    ),
    non_missing_aes = c("x", "y", "angle", "angle0", "radius"),
    required_aes = c("x", "y", "angle"),
    handle_na = ggplot2::Geom$handle_na,
    setup_params = function(self, data, params) {
        steps <- vec_cast(.subset2(params, "steps"), integer(),
            x_arg = "steps",
            call = call(snake_class(self))
        )
        assert_bool(.subset2(params, "clockwise"),
            arg = "clockwise", call = call(snake_class(self))
        )
        params$steps <- max(steps, 1L) + 1L
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
                          clockwise = TRUE, lineend = "butt",
                          linejoin = "round", linemitre = 10) {
        # Expand x, y, radius data to points along circle
        circular_data <- .mapply(
            function(x, y, radius, ang, ang0) {
                if (clockwise) {
                    ang0 <- 90 - ang0
                    radians <- seq(ang0, ang0 - ang, length.out = steps)[-1L] *
                        pi / 180
                } else {
                    ang0 <- 90 + ang0
                    radians <- seq(ang0, ang0 + ang, length.out = steps)[-1L] *
                        pi / 180
                }
                data_frame0(
                    x = c(x, cos(radians) * radius + x),
                    y = c(y, sin(radians) * radius + y)
                )
            },
            list(
                x = data$x, y = data$y,
                radius = data$radius, ang = data$angle, ang0 = data$angle0
            ),
            MoreArgs = NULL
        )
        circular_data <- vec_rbind(!!!circular_data)

        # Transform to viewport coords
        circular_data <- coord$transform(circular_data, panel_params)

        # Draw as grob
        grid::polygonGrob(
            x = circular_data$x,
            y = circular_data$y,
            id.lengths = rep_len(steps, nrow(data)),
            default.units = "native",
            gp = grid::gpar(
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
