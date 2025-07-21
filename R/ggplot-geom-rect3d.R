#' Add z-aesthetic for geom_tile
#'
#' @section new aesthetics:
#'  - `z`: the third dimention (in the z direction), use
#'    [`scale_z_continuous()`] to control the ranges.
#'  - `theta`: Angle between x-axis and z-axis.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_polygon
#' @inheritParams ggplot2::geom_path
#' @aesthetics GeomRect3d
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#' colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#' ggheatmap(small_mat,
#'     filling = FALSE,
#'     theme = theme(
#'         legend.box.spacing = unit(10, "mm"),
#'         plot.margin = margin(t = 15, unit = "mm")
#'     )
#' ) +
#'     geom_tile3d(
#'         aes(fill = value, z = value, width = 0.8, height = 0.8),
#'         color = "black"
#'     ) +
#'     scale_fill_viridis_c(
#'         option = "plasma",
#'         breaks = scales::breaks_pretty(3L)
#'     ) +
#'     coord_cartesian(clip = "off")
#'
#' @export
geom_rect3d <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", ...,
                        lineend = "butt", linejoin = "round", linemitre = 10,
                        na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRect3d,
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

#' @importFrom ggplot2 ggproto fill_alpha
GeomRect3d <- ggproto(
    "GeomRect3d",
    ggplot2::GeomRect,
    required_aes = c(ggplot2::GeomRect$required_aes, "z"),
    non_missing_aes = c(ggplot2::GeomRect$non_missing_aes, "z", "theta"),
    setup_data = function(self, data, params) {
        theta <- data$theta %||% .subset2(params, "theta")
        if (!is.null(theta) && any(theta <= 0 || theta >= 90)) {
            cli_abort(
                "value mapped to {.field theta} aesthetic must > 0 and < 90."
            )
        }
        data$theta <- theta %||% 60
        ggproto_parent(ggplot2::GeomRect, self)$setup_data(data, params)
    },
    draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                          linejoin = "round", linemitre = 10) {
        data <- setup_3d_data(data)
        # Transform to viewport coords
        coords <- coord$transform(data, panel_params)

        # collapse the gpar value
        data <- .subset2(
            vec_split(
                data[setdiff(names(data), c("x", "y", "width", "height"))],
                .subset2(data, "polygon_id")
            ),
            "val"
        )
        data <- vec_rbind(!!!lapply(data, vec_unique))

        # Draw as grob
        grid::polygonGrob(
            x = coords$x,
            y = coords$y,
            id = coords$polygon_id,
            default.units = "native",
            gp = gpar(
                col = data$colour,
                fill = fill_alpha(data$fill, data$alpha),
                lwd = data$linewidth,
                lty = data$linetype,
                lineend = lineend,
                linejoin = linejoin,
                linemitre = linemitre
            )
        )
    }
)

setup_3d_data <- function(data) {
    data <- vec_slice(
        data,
        order(
            .subset2(data, "xmin"),
            .subset2(data, "ymin"),
            decreasing = TRUE
        )
    )
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
        },
        data,
        MoreArgs = NULL
    )
    vec_cbind(
        vec_rbind(!!!coords),
        vec_rep_each(
            data[
                vec_set_difference(
                    names(data),
                    c("x", "xmin", "xmax", "y", "ymin", "ymax", "z")
                )
            ],
            list_sizes(coords)
        ),
        polygon_id = vec_rep_each(seq_along(coords), list_sizes(coords))
    )
}

#' @aesthetics GeomTile3d
#' @importFrom rlang list2
#' @export
#' @rdname geom_rect3d
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

#' @importFrom ggplot2 ggproto ggproto_parent
GeomTile3d <- ggproto(
    "GeomTile3d",
    ggplot2::GeomTile,
    required_aes = c(ggplot2::GeomTile$required_aes, "z"),
    non_missing_aes = c(ggplot2::GeomTile$non_missing_aes, "z", "theta"),
    setup_data = function(self, data, params) {
        theta <- data$theta %||% .subset2(params, "theta")
        if (!is.null(theta) && any(theta <= 0 || theta >= 90)) {
            cli_abort(
                "value mapped to {.field theta} aesthetic must > 0 and < 90."
            )
        }
        data$theta <- theta %||% 60
        ggproto_parent(ggplot2::GeomTile, self)$setup_data(data, params)
    },
    draw_panel = function(self, data, panel_params, coord, lineend = "butt",
                          linejoin = "round", linemitre = 10) {
        ggproto_parent(GeomRect3d, self)$draw_panel(
            data = data, panel_params = panel_params, coord = coord,
            lineend = lineend, linejoin = linejoin, linemitre = linemitre
        )
    }
)

#' z scales
#'
#' @param ... Other arguments passed on to
#'   [`continuous_scale()`][ggplot2::continuous_scale],
#'   [`binned_scale()`][ggplot2::binned_scale], or
#'   [`discrete_scale()`][ggplot2::discrete_scale] as appropriate, to control
#'   name, limits, breaks, labels and so forth.
#' @param range Output range of z values. Must larger than 0.
#' @inheritParams ggplot2::continuous_scale
#' @seealso [`geom_tile3d()`]/[`geom_rect3d()`]
#' @export
#' @examples
#'
#' set.seed(7)
#' mat <- matrix(runif(100), 10)
#' rownames(mat) <- LETTERS[1:10]
#' colnames(mat) <- letters[1:10]
#' ggheatmap(mat,
#'     filling = FALSE,
#'     theme = theme(
#'         legend.box.spacing = unit(10, "mm"),
#'         plot.margin = margin(t = 15, unit = "mm")
#'     )
#' ) +
#'     geom_tile3d(aes(fill = value, z = value, width = 0.8, height = 0.8)) +
#'     scale_z_continuous(range = c(0.2, 1)) +
#'     coord_cartesian(clip = "off")
#' @export
scale_z_continuous <- function(name = waiver(), ..., range = c(0.1, 1),
                               guide = "none") {
    if (min(range) < 0) {
        cli_abort("{.arg range} must contain only positive values")
    }
    ggplot2::continuous_scale("z",
        name = name, palette = scales::pal_rescale(range), ...,
        guide = guide
    )
}

#' @rdname scale_z_continuous
#' @export
scale_z_binned <- function(name = waiver(), ..., range = c(0.1, 1),
                           guide = "none") {
    if (min(range) < 0) {
        cli_abort("{.arg range} must contain only positive values")
    }
    ggplot2::binned_scale("z",
        name = name, palette = scales::pal_rescale(range), ...,
        guide = guide
    )
}

#' @rdname scale_z_continuous
#' @export
scale_z_discrete <- function(...) {
    cli_warn("Using {z} for a discrete variable is not advised.")
    args <- list2(...)
    args$call <- args$call %||% current_call()
    rlang::exec(scale_z_ordinal, !!!args)
}

#' @rdname scale_z_continuous
#' @export
scale_z_ordinal <- function(name = waiver(), ..., range = c(0.1, 1),
                            guide = "none") {
    if (min(range) < 0) {
        cli_abort("{.arg range} must contain only positive values")
    }
    ggplot2::discrete_scale(
        "z",
        name = name,
        palette = function(n) seq(range[1], range[2], length.out = n),
        ...,
        gudie = guide
    )
}

#' @rdname scale_z_continuous
#' @export
#' @usage NULL
scale_z_datetime <- function(name = waiver(), ..., range = c(0.1, 1),
                             guide = "none") {
    if (min(range) < 0) {
        cli_abort("{.arg range} must contain only positive values")
    }
    ggplot2::datetime_scale(
        aesthetics = "z", transform = "time", name = name,
        palette = scales::pal_rescale(range),
        ..., guide = guide
    )
}

#' @rdname scale_z_continuous
#' @export
#' @usage NULL
scale_z_date <- function(name = waiver(), ..., range = c(0.1, 1),
                         guide = "none") {
    if (min(range) < 0) {
        cli_abort("{.arg range} must contain only positive values")
    }
    ggplot2::datetime_scale(
        aesthetics = "z", transform = "date", name = name,
        palette = scales::pal_rescale(range),
        ..., guide = guide
    )
}
