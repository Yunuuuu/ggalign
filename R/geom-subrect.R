#' Subdivide Rectangles
#'
#' @description
#' `geom_subrect()` and `geom_subtile()` do the same thing, but are
#' parameterised differently: `geom_subrect()` uses the locations of the four
#' corners (`xmin`, `xmax`, `ymin` and `ymax`), while `geom_subtile()` uses the
#' center of the tile and its size (`x`, `y`, `width`, `height`)
#'
#' @param direction A string indicates the divide direction, either
#' `"horizontal"` or `"vertical"`.
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_segment
#' @eval rd_gg_aesthetics("geom", "subrect")
#' @importFrom rlang list2
#' @export
geom_subrect <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         direction = NULL, lineend = "butt", linejoin = "mitre",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSubrect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            direction = direction,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom ggplot2 ggproto ggproto_parent
GeomSubrect <- ggproto(
    "GeomSubrect",
    ggplot2::GeomRect,
    extra_params = c(ggplot2::GeomRect$extra_params, "direction"),
    setup_data = function(self, data, params) {
        data <- ggproto_parent(ggplot2::GeomRect, self)$setup_data(data, params)
        indices <- vec_group_loc(data[c("xmin", "xmax", "ymin", "ymax")])
        data_list <- vec_chop(data, indices = .subset2(indices, "loc"))
        vec_rbind(!!!lapply(data_list, function(data) {
            n <- vec_size(data)
            if (n == 1L) return(data) # styler: off
            if (n == 4L) {
                width <- (data$xmax - data$xmin) / 2L
                height <- (data$ymax - data$ymin) / 2L
                data$xmin <- data$xmin + vec_rep_each(c(0L, width), 2L)
                data$xmax <- data$xmax - vec_rep_each(c(width, 0L), 2L)
                data$ymin <- data$ymin + vec_rep(c(0L, height), 2L)
                data$ymax <- data$ymax - vec_rep(c(height, 0L), 2L)
            } else {
                if (n > 4L) {
                    cli::cli_warn(
                        "Using {.fn {snake_class(self)}} with more than 4 groups ({n}) is not advised"
                    )
                }
                if (is_horizontal(.subset2(params, "direction"))) {
                    width <- data$xmax - data$xmin
                    data$xmin <- data$xmin +
                        (width / n * (seq_len(n) - 1L))
                    data$xmax <- data$xmax -
                        (width / n * rev(seq_len(n) - 1L))
                } else {
                    height <- data$ymax - data$ymin
                    data$ymin <- data$ymin +
                        (height / n * (seq_len(n) - 1L))
                    data$ymax <- data$ymax -
                        (height / n * rev(seq_len(n) - 1L))
                }
            }
            data
        }))
    }
)

#' @importFrom rlang list2
#' @export
#' @rdname geom_subrect
geom_subtile <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         direction = NULL, lineend = "butt", linejoin = "mitre",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSubtile,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            direction = direction,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
GeomSubtile <- ggproto(
    "GeomSubtile",
    ggplot2::GeomTile,
    extra_params = c(ggplot2::GeomRect$extra_params, "direction"),
    setup_data = function(self, data, params) {
        data <- ggproto_parent(ggplot2::GeomTile, self)$setup_data(data, params)
        ggproto_parent(GeomSubrect, self)$setup_data(data, params)
    }
)
