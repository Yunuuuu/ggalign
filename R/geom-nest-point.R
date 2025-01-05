geom_nest_point <- function(mapping = NULL, data = NULL,
                            stat = "identity", position = "identity",
                            ...,
                            width = NULL,
                            nest_reorder = TRUE,
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomNestPoint,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            na.rm = na.rm, width = width,
            nest_reorder = nest_reorder, ...
        )
    )
}

#' @importFrom grid gTree gList
#' @importFrom ggplot2 ggproto resolution
GeomNestPoint <- ggproto("GeomNestPoint",
    ggplot2::GeomPoint,
    required_aes = c(ggplot2::GeomPoint$required_aes, "nest"),
    extra_params = c("na.rm", "width", "orientation"),
    setup_params = function(data, params) {
        params$flipped_aes <- ggplot2::has_flipped_aes(data, params)
        params
    },
    setup_data = function(self, data, params) {
        data <- ggproto_parent(ggplot2::GeomPoint, self)$setup_data(
            data, params
        )
        data$flipped_aes <- params$flipped_aes
        data
    },
    draw_panel = function(self, data, panel_params, coord,
                          width, nest_reorder, flipped_aes) {
        data <- ggplot2::flip_data(data, flipped_aes)
        data$width <- data$width %||%
            width %||% (resolution(data$x, FALSE, TRUE) * 0.9)
        dlist <- vec_split(data, data[c("x", "group")])
        dlist <- lapply(.subset2(dlist, "val"), function(d) {
            xmin <- min(d$x - d$width / 2)
            xmax <- max(d$x + d$width / 2)
            if (isTRUE(nest_reorder) && ggfun("is.discrete")(d$nest)) {
                d$nest <- fct_reorder(d$nest, d$y)
            }
            d$x <- map_nest(d$nest, c(xmin, xmax))
            ggplot2::flip_data(d, flipped_aes)
        })
        ggproto_parent(ggplot2::GeomPoint, self)$draw_panel(
            vec_rbind(!!!dlist), panel_params, coord
        )
    }
)

map_nest <- function(value, to = c(0, 1)) {
    if (ggfun("is.discrete")(value)) {
        range <- scales::DiscreteRange$new()
        range$train(value)
        value <- seq_along(range$range)[match(as.character(value), range$range)]
    } else {
        range <- scales::ContinuousRange$new()
        range$train(value)
        value <- scales::oob_censor(value, range = range$range)
    }
    scales::rescale(value, to)
}
