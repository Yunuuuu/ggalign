#' Layer with Grid or Function
#'
#' Draw ggplot2 layer with a grod or function.
#'
#' @param draw Either a [grob][grid::grob] object or a function (can be
#'   purrr-style) which accepts two arguments (\code{data} and \code{coords})
#'   and returns a [grob][grid::grob].
#'   
#'   when `draw` is a function, it is used as the `draw_group` function
#'   in a [Geom][ggplot2::Geom] `ggproto` object. So One should always call
#'   `coord$transform(data, panel_params)` in function `draw` to get transformed
#'   data in the plot scales.
#' @param ... Additional arguments passed to `draw`.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @details If you want to combine the functionality of multiple geoms it can
#'   usually be achieved by preparing the data for each of the geoms inside the
#'   `draw_*()` call and send it off to the different geoms, collecting the
#'   output in a [`grid::gList`] (a list of grobs) if the call is `draw_group()`
#'   or a [`grid::gTree`] (a grob containing multiple children grobs) if the
#'   call is `draw_panel()`.
#' @seealso <https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html>
#' @examples
#' ggdraw_text <- grid::textGrob(
#'     "ggdraw",
#'     x = c(0, 0, 0.5, 1, 1),
#'     y = c(0, 1, 0.5, 0, 1),
#'     hjust = c(0, 0, 0.5, 1, 1),
#'     vjust = c(0, 1, 0.5, 0, 1)
#' )
#' ggplot2::ggplot(data.frame(x = 1, y = 2)) +
#'     geom_draw(ggdraw_text)
#' @export
geom_draw <- function(draw = grid::nullGrob(), ...,
                      mapping = NULL, data = NULL,
                      stat = "identity", position = "identity",
                      na.rm = FALSE, inherit.aes = TRUE) {
    draw <- allow_lambda(draw)
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomDraw,
        position = position,
        show.legend = FALSE,
        inherit.aes = inherit.aes,
        params = rlang::list2(
            draw = draw,
            na.rm = na.rm,
            draw_params = rlang::list2(...)
        )
    )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @aliases GeomDraw
#' @export
GeomDraw <- ggplot2::ggproto(
    "GeomDraw", ggplot2::Geom,
    ## No required_aes
    ## No default_aes
    ## No draw_key
    extra_params = c("na.rm"),
    draw_group = function(data, panel_params, coord, draw, draw_params) {
        if (grid::is.grob(draw)) {
            draw
        } else {
            coords <- coord$transform(data, panel_params)
            rlang::inject(draw(coords, !!!draw_params))
        }
    }
)

#' @inherit ggplot2::Geom title seealso
#' @inheritSection ggplot2::Geom Geoms
#' @inheritSection ggplot2::Coord Coordinate systems
#' @name ggplot2-ggproto
NULL
