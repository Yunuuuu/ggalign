#' Layer with Grid or Function
#'
#' Draw ggplot2 layer with a grod or function.
#'
#' @param draw Either a function (can be purrr-style) which accepts two
#'   arguments (\code{data} and \code{coords}) and returns a [grob][grid::grob]
#'   or any objects which can be converted to [grob][grid::grob] by [patch()].
#'
#'   when `draw` is a function, it is used as the `draw_group` function
#'   in a [Geom][ggplot2::Geom] `ggproto` object.
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' function specified in argument `draw` or [patch()] if `draw` is not a
#' function.
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
#' @return A ggplot2 layer.
#' @importFrom ggplot2 zeroGrob
#' @export
geom_draw <- function(draw = zeroGrob(), ...,
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
        params = list(
            draw = draw, na.rm = na.rm,
            draw_params = rlang::list2(...)
        )
    )
}

#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @aliases GeomDraw
#' @importFrom grid is.grob
#' @importFrom ggplot2 ggproto
#' @export
#' @keywords internal
GeomDraw <- ggproto(
    "GeomDraw", ggplot2::Geom,
    ## No required_aes
    ## No default_aes
    ## No draw_key
    extra_params = c("na.rm"),
    draw_group = function(data, panel_params, coord, draw, draw_params) {
        if (is.function(draw)) {
            coords <- coord$transform(data, panel_params)
            grob <- rlang::inject(draw(coords, !!!draw_params))
            if (!is.grob(grob)) {
                cli::cli_abort("{.fn draw} must return a {.cls grob}")
            }
        } else {
            grob <- rlang::inject(patch(x = draw, !!!draw_params))
            if (!is.grob(grob)) {
                cli::cli_abort("{.fn patch} must return a {.cls grob}")
            }
        }
        grob
    }
)

#' @inherit ggplot2::Geom title seealso
#' @inheritSection ggplot2::Geom Geoms
#' @inheritSection ggplot2::Coord Coordinate systems
#' @name ggplot2-ggproto
NULL
