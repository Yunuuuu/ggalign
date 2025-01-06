#' Layer with Grid or Function
#'
#' Draw a ggplot2 layer using a grob or a function.
#'
#' @param draw Either a [grob][grid::grob] object or a function (can be
#'   purrr-style) that accepts at least three arguments (`data`, `panel_params`
#'   and `coord`) and returns a [grob][grid::grob].
#'
#'   When `draw` is a function, it is used as the `draw_group`/`draw_panel`
#'   function in a [Geom][ggplot2::Geom] `ggproto` object. You should always
#'   call `coord$transform(data, panel_params)` inside the function `draw` to
#'   obtain transformed data in the plot scales.
#'
#' @param type A single string of `r oxford_or(c("group", "panel"))`, `"group"`
#'   draws geoms with `draw_group`, which displays multiple observations as one
#'   geometric object, and `"panel"` draws geoms with `draw_panel`, displaying
#'   individual graphical objects for each observation (row). Default:
#'   `"group"`.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @details If you want to combine the functionality of multiple geoms, it can
#'   typically be achieved by preparing the data for each geom inside the
#'   `draw_*()` call and sending it off to the different geoms, collecting the
#'   output in a [`grid::gList`] (a list of grobs) for `draw_group()` or a
#'   [`grid::gTree`] (a grob containing multiple child grobs) for
#'   `draw_panel()`.
#' @seealso <https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html>
#' @examples
#' text <- grid::textGrob(
#'     "ggdraw",
#'     x = c(0, 0, 0.5, 1, 1),
#'     y = c(0, 1, 0.5, 0, 1),
#'     hjust = c(0, 0, 0.5, 1, 1),
#'     vjust = c(0, 1, 0.5, 0, 1)
#' )
#' ggplot(data.frame(x = 1, y = 2)) +
#'     geom_draw(text)
#' @importFrom rlang list2 arg_match0
#' @importFrom ggplot2 ggproto
#' @export
geom_draw <- function(draw, mapping = NULL, data = NULL,
                      type = "group", stat = "identity",
                      position = "identity", ...,
                      na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE) {
    type <- arg_match0(type, c("group", "panel"))
    draw <- allow_lambda(draw)
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = switch(type,
            panel = ggproto("GeomDraw",
                ggplot2::Geom,
                draw_panel = draw_fn,
                draw_key = draw_key_draw
            ),
            group = ggproto("GeomDraw",
                ggplot2::Geom,
                draw_group = draw_fn,
                draw_key = draw_key_draw
            )
        ),
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, draw = draw, params = list2(...))
    )
}

draw_fn <- function(data, panel_params, coord, draw, params) {
    if (is.grob(draw) || is.gList(draw)) {
        draw
    } else {
        draw <- rlang::as_function(draw)

        # restore width and height
        data$color <- data$colour
        inject(draw(data, panel_params, coord, !!!params))
    }
}

#' @inherit ggplot2::draw_key_point
#' @description
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`,
#' where `*` stands for the name of the respective key glyph. The key glyphs can
#' be customized for individual geoms by providing a geom with the `key_glyph`
#' argument. The `draw_key_draw` function provides this interface for custom key
#' glyphs used with [`geom_draw()`].
#'
#' @importFrom ggplot2 zeroGrob
#' @importFrom grid gTree
#' @export
draw_key_draw <- function(data, params, size) {
    ans <- try_fetch(
        draw_fn(data,
            panel_params = NULL, coord = NULL,
            draw = .subset2(params, "draw"),
            params = .subset2(params, "params")
        ),
        error = function(cnd) NULL
    )
    if (is.gList(ans)) ans <- gTree(children = ans)
    if (is.grob(ans)) {
        ans
    } else {
        zeroGrob()
    }
}
