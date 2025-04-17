#' Layer with Grid or Function
#'
#' Draw a ggplot2 layer using a grob or a function.
#'
#' @param draw Either a [grob][grid::grob] object or a function (can be
#'   purrr-style) that accepts at least one argument (a data frame of
#'   transformed coordinates) and returns a [grob][grid::grob].
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
#' @importFrom ggplot2 ggproto aes
#' @export
geom_draw <- function(draw, mapping = NULL, data = NULL,
                      type = "group", stat = "identity",
                      position = "identity", ...,
                      na.rm = FALSE, show.legend = FALSE, inherit.aes = TRUE) {
    type <- arg_match0(type, c("group", "panel"))
    if (!is.grob(draw) && !is.gList(draw)) draw <- rlang::as_function(draw)
    dots <- list2(...)
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = switch(type,
            panel = ggproto(
                "GeomDraw",
                ggplot2::Geom,
                default_aes = GeomGshape$default_aes,
                setup_data = draw_setup_data,
                draw_panel = draw_geom_draw,
                draw_key = ggplot2::draw_key_blank
            ),
            group = ggproto(
                "GeomDraw",
                ggplot2::Geom,
                default_aes = GeomGshape$default_aes,
                setup_data = draw_setup_data,
                draw_group = draw_geom_draw,
                draw_key = ggplot2::draw_key_blank
            )
        ),
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = c(
            list(na.rm = na.rm, draw = draw, .__draw_dots__ = dots), dots
        )
    )
}

# GeomTile will respect width and height
draw_setup_data <- function(self, data, params) {
    data$x <- data$x %||% 0.5
    data$y <- data$y %||% 0.5
    ggplot2::GeomTile$setup_data(data, params)
}

draw_geom_draw <- function(data, panel_params, coord, draw, .__draw_dots__) {
    if (is.function(draw)) {
        data <- coord$transform(data, panel_params)
        # restore colour
        if (!is.null(data$colour) && is.null(data$color)) {
            data$color <- data$colour
        }
        if (!is.null(data$color) && is.null(data$colour)) {
            data$colour <- data$color
        }

        # restore width and height
        if (!is.null(data$xmin) && !is.null(data$xmax)) {
            data$width <- data$xmax - data$xmin
        }
        if (!is.null(data$ymin) && !is.null(data$ymax)) {
            data$height <- data$ymax - data$ymin
        }
    }
    make_draw_grob(draw, data, .__draw_dots__)
}

#' @return A [grob][grid::grob] object.
#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom ggplot2 zeroGrob
#' @keywords internal
#' @noRd
make_draw_grob <- function(draw, data, dots) {
    if (is.function(draw)) {
        args <- formalArgs(draw)
        if (any(args == "...")) {
            draw <- inject(draw(!!!data, !!!dots))
        } else {
            draw <- inject(draw(
                !!!.subset(data, intersect(names(data), args)),
                !!!.subset(dots, intersect(
                    setdiff(names(dots), names(data)), args
                ))
            ))
        }
    }
    ensure_grob(draw, zeroGrob())
}
