#' Used to match theme
#'
#' @keywords internal
#' @noRd
theme_recycle <- function() {
    structure(list(), class = "theme_recycle")
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.theme_recycle <- function(object, plot, object_name) {
    ParentFacet <- .subset2(plot, "facet")
    if (!inherits(ParentFacet, c("FacetGrid", "FacetWrap"))) {
        return(plot)
    }
    # recycle axis theme elements
    plot$facet <- ggproto(
        NULL, ParentFacet,
        draw_panels = function(self, panels, layout,
                               x_scales = NULL, y_scales = NULL,
                               ranges, coord, data = NULL, theme, params) {
            # we recycle the theme elements of the guide axis
            theme <- recycle_theme_axis("x", theme, x_scales)
            theme <- recycle_theme_axis("y", theme, y_scales)
            ParentCoord <- coord
            h_tick0 <- h_text0 <- 0L
            v_tick0 <- v_text0 <- 0L
            # subset theme for each panel
            coord <- ggproto(NULL, ParentCoord,
                # `align_scales` will attach the `.__plot_index__`
                render_axis_h = function(self, panel_params, theme) {
                    scale <- .subset2(panel_params, "x")$scale
                    h_tick1 <- h_tick0 + length(scale$get_breaks())
                    h_text1 <- h_text0 + length(scale$get_labels())
                    theme <- subset_theme_axis(
                        "x", theme, h_tick0, h_text0, h_tick1, h_text1
                    )
                    h_tick0 <<- h_tick1
                    h_text0 <<- h_text1
                    ggproto_parent(ParentCoord, self)$render_axis_h(
                        panel_params, theme
                    )
                },
                render_axis_v = function(self, panel_params, theme) {
                    scale <- .subset2(panel_params, "y")$scale
                    v_tick1 <- v_tick0 + length(scale$get_breaks())
                    v_text1 <- v_text0 + length(scale$get_labels())
                    theme <- subset_theme_axis(
                        "y", theme, v_tick0, v_text0, v_tick1, v_text1
                    )
                    v_tick0 <<- v_tick1
                    v_text0 <<- v_text1
                    ggproto_parent(ParentCoord, self)$render_axis_v(
                        panel_params, theme
                    )
                }
            )
            ggproto_parent(ParentFacet, self)$draw_panels(
                panels = panels, layout = layout,
                x_scales = x_scales, y_scales = y_scales,
                ranges = ranges, coord = coord, data = data,
                theme = theme, params = params
            )
        }
    )
    plot
}

#################################################################
recycle_theme_axis <- function(axis, theme, scales) {
    breaks <- unlist(lapply(scales, function(s) s$get_breaks()), FALSE, FALSE)
    labels <- unlist(lapply(scales, function(x) x$get_labels()), FALSE, FALSE)
    align_theme_axis(axis, theme,
        tick_fn = function(v, arg) rep(v, length.out = length(breaks)),
        text_fn = function(v, arg) rep(v, length.out = length(labels))
    )
}

subset_theme_axis <- function(axis, theme, tick0, text0, tick1, text1) {
    tick_index <- (tick0 + 1L):tick1
    text_index <- (text0 + 1L):text1
    align_theme_axis(
        axis, theme,
        tick_fn = function(value) value[tick_index],
        text_fn = function(value) value[text_index]
    )
}

align_theme_axis <- function(axis, theme, text_fn, tick_fn) {
    if (axis == "y") {
        positions <- c("left", "right")
    } else {
        positions <- c("top", "bottom")
    }
    # only apply the function when the element values is not a scalar
    .text_fn <- function(v) if (length(v) > 1L) text_fn(v) else v
    .tick_fn <- function(v) if (length(v) > 1L) tick_fn(v) else v
    for (element in paste("axis.text", axis, positions, sep = ".")) {
        theme <- theme_element_lapply(
            .theme = theme, .element = element, .class = "element_text",
            .params = c(
                "family", "face", "colour", "size", "hjust", "vjust",
                "angle", "lineheight", "color"
            ),
            .fn = .text_fn
        )
    }
    for (element in paste("axis.ticks", axis, positions, sep = ".")) {
        theme <- theme_element_lapply(
            .theme = theme, .element = element, .class = "element_line",
            .params = c("colour", "linewidth", "linetype", "lineend", "color"),
            .fn = .tick_fn
        )
    }
    for (element in paste("axis.ticks.length", axis, positions, sep = ".")) {
        theme <- theme_element_lapply(
            .theme = theme, .element = element, .fn = .tick_fn
        )
    }
    theme
}

##########################################################################
#' Apply a function to each parameters of the element object in the theme
#'
#' @importFrom ggplot2 calc_element
#' @noRd
theme_element_lapply <- function(.theme, .element, .fn, ...,
                                 .class = NULL, .params = NULL) {
    el <- calc_element(.element, .theme)
    if (is.null(.class)) { # the element should be the value
        .theme[[.element]] <- .fn(el, ...)
    } else if (inherits(el, .class)) {
        # recusively to the value of the element
        for (e in (.params %||% names(el))) {
            el[[e]] <- .fn(.subset2(el, e), ...)
        }
        .theme[[.element]] <- el
    }
    .theme
}
