#' @importFrom ggplot2 theme_classic
default_theme <- function() theme_classic()

#' @importFrom ggplot2 rel element_line element_rect element_text
theme_add_panel <- function(base_size = 11) {
    half_line <- base_size / 2
    theme(
        panel.border = element_rect(fill = NA, colour = "grey20"),
        panel.grid = element_line(colour = "grey92"),
        panel.grid.minor = element_line(linewidth = rel(0.5)),
        panel.background = element_rect(fill = "white", colour = NA),
        strip.background = element_rect(
            fill = "white", colour = "black", linewidth = rel(2)
        ),
        strip.clip = "inherit",
        strip.text = element_text(
            colour = "grey10", size = rel(0.8),
            margin = margin(
                0.8 * half_line, 0.8 * half_line,
                0.8 * half_line, 0.8 * half_line
            )
        ),
        strip.text.x = NULL,
        strip.text.y = element_text(angle = -90),
        strip.text.y.left = element_text(angle = 90)
    )
}

#' @importFrom ggplot2 element_blank
theme_no_panel <- function(...) {
    theme(
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_blank(),
        strip.text = element_blank(),
        strip.background = element_blank(),
        plot.background = element_blank()
    )
}

#' Remove axis elements
#'
#' @param axes Which axes elements should be removed? A string containing
#' one or more of `r oxford_and(c(.tlbr, "x", "y"))`.
#' @param text If `TRUE`, will remove the axis labels.
#' @param ticks If `TRUE`, will remove the axis ticks.
#' @param title If `TRUE`, will remove the axis title.
#' @param line If `TRUE`, will remove the axis line.
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples
#' p <- ggplot() +
#'     geom_point(aes(x = wt, y = qsec), data = mtcars)
#' p + theme_no_axes()
#' p + theme_no_axes("b")
#' p + theme_no_axes("l")
#' @importFrom rlang inject
#' @importFrom ggplot2 theme element_blank
#' @export
theme_no_axes <- function(axes = "xy", text = TRUE, ticks = TRUE,
                          title = TRUE, line = FALSE) {
    assert_string(axes, empty_ok = FALSE)
    if (grepl("[^tlbrxy]", axes)) {
        cli_abort(sprintf(
            "{.arg axes} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    }
    axes <- split_position(axes)
    el <- list(text = text, ticks = ticks, title = title, line = line)
    el <- names(el)[vapply(el, isTRUE, logical(1L), USE.NAMES = FALSE)]
    el_axis <- el_pos <- NULL
    if (length(positions <- vec_set_intersect(axes, .tlbr))) {
        positions <- .subset(
            c(t = "top", l = "left", b = "bottom", r = "right"),
            positions
        )
        el_pos <- vec_expand_grid(pos = positions, el = el)
        el_pos <- paste("axis",
            .subset2(el_pos, "el"),
            if_else(.subset2(el_pos, "pos") %in% c("top", "bottom"), "x", "y"),
            .subset2(el_pos, "pos"),
            sep = "."
        )
    }
    if (length(axes <- vec_set_intersect(axes, c("x", "y")))) {
        el_axis <- vec_expand_grid(axes = axes, el = el)
        el_axis <- paste("axis",
            .subset2(el_axis, "el"), .subset2(el_axis, "axes"),
            sep = "."
        )
    }
    el <- c(el_axis, el_pos)
    el <- vec_set_names(vec_rep(list(element_blank()), length(el)), el)
    inject(theme(!!!el, validate = FALSE))
}

#' @importFrom rlang try_fetch
complete_theme <- function(theme) {
    try_fetch(
        ggfun("complete_theme")(theme),
        error = function(cnd) ggfun("plot_theme")(list(theme = theme))
    )
}

is_theme_complete <- function(x) isTRUE(attr(x, "complete", exact = TRUE))

#' Theme Polygon elements
#'
#' Draw polygon.
#'
#' @inheritParams ggplot2::element_rect
#' @inheritParams geom_rect3d
#' @inheritParams ggplot2::fill_alpha
#' @seealso [`element_rect`][ggplot2::element_rect]
#' @return A `element_polygon` object
#' @export
element_polygon <- function(fill = NULL, colour = NULL, linewidth = NULL,
                            linetype = NULL, alpha = NULL, lineend = NULL,
                            linejoin = NULL, linemitre = NULL, color = NULL,
                            inherit.blank = FALSE) {
    if (!is.null(color)) colour <- color
    structure(
        list(
            fill = fill, colour = colour, alpha = alpha,
            linewidth = linewidth, linetype = linetype,
            lineend = lineend, linejoin = linejoin, linemitre = linemitre,
            inherit.blank = inherit.blank
        ),
        class = c("element_polygon", "element")
    )
}

#' @importFrom grid gpar
#' @importFrom ggplot2 element_grob
#' @export
element_grob.element_polygon <- function(element, x, y,
                                         width = 1, height = 1, fill = NULL,
                                         colour = NULL, linewidth = NULL, linetype = NULL, ...) {
    gp <- gpar(
        lwd = ggfun("len0_null")(linewidth * .pt),
        col = colour,
        fill = fill,
        lty = linetype
    )
    element_gp <- gpar(
        lwd = ggfun("len0_null")(element$linewidth * .pt),
        col = element$colour,
        fill = fill_alpha(element$fill, element$alpha %||% NA),
        lty = element$linetype,
        lineend = element$lineend,
        linejoin = element$linejoin,
        linemitre = element$linemitre
    )
    grid::polygonGrob(
        x = x, y = y,
        gp = ggfun("modify_list")(element_gp, gp), ...
    )
}

#' @importFrom ggplot2 register_theme_elements el_def element_line
theme_elements <- function() {
    register_theme_elements(
        element_tree = list(
            plot.patch_title = el_def("element_text", "text"),
            plot.patch_title.top = el_def("element_text", "text"),
            plot.patch_title.left = el_def("element_text", "text"),
            plot.patch_title.bottom = el_def("element_text", "text"),
            plot.patch_title.right = el_def("element_text", "text"),
            plot.patch_title.position = el_def("character"),
            plot.patch_title.position.top = el_def("character"),
            plot.patch_title.position.left = el_def("character"),
            plot.patch_title.position.bottom = el_def("character"),
            plot.patch_title.position.right = el_def("character"),
            plot.ggalign_ranges = el_def("element_polygon"),
            plot.ggalign_lines = el_def("element_line"),
            panel.spacing.r = el_def(c("unit", "rel"), "panel.spacing")
        )
    )
}

#' Used to match theme
#'
#' @keywords internal
#' @noRd
theme_recycle <- function() structure(list(), class = "theme_recycle")

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
