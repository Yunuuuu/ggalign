#' Theme for Layout Plots
#'
#' Default theme for `r rd_layout()`.
#'
#' @details
#' You can change the default theme using the option
#' `r code_quote(sprintf("%s.default_theme", pkg_nm()))`. This option should be
#' set to a function that returns a [`theme()`][ggplot2::theme] object.
#'
#' @inheritDotParams ggplot2::theme_classic
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples
#' # Setting a new default theme
#' old <- options(ggalign.default_theme = function() theme_bw())
#'
#' # Creating a heatmap with the new theme
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro(k = 3L)
#'
#' # Restoring the old default theme
#' options(old)
#' @importFrom ggplot2 theme_classic
#' @export
theme_ggalign <- function(...) {
    theme_classic(...) +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank(),
            plot.background = element_blank()
        )
}

default_theme <- function() {
    opt <- sprintf("%s.default_theme", pkg_nm())
    if (is.null(ans <- getOption(opt, default = NULL))) {
        return(theme_ggalign())
    }
    if (is.function(ans <- allow_lambda(ans))) {
        if (!inherits(ans <- rlang::exec(ans), "theme")) {
            cli_abort(c(
                "{.arg {opt}} must return a {.fn theme} object",
                i = "You have provided {.obj_type_friendly {ans}}"
            ))
        }
    } else {
        cli_abort(c(
            "{.arg {opt}} must be a {.cls function}",
            i = "You have provided {.obj_type_friendly {ans}}"
        ))
    }
    ans
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
#' @importFrom rlang arg_match0 inject
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
#' @importFrom ggplot2 theme_get
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
#' @seealso [`element_rect`][ggplot2::element_rect]
#' @return A `element_polygon` object
#' @export
element_polygon <- function(fill = NULL, colour = NULL, linewidth = NULL,
                            linetype = NULL, color = NULL,
                            inherit.blank = FALSE) {
    if (!is.null(color)) colour <- color
    structure(
        list(
            fill = fill, colour = colour, linewidth = linewidth,
            linetype = linetype, inherit.blank = inherit.blank
        ),
        class = c("element_polygon", "element")
    )
}

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
        fill = element$fill,
        lty = element$linetype
    )
    grid::polygonGrob(
        x = x, y = y,
        gp = ggfun("modify_list")(element_gp, gp), ...
    )
}

#' @importFrom ggplot2 register_theme_elements el_def
theme_elements <- function() {
    register_theme_elements(
        plot.ggalign_ranges = element_polygon(
            fill = NA,
            color = "black",
            linewidth = 0.5,
            linetype = 1
        ),
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
            plot.ggalign_ranges = el_def("element_polygon")
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
