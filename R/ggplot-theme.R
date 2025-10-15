#' @importFrom ggplot2 theme_bw element_blank
default_theme <- function() {
    if (is_theme_unset()) {
        theme_bw() +
            theme(
                axis.text = ggplot2::element_text(color = "black"),
                strip.text = ggplot2::element_text(color = "black"),
                panel.border = element_blank(),
                panel.grid = element_blank()
            )
    } else {
        theme_get()
    }
}

# nocov start
#' @importFrom ggplot2 theme
new_theme_class <- function(new_class) {
    rlang::new_function(
        # We utilize editor completion by listing all `theme()` arguments here.
        # By placing `...` at the beginning, we can check if the first
        # following argument is a `theme()` object rather than individual theme
        # elements.
        c(
            rlang::exprs(... = ),
            .subset(
                rlang::fn_fmls(theme),
                vec_set_difference(names(rlang::fn_fmls(theme)), "...")
            )
        ),
        quote({
            elements <- ggfun("find_args")(..., complete = NULL, validate = NULL)
            ans <- theme(!!!elements)
            th <- NULL
            for (i in seq_len(...length())) {
                if (inherits(t <- ...elt(i), "theme")) {
                    th <- ggfun("add_theme")(th, t)
                }
            }
            add_class(ggfun("add_theme")(th, ans), new_class)
        })
    )
}
# nocov end

S3_class_theme <- S7::new_S3_class("theme")

# Check if user has set the theme
#' @importFrom ggplot2 complete_theme
is_theme_unset <- function() {
    isTRUE(all.equal(
        complete_theme(ggfun("ggplot_global")$theme_default),
        complete_theme(theme_get())
    ))
}

is_theme_complete <- function(x) isTRUE(attr(x, "complete", exact = TRUE))

#' @importFrom ggplot2 rel element_blank
theme_no_strip <- function() {
    theme(
        strip.text = element_blank(),
        strip.background = element_blank()
    )
}

#' @importFrom ggplot2 element_rect
theme_panel_border <- function() theme(panel.border = element_rect(fill = NA))

# nocov start
#' @importFrom ggplot2 register_theme_elements el_def
#' @importFrom ggplot2 element_line element_polygon element_text
theme_elements <- function() {
    register_theme_elements(
        ggalign.line = element_line(
            color = "black",
            linewidth = 0.5,
            linetype = 1,
            lineend = "butt",
            linejoin = "round"
        ),
        ggalign.polygon = element_polygon(
            fill = NA,
            colour = "black",
            linewidth = 0.5,
            linetype = 1,
            linejoin = "round"
        ),
        element_tree = list(
            plot.patch_title = el_def(element_text, "text"),
            plot.patch_title.top = el_def(element_text, "text"),
            plot.patch_title.left = el_def(element_text, "text"),
            plot.patch_title.bottom = el_def(element_text, "text"),
            plot.patch_title.right = el_def(element_text, "text"),
            plot.patch_title.position = el_def("character"),
            plot.patch_title.position.top = el_def("character"),
            plot.patch_title.position.left = el_def("character"),
            plot.patch_title.position.bottom = el_def("character"),
            plot.patch_title.position.right = el_def("character"),
            panel.spacing.r = el_def(c("unit", "rel"), "panel.spacing"),
            ggalign.line = el_def(element_line),
            ggalign.polygon = el_def(element_polygon)
        )
    )
}
# nocov end

#' Used to match theme
#'
#' @keywords internal
#' @noRd
theme_recycle <- S7::new_class("theme_recycle")

#' @importFrom ggplot2 update_ggplot ggproto ggproto_parent
S7::method(update_ggplot, list(theme_recycle, ggplot2::class_ggplot)) <-
    function(object, plot, objectname, ...) {
        ParentFacet <- plot$facet
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
                        scale <- (.subset2(panel_params, "x") %||%
                            .subset2(panel_params, "theta")
                        )$scale
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
                        scale <- (.subset2(panel_params, "y") %||%
                            .subset2(panel_params, "r")
                        )$scale
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
# Apply a function to the vectorized field of the theme object
#' @importFrom ggplot2 calc_element
theme_vec <- function(.th, .el, .fn, ...) {
    element <- calc_element(.el, .th)
    if (inherits(element, "element")) {
        .th[[.el]] <- element_vec(element, .fn, ...)
    } else if (!is.null(element)) {
        .th[[.el]] <- .fn(element, ...)
    }
    .th
}

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
        tick_fn = function(value) vec_slice(value, tick_index),
        text_fn = function(value) vec_slice(value, text_index)
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
        theme <- theme_vec(theme, element, .text_fn)
    }
    for (element in paste("axis.ticks", axis, positions, sep = ".")) {
        theme <- theme_vec(theme, element, .tick_fn)
    }
    for (element in paste("axis.ticks.length", axis, positions, sep = ".")) {
        theme <- theme_vec(theme, element, .tick_fn)
    }
    theme
}
