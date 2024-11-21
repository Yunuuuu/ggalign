ggfun <- local({
    ggplot2_namespace <- NULL
    function(x, mode = "any") {
        if (is.null(ggplot2_namespace)) {
            ggplot2_namespace <<- getNamespace("ggplot2")
        }
        get(x, envir = ggplot2_namespace, inherits = FALSE, mode = mode)
    }
})

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) rlang::as_function(x) else x
}

is.waive <- function(x) inherits(x, "waiver")

`%|w|%` <- function(x, y) if (inherits(x, "waiver")) y else x

snake_class <- function(x) ggfun("snake_class")(x)

add_default_mapping <- function(mapping, default_mapping) {
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    default_mapping
}

#######################################################################
# ggplot2 add default scales in `compute_aesthetics` process
# then ggplot2 transform all scales
#  layout:
#   in ggplot_build
#    - `setup`:
#       - call `facet$setup_params`
#       - attach `plot_env`
#       - call `facet$setup_data`
#       - call `facet$compute_layout`
#       - call `coord$setup_layout`
#       - call `facet$map_data`
#    - `train_position` call `init_scales`
#    - `setup_panel_params`
#    - `map_position`
#    - `setup_panel_guides`
#  in ggplot_gtable
#    - `render`:
#         - call `coord$draw_panel` for each panel
#         - call `facet$draw_panels`: only once
#           - call `facet$init_gtable`:
#           - call `facet$attach_axes`:
#             - call `coord$render_axis_h`:
#               - call `guide$draw`:
#             - call `coord$render_axis_v`:
#               - call `guide$draw`:
#           - call `facet$attach_strips`:

#' Set limits for each panel
#'
#' @keywords internal
#' @noRd
coord_ggalign <- function(xlim_list = NULL, ylim_list = NULL) {
    if (!is.null(xlim_list) && !is.list(xlim_list)) xlim_list <- list(xlim_list)
    if (!is.null(ylim_list) && !is.list(ylim_list)) ylim_list <- list(ylim_list)
    structure(
        list(xlim_list = xlim_list, ylim_list = ylim_list),
        class = "coord_ggalign"
    )
}

setup_limits <- function(axis, params) {
    panel <- .subset2(params, "panel")
    index <- .subset2(params, "index")

    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (axis == "y") panel <- fct_rev(panel)
    lapply(split(seq_along(index), panel), function(plot_coord) {
        range(plot_coord) + c(-0.5, 0.5)
    })
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.coord_ggalign <- function(object, plot, object_name) {
    if (all(vapply(object, is.null, logical(1L), USE.NAMES = FALSE))) {
        return(plot)
    }
    Parent <- .subset2(plot, "coordinates")
    plot$coordinates <- ggproto(
        NULL, Parent,
        num_of_panels = NULL,
        panel_counter = NULL,
        setup_layout = function(self, layout, params) {
            # we always initialize the number of panels and a panel counter
            self$num_of_panels <- vec_unique_count(.subset2(layout, "PANEL"))
            self$panel_counter <- 0L
            # call the parent method
            ggproto_parent(Parent, self)$setup_layout(layout, params)
        },
        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            cur_panel <- self$panel_counter + 1L
            if (!is.null(xlim_list <- .subset2(object, "xlim_list")) &&
                length(xlim_list) >= cur_panel) {
                if (scale_x$is_discrete()) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    self$limits$x <- .subset2(xlim_list, cur_panel) -
                        (min(.subset2(xlim_list, cur_panel)) - 0.5)
                    # we reset the breaks into character breaks
                    # we match it by the original labels
                    domain <- intersect(scale_x$labels, scale_x$get_limits())
                    keep <- match(domain, scale_x$labels)
                    scale_x$labels <- scale_x$breaks <- scale_x$labels[keep]
                } else {
                    self$limits$x <- .subset2(xlim_list, cur_panel)
                }
            }
            if (!is.null(ylim_list <- .subset2(object, "ylim_list")) &&
                length(ylim_list) >= cur_panel) {
                if (scale_y$is_discrete()) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    self$limits$y <- .subset2(ylim_list, cur_panel) -
                        (min(.subset2(ylim_list, cur_panel)) - 0.5)
                    # we reset the breaks into character breaks
                    # we match it by the original labels
                    domain <- intersect(scale_y$labels, scale_y$get_limits())
                    keep <- match(domain, scale_y$labels)
                    scale_y$labels <- scale_y$breaks <- scale_y$labels[keep]
                } else {
                    self$limits$y <- .subset2(ylim_list, cur_panel)
                }
            }
            self$panel_counter <- cur_panel
            ggproto_parent(Parent, self)$setup_panel_params(
                scale_x = scale_x, scale_y = scale_y, params = params
            )
        }
    )
    plot
}

###############################################################
#' Used to match scales and theme
#'
#' @keywords internal
#' @noRd
facet_ggalign <- function(x = NULL, y = NULL) {
    structure(list(x = x, y = y), class = "facet_ggalign")
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.facet_ggalign <- function(object, plot, object_name) {
    if (is.null(.subset2(object, "x")) && is.null(.subset2(object, "y"))) {
        return(plot) # Do nothing
    }
    Parent <- .subset2(plot, "facet")
    plot$facet <- ggproto(
        NULL, Parent,
        init_scales = function(self, layout, x_scale = NULL, y_scale = NULL,
                               params) {
            scales <- ggproto_parent(Parent, self)$init_scales(
                layout, x_scale, y_scale, params
            )
            # for each scale, we set the `breaks` and `labels`
            if (!is.null(x_scale) &&
                !is.null(x_params <- .subset2(object, "x"))) {
                scales$x <- align_scales(
                    self, x_scale, "x", x_params,
                    panel_scales = .subset2(scales, "x")
                )
            }
            if (!is.null(y_scale) &&
                !is.null(y_params <- .subset2(object, "y"))) {
                scales$y <- align_scales(
                    self, y_scale, "y", y_params,
                    panel_scales = .subset2(scales, "y")
                )
            }
            scales
        },
        draw_panels = function(self, panels, layout,
                               x_scales = NULL, y_scales = NULL,
                               ranges, coord, data = NULL, theme, params) {
            if (!is.null(x_params <- .subset2(object, "x"))) {
                theme <- recycle_theme(
                    "x", theme,
                    self[["_x_breaks"]], self[["_x_labels"]],
                    x_params$index
                )
            }
            if (!is.null(y_params <- .subset2(object, "y"))) {
                theme <- recycle_theme(
                    "y", theme,
                    self[["_y_breaks"]], self[["_y_labels"]],
                    y_params$index
                )
            }
            ParentCoord <- coord
            coord <- ggproto(NULL, ParentCoord,
                render_axis_h = function(self, panel_params, theme) {
                    if (!is.null(x_params)) {
                        x <- .subset2(panel_params, "x")$scale[[
                            "_ggalign_breaks"
                        ]]
                        theme <- subset_theme("x", theme, x)
                    }
                    ggproto_parent(ParentCoord, self)$render_axis_h(
                        panel_params, theme
                    )
                },
                render_axis_v = function(self, panel_params, theme) {
                    if (!is.null(y_params)) {
                        y <- .subset2(panel_params, "y")$scale[[
                            "_ggalign_breaks"
                        ]]
                        theme <- subset_theme("y", theme, y)
                    }
                    ggproto_parent(ParentCoord, self)$render_axis_v(
                        panel_params, theme
                    )
                }
            )


            ggproto_parent(Parent, self)$draw_panels(
                panels = panels, layout = layout,
                x_scales = x_scales, y_scales = y_scales,
                ranges = ranges, coord = coord, data = data,
                theme = theme, params = params
            )
        }
    )
    plot
}

align_scales <- function(facet, scale, axis, params, panel_scales) {
    panel <- .subset2(params, "panel")
    index <- .subset2(params, "index")
    labels <- .subset2(params, "labels")

    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (axis == "y") panel <- fct_rev(panel)

    # we always use the discrete scale to determine labels and breaks
    # https://github.com/tidyverse/ggplot2/blob/7fb4c382f9ea332844d469663a8047355a88dd7a/R/scale-.R#L927
    if (is.null(labels) && is.waive(scale$labels) && is.waive(scale$breaks)) {
        # special case for data have no labels
        # By default we remove breaks and labels
        breaks <- NULL
        labels <- NULL
    } else {
        breaks <- get_breaks(scale, seq_along(index), labels)
        labels <- get_labels(scale, breaks, labels)
    }
    # save the breaks and labels for usage of `recycle_theme()`
    facet[[paste("", axis, "breaks", sep = "_")]] <- breaks
    facet[[paste("", axis, "labels", sep = "_")]] <- labels

    # by default we elways remove any expansion
    default_expand <- ggplot2::expansion()
    # the global expand should be added in the two flank
    flank_expand <- scale$expand %|w|% default_expand

    # setup the expand for usage of each scale
    if (nlevels(panel) > 1L) {
        expand <- rep_len(list(default_expand), nlevels(panel))
        # for expand in the head
        expand[[1L]] <- c(vec_slice(flank_expand, 1:2), rep_len(0, 2L))
        # for expand in the tail
        expand[[nlevels(panel)]] <- c(
            rep_len(0, 2L), vec_slice(flank_expand, 3:4)
        )
    } else {
        expand <- list(flank_expand)
    }

    # fill `NULL` with the global scale --------------------
    .mapply(function(s, data_index, plot_coord, e) {
        if (is.null(breaks)) {
            s$breaks <- NULL
            s$labels <- NULL
        } else {
            # setup breaks and labels --------------------
            in_domain <- match(data_index, breaks)
            keep <- !is.na(in_domain)
            if (any(keep)) {
                # `_ggalign_breaks` used to match theme element values
                s[["_ggalign_breaks"]] <- s$breaks <- plot_coord[keep]
                s$labels <- labels[in_domain[keep]]
            } else {
                s$breaks <- NULL
                s$labels <- NULL
            }
        }
        s$expand <- e
        s
    }, list(
        s = panel_scales,
        data_index = split(index, panel),
        plot_coord = split(seq_along(index), panel),
        e = expand
    ), NULL)
}

recycle_theme <- function(axis, theme, titcks, labels, index) {
    align_theme(axis, theme,
        text_fn = function(v, arg) {
            if (inherits(v, "AsIs")) {
                rep(v, length.out = length(labels))
            } else {
                rep(v, length.out = length(labels))[index]
            }
        },
        tick_fn = function(v, arg) {
            if (inherits(v, "AsIs")) {
                rep(v, length.out = length(titcks))
            } else {
                rep(v, length.out = length(titcks))[index]
            }
        }
    )
}

subset_theme <- function(axis, theme, breaks) {
    align_theme(axis, theme, function(value) value[breaks])
}

align_theme <- function(axis, theme, fn, text_fn = fn, tick_fn = fn) {
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

identity_trans <- function(scale) {
    # for continuous scale, we don't allow the trans
    if (!scale$is_discrete() && !identical(scale$trans$name, "identity")) {
        cli_warn(sprintf(
            "{.arg trans} must be {.field identity} in {.code %s}",
            deparse(scale$call)
        ))
        scale$trans <- scales::as.transform("identity")
    }
    scale
}

get_breaks <- function(scale, layout_breaks, layout_labels) {
    breaks <- scale$breaks
    if (identical(breaks, NA)) {
        cli_abort(c(
            "Invalid {.arg breaks} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }
    if (is.null(breaks)) {
        return(NULL)
    }

    if (is.waive(breaks)) {
        breaks <- layout_breaks
    } else {
        if (is.function(breaks)) {
            breaks <- breaks(layout_labels %||% layout_breaks)
        }

        if (is.factor(breaks) || is.character(breaks)) {
            # we interpreted the character breaks as the names of the original
            # matrix data.
            breaks <- layout_breaks[
                match(as.character(breaks), layout_labels %||% layout_breaks)
            ]
        } else {
            # breaks must be a character or an integer
            breaks <- vec_cast(breaks, integer(), call = scale$call)
        }
    }

    # Breaks only occur only on values in domain
    in_domain <- intersect(breaks, layout_breaks)
    structure(in_domain, pos = match(in_domain, breaks))
}

#' @importFrom rlang is_empty
get_labels <- function(scale, breaks, layout_labels) {
    labels <- scale$labels
    if (is_empty(breaks)) { # if no breaks, no labels
        return(NULL)
    }

    if (is.null(labels)) {
        return(NULL)
    }

    if (identical(labels, NA)) {
        cli_abort(c(
            "Invalid {.arg labels} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }

    # if layout have no names, use the breaks directly
    if (!is.null(layout_labels)) {
        user_breaks <- layout_labels[breaks]
    } else {
        user_breaks <- breaks
    }

    if (is.waive(labels)) {
        user_breaks
    } else if (is.function(labels)) {
        labels(user_breaks)
    } else if (!is.null(names(labels))) {
        # If labels have names, use them to match with breaks
        map <- match(names(labels), user_breaks, nomatch = 0L)
        user_breaks[map] <- labels[map != 0L]
        user_breaks
    } else {
        # Need to ensure that if breaks were dropped, corresponding labels
        # are too
        if (!is.null(pos <- attr(breaks, "pos"))) {
            labels <- labels[pos]
        }
        labels
    }
}
