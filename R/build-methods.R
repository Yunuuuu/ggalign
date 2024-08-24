#' Build `Layout` object for rendering.
#'
#' @param layout A [layout_heatmap()] or [layout_stack()] object.
#' @examples
#' build_alignpatches(ggheatmap(matrix(rnorm(100L), nrow = 10L)))
#' @export
#' @return A `alignpatches` object or `NULL` if no plots.
build_alignpatches <- function(layout) UseMethod("build_alignpatches")

#' @export
build_alignpatches.default <- function(layout) {
    cli::cli_abort("{.arg x} must be a {.cls Layout} object")
}

align_build <- function(x, panel, index,
                        extra_panel, extra_index,
                        plot_data, free_labs) {
    if (is.null(.subset2(x, "plot"))) {
        return(list(plot = NULL, size = NULL))
    }
    direction <- .subset2(x, "direction")

    x$lock()
    on.exit(x$unlock())

    # let `align` to determine how to draw
    # 1. add default layer
    # 2. add plot data
    params <- .subset2(x, "params")
    draw_params <- params[
        intersect(
            names(params),
            align_method_params(
                x$draw,
                c("panel", "index", "extra_panel", "extra_index")
            )
        )
    ]
    plot <- rlang::inject(x$draw(
        panel, index, extra_panel, extra_index,
        !!!draw_params
    ))
    plot_data <- .subset2(x, "plot_data") %|w|% plot_data %|w|% NULL
    free_labs <- .subset2(x, "free_labs") %|w|% free_labs %|w|%
        switch_direction(direction, c("t", "b"), c("l", "r"))
    plot <- finish_plot_data(plot, plot_data, call = .subset2(x, "call"))

    # only when user use the internal facet, we'll
    # set up the scale limits, breaks and labels
    if (.subset2(x, "facet")) {
        # set up scales and facet
        if (nlevels(panel) > 1L) {
            default_facet <- switch_direction(
                direction,
                ggplot2::facet_grid(
                    rows = ggplot2::vars(fct_rev(.data$.panel)),
                    scales = "free_y", space = "free_y",
                    drop = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free_x", space = "free_x",
                    drop = FALSE
                )
            )
        } else {
            # No facet
            default_facet <- NULL
        }
        scales <- set_scales(
            plot = plot,
            scale_name = to_coord_axis(direction),
            panel = panel,
            index = index,
            layout_labels = .subset2(x, "labels"),
            facet_scales = .subset2(x, "facetted_pos_scales"),
            set_limits = .subset2(x, "limits")
        )
        plot <- remove_scales(plot, .subset2(scales, 1L)$aesthetics)
        plot <- align_set_scales_and_facet(
            plot = plot, direction = direction,
            scales = scales, default_facet = default_facet
        )
    }
    list(plot = free_lab(plot, free_labs), size = .subset2(x, "size"))
}

finish_plot_data <- function(plot, plot_data,
                             data = .subset2(plot, "data"),
                             call = caller_call()) {
    if (!is.null(plot_data)) {
        if (!is.data.frame(data <- plot_data(data))) {
            cli::cli_abort(
                "{.arg plot_data} must return a {.cls data.frame}",
                call = call
            )
        }
        plot$data <- data
    } else {
        plot$data <- data
    }
    plot
}

align_set_scales_and_facet <- function(plot, direction, scales, default_facet) {
    facet <- melt_facet(.subset2(plot, "facet"), default_facet)
    if (is.null(default_facet)) { # no panel
        plot <- plot + scales + facet
    } else {
        plot <- plot + facet +
            switch_direction(
                direction,
                ggh4x::facetted_pos_scales(y = scales),
                ggh4x::facetted_pos_scales(x = scales)
            )
    }
    plot
}

#' @param layout_labels A character of the data names.
#' @return A list of scales for each panel
#' @noRd
set_scales <- function(plot, scale_name, panel, index,
                       layout_labels, facet_scales,
                       set_limits = TRUE,
                       expand = ggplot2::expansion()) {
    panel <- panel[index]

    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (scale_name == "y") panel <- fct_rev(panel)
    n_panel <- nlevels(panel)

    # By default we'll use the continuous scale
    use_discrete <- FALSE

    # the single scale control the labels and breaks
    # and determine the global theme
    if (!is.null(global_scale <- plot$scales$get_scales(scale_name))) {
        use_discrete <- global_scale$is_discrete()
    }

    user_scales <- rep_len(list(NULL), n_panel)
    if (n_panel > 1L &&
        !is.null(facet_scales) &&
        !is_empty(user_scales <- .subset2(facet_scales, scale_name))) {
        # if the global scale was not provided,
        # we test if the first facetted scale is discrete
        if (is.null(global_scale)) {
            for (scale in user_scales) {
                if (inherits(scale, "Scale")) {
                    use_discrete <- scale$is_discrete()
                    break
                }
            }
        }
    }

    # set the default global scale
    if (is.null(global_scale)) {
        if (use_discrete) {
            global_scale <- switch(scale_name,
                x = ggplot2::scale_x_discrete(),
                y = ggplot2::scale_y_discrete()
            )
        } else {
            global_scale <- switch(scale_name,
                x = ggplot2::scale_x_continuous(),
                y = ggplot2::scale_y_continuous()
            )
        }
    }

    # we always use the discrete scale to determine labels and breaks
    # https://github.com/tidyverse/ggplot2/blob/7fb4c382f9ea332844d469663a8047355a88dd7a/R/scale-.R#L927
    if (is.null(layout_labels) &&
        is.waive(global_scale$labels) &&
        is.waive(global_scale$breaks)) {
        # special case for data have no layout labels
        # By default we remove breaks and labels
        breaks <- NULL
        labels <- NULL
    } else {
        breaks <- get_breaks(global_scale, seq_along(index), layout_labels)
        labels <- get_labels(global_scale, breaks, layout_labels)
    }

    # fill NULL with the global scale --------------------
    if (is.null(names(user_scales))) {
        ids <- seq_len(nlevels(panel))
    } else {
        ids <- levels(panel)
    }
    for (id in ids) {
        if (is.null(scale <- .subset2(user_scales, id))) {
            user_scales[[id]] <- global_scale$clone()
        } else {
            user_scales[[id]] <- scale$clone()
        }
    }
    if (!is.list(expand)) expand <- rep_len(list(expand), n_panel)
    # we always reset the limits of the user provided scales
    .mapply(function(scale, data, e) {
        data_index <- .subset2(data, "index")
        plot_coord <- .subset2(data, "coord")

        # setup limits -------------------------------
        if (set_limits) {
            # always reset the limits
            if (use_discrete) {
                # the labels of `plot_coord` is the data index
                # See `heatmap_build_data`
                scale$limits <- plot_coord
            } else {
                scale$limits <- range(plot_coord) + c(-0.5, 0.5)
            }
        }

        # setup breaks and labels --------------------
        in_domain <- match(data_index, breaks)
        keep <- !is.na(in_domain)
        scale$breaks <- plot_coord[keep]
        scale$labels <- labels[in_domain[keep]]

        # if user provides expand, we'll use it, otherwise, use the default
        if (is.waive(scale$expand)) scale$expand <- e
        scale
    }, list(
        scale = user_scales,
        data = split(
            data_frame0(coord = seq_along(index), index = index),
            panel
        ),
        e = expand
    ), NULL)
}

get_breaks <- function(scale, layout_limits, layout_labels) {
    breaks <- scale$breaks
    if (identical(breaks, NA)) {
        cli::cli_abort(c(
            "Invalid {.arg breaks} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }
    if (is.null(breaks)) {
        return(NULL)
    }

    if (is.waive(breaks)) {
        breaks <- layout_limits
    } else {
        if (is.function(breaks)) {
            breaks <- breaks(layout_labels %||% layout_limits)
        }

        if (is.factor(breaks) || is.character(breaks)) {
            # we interpreted the character breaks as the names of the original
            # matrix data.
            breaks <- layout_limits[
                match(as.character(breaks), layout_labels %||% layout_limits)
            ]
        } else if (is.numeric(breaks)) {
            # we interpreted the numeric breaks as the index of the original
            # matrix data
            breaks <- as.integer(breaks)
        } else {
            return(NULL)
        }
    }

    # Breaks only occur only on values in domain
    in_domain <- intersect(breaks, layout_limits)
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
        cli::cli_abort(c(
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

remove_scales <- function(plot, scale_aesthetics) {
    scales <- .subset2(plot, "scales")$clone()
    if (any(prev_aes <- scales$find(scale_aesthetics))) {
        scales$scales <- scales$scales[!prev_aes]
    }
    plot$scales <- scales
    plot
}

#' @importFrom rlang is_empty
extract_scales <- function(plot, axis, n_panel, facet_scales) {
    # if no facets, or if no facet scales, we replicate the single scale
    # object to match the panel numbers
    if (n_panel > 1L &&
        !is.null(facet_scales) &&
        !is_empty(ans <- .subset2(facet_scales, axis))) {
    } else {
        ans <- rep_len(list(plot$scales$get_scales(axis)), n_panel)
    }
    ans
}

melt_facet <- function(user_facet, default_facet) {
    if (is.null(default_facet)) { # no panel
        # we only support `FacetNull` if there have no panel
        if (inherits(user_facet, "FacetNull")) return(user_facet) # styler: off
        return(ggplot2::facet_null())
    }
    # we only support `FacetGrid` if there have multiple panel
    if (!inherits(user_facet, "FacetGrid")) return(default_facet) # styler: off

    # will change the user input, so we must use `ggproto_clone`
    user_facet <- ggproto_clone(user_facet)

    # we always fix the grid rows and cols
    user_facet$params$rows <- default_facet$params$rows
    user_facet$params$cols <- default_facet$params$cols
    user_facet$params$drop <- default_facet$params$drop

    # if the default is free, it must be free
    user_facet$params$free$x <- user_facet$params$free$x ||
        default_facet$params$free$x
    user_facet$params$space_free$x <- user_facet$params$space_free$x ||
        default_facet$params$space_free$x
    user_facet$params$free$y <- user_facet$params$free$y ||
        default_facet$params$free$y
    user_facet$params$space_free$y <- user_facet$params$space_free$x ||
        default_facet$params$space_free$y
    user_facet
}
