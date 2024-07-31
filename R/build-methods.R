#' @importFrom ggplot2 theme element_blank
#' @export
align_build <- function(x, panels, index) {
    ans <- list(size = .subset2(x, "size"))
    if (is.null(.subset2(x, "plot"))) {
        return(c(list(plot = NULL), ans))
    }
    direction <- .subset2(x, "direction")
    # set up default scale and facet
    if (nlevels(panels) > 1L) {
        default_facet <- switch_direction(
            direction,
            ggplot2::facet_grid(
                rows = ggplot2::vars(fct_rev(.data$.panel)),
                scales = "free_y", space = "free_y"
            ),
            ggplot2::facet_grid(
                cols = ggplot2::vars(.data$.panel),
                scales = "free_x", space = "free_x"
            )
        )
    } else {
        # No facet
        default_facet <- NULL
    }
    # we merge `labels` and `labels_nudge`
    default_scales <- set_default_scales(
        scale_name = to_coord_axis(direction),
        panels = panels,
        index = index,
        labels = .subset2(x, "labels"),
        nudge = .subset2(x, "labels_nudge")
    )
    x$lock()
    on.exit(x$unlock())
    # let `align` to determine how to draw
    # 1. add default layer
    # 2. add plot data
    params <- .subset2(x, "params")
    draw_params <- params[
        intersect(names(params), align_method_params(x$draw))
    ]
    plot <- rlang::inject(x$draw(panels, index, !!!draw_params))

    # in the finally, we ensure the scale limits is the same across all plots
    plot <- align_set_scales_and_facet(
        plot = plot, direction,
        .subset2(x, "facetted_pos_scales"),
        default_scales, default_facet
    )
    c(list(plot = plot), ans)
}

align_set_scales_and_facet <- function(plot, direction,
                                       facetted_pos_scales,
                                       default_scales,
                                       default_facet) {
    user_scales <- extract_scales(
        plot = plot, to_coord_axis(direction),
        n_panels = length(default_scales),
        facet_scales = facetted_pos_scales
    )
    for (i in seq_along(default_scales)) {
        user_scales[[i]] <- melt_scale(
            .subset2(user_scales, i),
            .subset2(default_scales, i)
        )
    }
    user_facet <- melt_facet(.subset2(plot, "facet"), default_facet)
    if (is.null(default_facet)) { # no panels
        user_scales[[1L]]$aesthetics
        plot <- plot + user_scales + user_facet
    } else {
        plot <- plot + user_facet +
            switch_direction(
                direction,
                ggh4x::facetted_pos_scales(y = user_scales),
                ggh4x::facetted_pos_scales(x = user_scales)
            )
    }
    plot
}

melt_scale <- function(user_scale, default_scale) {
    if (is.null(user_scale)) {
        ans <- default_scale$clone()
    } else {
        ans <- user_scale$clone()
        # always reset the limits
        ans$limits <- default_scale$limits

        # it's not possible for user to know the `breaks` or `labels`
        # since we'll reorder the observations.
        # so we always set the breaks or labels into the default if user not
        # remove it.
        if (!is.null(ans$breaks)) {
            ans$breaks <- default_scale$breaks
        }

        if (!is.null(ans$labels)) {
            ans$labels <- default_scale$labels
        }

        # if user provides expand, we'll use it, otherwise, use the default
        if (is.waiver(ans$expand)) {
            ans$expand <- default_scale$expand
        }
    }
    ans
}

#' @return A list of scales for each panel
#' @noRd
set_default_scales <- function(scale_name, panels, index, labels, nudge,
                               expand = ggplot2::expansion()) {
    if (is.numeric(nudge)) {
        nudge <- nudge[index]
    } else if (is.waiver(nudge)) {
        nudge <- rep_len(0, length(index))
    }
    panels <- panels[index]
    # For y-axis, ggplot arrange panels from top to bottom,
    # we always choose to reverse the panel order
    if (scale_name == "y") panels <- fct_rev(panels)
    labels <- labels[index]
    fn <- switch(scale_name,
        x = ggplot2::scale_x_continuous,
        y = ggplot2::scale_y_continuous
    )
    data <- split(seq_along(index), panels)
    if (!is.list(expand)) expand <- rep_len(list(expand), length(data))
    .mapply(function(x, expand) {
        if (is.null(nudge)) {
            breaks <- NULL
            labels <- NULL # if breaks is `NULL`, `labels` cannot be set
        } else {
            breaks <- x + nudge[x]
            labels <- labels[x]
        }
        fn(
            limits = range(x) + c(-0.5, 0.5),
            breaks = breaks,
            labels = labels,
            expand = expand
        )
    }, list(x = data, expand = expand), NULL)
}

#' @importFrom rlang is_empty
extract_scales <- function(plot, axis, n_panels, facet_scales) {
    # if no facets, or if no facet scales
    if (n_panels > 1L && !is.null(facet_scales) &&
        !is_empty(ans <- .subset2(facet_scales, axis))) {
        # we don't allow the usage of formula in ggalign package
        if (rlang::is_formula(.subset2(ans, 1L))) {
            cli::cli_warn(paste(
                "{.fn facetted_pos_scales} formula is not supported in",
                "{.pkg ggalign}"
            ))
            ans <- rep_len(list(plot$scales$get_scales(axis)), n_panels)
        }
    } else {
        ans <- rep_len(list(plot$scales$get_scales(axis)), n_panels)
    }
    ans
}

melt_facet <- function(user_facet, default_facet) {
    if (is.null(default_facet)) { # no panels
        # we only support `FacetNull` if there have no panels
        if (inherits(user_facet, "FacetNull")) return(user_facet) # styler: off
        return(ggplot2::facet_null())
    }
    # we only support `FacetGrid` if there have multiple panels
    if (!inherits(user_facet, "FacetGrid")) return(default_facet) # styler: off

    # will change the user input, so we must use `ggproto_clone`
    user_facet <- ggproto_clone(user_facet)
    # we always fix the grid rows and cols
    user_facet$params$rows <- default_facet$params$rows
    user_facet$params$cols <- default_facet$params$cols
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
