#' @export
ggalign_build.CircleLayout <- function(x) {
    x <- default_layout(x)
    if (is.null(out <- circle_build(x))) {
        out <- ggplot() +
            x@theme +
            ggplot2::labs(
                title = .subset2(x@titles, "title"),
                subtitle = .subset2(x@titles, "subtitle"),
                caption = .subset2(x@titles, "caption")
            )
    }
    out
}

#' @importFrom ggplot2 find_panel calc_element ggproto
#' @importFrom gtable gtable_add_grob
#' @importFrom grid unit viewport editGrob
#' @importFrom rlang is_empty
circle_build <- function(circle, schemes = NULL, theme = NULL) {
    if (is_empty(plot_list <- circle@plot_list)) {
        return(NULL)
    }
    schemes <- inherit_parent_layout_schemes(circle, schemes)
    theme <- inherit_parent_layout_theme(circle, theme)

    # we remove the plot without actual plot area
    keep <- vapply(plot_list, function(plot) {
        !is.null(plot@plot)
    }, logical(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, keep)
    if (is_empty(plot_list)) return(NULL) # styler: off

    # we reorder the plots based on the `order` slot
    plot_order <- vapply(plot_list, function(plot) {
        .subset2(plot@active, "order")
    }, integer(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, make_order(plot_order))

    # plot coordinate
    if (is.null(input_radial <- circle@radial)) {
        radial <- ggplot2::coord_radial(theta = "x", r.axis.inside = TRUE)
    } else {
        radial <- ggproto(NULL, input_radial, theta = "x", r_axis_inside = TRUE)
    }

    # for every plot track, all relative to the total radius `1`
    sizes <- vapply(plot_list, function(plot) {
        # for circular layout, we only support relative size
        if (is.na(size <- as.numeric(plot@size))) {
            size <- 1
        }
        size
    }, numeric(1L), USE.NAMES = FALSE)
    plot_track <- sizes / sum(sizes) * (1 - radial$inner_radius[1L] / 0.4)
    plot_sizes <- 1 - cumsum(c(0, plot_track[-length(plot_track)]))
    plot_inner <- plot_sizes - plot_track
    plot_table <- origin <- NULL
    design <- setup_design(circle@design)
    for (i in rev(seq_along(plot_list))) { # from inner-most to the out-most
        plot_size <- plot_sizes[[i]]
        plot <- .subset2(plot_list, i)
        align <- plot@align # `AlignProto` object
        plot_schemes <- inherit_schemes(plot@schemes, schemes)
        # the actual plot
        plot <- plot@plot

        # we always use `null` facet
        # we won't respect `free_facet` and `free_coord`
        plot <- gguse_facet(plot, ggplot2::facet_null())
        plot <- gguse_radial_coord(
            plot,
            coord = radial,
            inner_radius = c(plot_inner[[i]] / plot_size, 1) * 0.4,
            layout_name = align$layout_name
        )
        plot <- align$build_plot(plot, design = design)
        plot <- align$finish_plot(plot, schemes = plot_schemes, theme = theme)
        plot <- plot + ggplot2::labs(x = NULL, y = NULL)

        # copied from `ggplot2:::ggplot_gtable`
        data <- ggplot2::ggplot_build(plot)
        plot <- data$plot
        plot_layout <- data$layout
        data <- data$data
        theme <- complete_theme(plot$theme)

        geom_grobs <- ggfun("by_layer")(
            function(l, d) l$draw_geom(d, plot_layout),
            plot$layers, data,
            "converting geom to grob"
        )
        gt <- plot_layout$render(geom_grobs, data, theme, plot$labels)

        # for each inner gtable, we insert it to the panel area of the
        # outter gtable
        #
        # how to get the coordinate origin from the `coord_radial()` ?
        # origin <- layout$coord$transform(
        #     data.frame(x = 0.5, y = 0.5),
        #     panel_params = layout$panel_params[[1L]]
        # )
        # For bbox, `ggplot2::polar_bbox` always take (0.5, 0.5) as origin
        bbox <- plot_layout$panel_params[[1L]]$bbox
        just <- c(
            scales::rescale(0.5, from = bbox$x),
            scales::rescale(0.5, from = bbox$y)
        )

        if (is.null(plot_table)) {
            plot_table <- gt
        } else {
            # define the panel size of the inner track
            rescale_factor <- last_plot_size / plot_size

            # just using panel spacing as the spacer between two plots
            spacing <- calc_element("panel.spacing.y", theme)

            plot_table <- editGrob(plot_table, vp = viewport(
                width = unit(rescale_factor, "npc") - spacing,
                height = unit(rescale_factor, "npc") - spacing,
                x = origin[1L], y = origin[2L],
                just = just,
                default.units = "native",
                clip = "off"
            ))

            # add the inner track to the panel area of the outter track
            panel_loc <- find_panel(gt)
            plot_table <- gtable_add_grob(
                gt, plot_table,
                t = .subset2(panel_loc, "t"),
                l = .subset2(panel_loc, "l"),
                b = .subset2(panel_loc, "b"),
                r = .subset2(panel_loc, "r"),
                name = "inner-track"
            )
        }
        origin <- just
        last_plot_size <- plot_size # the last plot panel size
    }
    plot_table
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
