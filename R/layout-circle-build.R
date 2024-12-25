#' @export
ggalign_build.CircleLayout <- function(x) {
    x <- default_layout(x)
    circle_build(x)
}

#' @importFrom ggplot2 find_panel calc_element ggproto ggplotGrob theme
#' @importFrom gtable gtable_add_grob gtable_add_padding
#' @importFrom grid unit viewport editGrob
#' @importFrom rlang is_empty arg_match0
circle_build <- function(circle, schemes = NULL, theme = NULL) {
    schemes <- inherit_parent_layout_schemes(circle, schemes)
    theme <- inherit_parent_layout_theme(circle, theme)
    # for empty plot
    base <- ggplot() +
        theme +
        ggplot2::labs(
            title = .subset2(circle@titles, "title"),
            subtitle = .subset2(circle@titles, "subtitle"),
            caption = .subset2(circle@titles, "caption")
        )
    if (is_empty(plot_list <- circle@plot_list)) {
        return(ggplotGrob(base))
    }

    # we remove the plot without actual plot area
    keep <- vapply(plot_list, function(plot) {
        !is.null(plot@plot)
    }, logical(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, keep)
    if (is_empty(plot_list)) return(ggplotGrob(base)) # styler: off

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
    guides <- vector("list", length(plot_list))
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

        # set limits and default scales
        if (!align$free_limits) {
            plot <- plot + ggalign_design(
                x = design,
                xlabels = .subset(align$labels, .subset2(design, "index"))
            )
        }

        # let `align` add other components
        plot <- align$build_plot(plot, design = design)
        plot <- align$finish_plot(plot, schemes = plot_schemes, theme = theme)
        plot <- plot + ggplot2::labs(x = NULL, y = NULL) +
            theme(panel.border = element_blank())

        # copied from `ggplot2:::ggplot_gtable`
        data <- ggplot2::ggplot_build(plot)
        plot <- data$plot
        plot_layout <- data$layout
        data <- data$data
        plot_theme <- complete_theme(plot$theme)

        geom_grobs <- ggfun("by_layer")(
            function(l, d) l$draw_geom(d, plot_layout),
            plot$layers, data,
            "converting geom to grob"
        )
        gt <- plot_layout$render(geom_grobs, data, plot_theme, plot$labels)

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

            # the spacer between two plots
            spacing <- calc_element("panel.spacing.r", plot_theme)
            if (inherits(spacing, "element_blank") || is.null(spacing)) {
                spacing <- unit(0, "mm")
            }

            plot_table <- editGrob(plot_table, vp = viewport(
                width = unit(rescale_factor, "npc") - spacing,
                height = unit(rescale_factor, "npc") - spacing,
                x = origin[1L], y = origin[2L], just = just,
                default.units = "native", clip = "off"
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

        # build legends
        default_position <- plot_theme$legend.position %||% "right"
        if (length(default_position) == 2) {
            default_position <- "inside"
        }
        if (default_position == "none") {
        } else {
            plot_theme$legend.key.width <- calc_element(
                "legend.key.width",
                plot_theme
            )
            plot_theme$legend.key.height <- calc_element(
                "legend.key.height",
                plot_theme
            )
            guides[[i]] <- plot$guides$draw(
                plot_theme,
                default_position,
                plot_theme$legend.direction
            )
        }
        origin <- just
        last_plot_size <- plot_size # the last plot panel size
    }
    # attach the guide legends
    guides <- lapply(c(.TLBR, "inside"), function(guide_pos) {
        unlist(lapply(guides, .subset2, guide_pos), FALSE, FALSE)
    })
    names(guides) <- c(.TLBR, "inside")

    theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
    theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
    theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)
    theme$legend.box.spacing <- calc_element(
        "legend.box.spacing", theme
    ) %||% unit(0.2, "cm")
    legend_box <- .mapply(
        function(guides, guide_pos) {
            # remove duplicated guides
            guides <- collapse_guides(guides)
            if (is_empty(guides)) {
                return(zeroGrob())
            }
            assemble_guides(guides, guide_pos, theme)
        },
        list(guides = guides, guide_pos = names(guides)),
        NULL
    )
    names(legend_box) <- names(guides)
    plot_table <- ggfun("table_add_legends")(plot_table, legend_box, theme)

    # Title
    title <- element_render(
        theme, "plot.title", .subset2(circle@titles, "title"),
        margin_y = TRUE, margin_x = TRUE
    )
    title_height <- grobHeight(title)

    # Subtitle
    subtitle <- element_render(
        theme, "plot.subtitle", .subset2(circle@titles, "subtitle"),
        margin_y = TRUE, margin_x = TRUE
    )
    subtitle_height <- grobHeight(subtitle)

    # whole plot annotation
    caption <- element_render(
        theme, "plot.caption", .subset2(circle@titles, "caption"),
        margin_y = TRUE, margin_x = TRUE
    )
    caption_height <- grobHeight(caption)

    # positioning of title and subtitle is governed by plot.title.position
    # positioning of caption is governed by plot.caption.position
    #   "panel" means align to the panel(s)
    #   "plot" means align to the entire plot (except margins and tag)
    title_pos <- arg_match0(
        theme$plot.title.position %||% "panel",
        c("panel", "plot"),
        arg_nm = "plot.title.position",
        error_call = quote(theme())
    )

    caption_pos <- arg_match0(
        theme$plot.caption.position %||% "panel",
        values = c("panel", "plot"),
        arg_nm = "plot.caption.position",
        error_call = quote(theme())
    )

    pans <- plot_table$layout[
        grepl("^panel", plot_table$layout$name), ,
        drop = FALSE
    ]
    if (title_pos == "panel") {
        title_l <- min(pans$l)
        title_r <- max(pans$r)
    } else {
        title_l <- 1
        title_r <- ncol(plot_table)
    }
    if (caption_pos == "panel") {
        caption_l <- min(pans$l)
        caption_r <- max(pans$r)
    } else {
        caption_l <- 1
        caption_r <- ncol(plot_table)
    }

    plot_table <- gtable_add_rows(plot_table, subtitle_height, pos = 0)
    plot_table <- gtable_add_grob(plot_table, subtitle,
        name = "subtitle",
        t = 1, b = 1, l = title_l, r = title_r, clip = "off"
    )

    plot_table <- gtable_add_rows(plot_table, title_height, pos = 0)
    plot_table <- gtable_add_grob(plot_table, title,
        name = "title",
        t = 1, b = 1, l = title_l, r = title_r, clip = "off"
    )

    plot_table <- gtable_add_rows(plot_table, caption_height, pos = -1)
    plot_table <- gtable_add_grob(plot_table, caption,
        name = "caption",
        t = -1, b = -1, l = caption_l, r = caption_r, clip = "off"
    )
    plot_table <- ggfun("table_add_tag")(plot_table, NULL, theme)

    # Margins
    plot_margin <- calc_element("plot.margin", theme) %||% margin()
    plot_table <- gtable_add_padding(plot_table, plot_margin)

    if (inherits(theme$plot.background, "element")) {
        plot_table <- gtable_add_grob(plot_table,
            element_render(theme, "plot.background"),
            t = 1, l = 1, b = -1, r = -1, name = "background", z = -Inf
        )
        plot_table$layout <- plot_table$layout[
            c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1)),
        ]
        plot_table$grobs <- plot_table$grobs[
            c(nrow(plot_table$layout), 1:(nrow(plot_table$layout) - 1))
        ]
    }

    # add alt-text as attribute
    # attr(plot_table, "alt-label") <- plot$labels$alt
    strip_pos <- find_strip_pos(plot_table)

    # always add strips columns and/or rows
    plot_table <- add_strips(plot_table, strip_pos)
    # add guides columns and/or rows for ggplot2 < 3.5.0
    plot_table <- add_guides(plot_table)
    setup_patch_titles(plot_table, patch_titles = list(
        top = NULL, left = NULL, bottom = NULL, right = NULL
    ), theme = theme)
}
