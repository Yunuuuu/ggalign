S7::method(ggalign_build, CircleLayout) <- function(x) {
    ggalign_init(x)
}

#' @importFrom ggplot2 find_panel ggproto ggplot_build
#' @importFrom ggplot2 theme complete_theme is_theme_element calc_element
#' @importFrom gtable gtable_add_grob gtable_add_padding is.gtable
#' @importFrom grid unit viewport editGrob
#' @importFrom rlang is_empty arg_match0
#' @importFrom S7 prop props
S7::method(ggalign_gtable, CircleLayout) <- function(x) {
    schemes <- prop(x, "schemes")
    theme <- prop(x, "theme")
    titles <- ggalign_init(prop(x, "titles"))
    # for empty plot
    base <- ggplot() +
        theme +
        ggplot2::labs(!!!props(titles))
    if (is_empty(plot_list <- x@box_list)) {
        return(ggalign_gtable(base))
    }

    # we remove the plot without actual plot area
    keep <- vapply(plot_list, function(plot) {
        !is.null(plot@plot)
    }, logical(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, keep)
    if (is_empty(plot_list)) return(ggalign_gtable(base)) # styler: off

    # we reorder the plots based on the `order` slot
    plot_order <- vapply(plot_list, function(plot) {
        prop(plot@active, "order")
    }, integer(1L), USE.NAMES = FALSE)
    plot_list <- .subset(plot_list, make_order(plot_order))

    # plot coordinate
    if (is.null(input_radial <- x@radial)) {
        radial <- coord_circle(theta = "x", r.axis.inside = TRUE)
    } else {
        radial <- ggproto(NULL, input_radial, theta = "x", r_axis_inside = TRUE)
    }

    sizes <- vapply(plot_list, function(plot) {
        # for circular layout, we only support relative size
        if (is.na(size <- as.numeric(prop(plot, "size")))) {
            size <- 1
        }
        size
    }, numeric(1L), USE.NAMES = FALSE)

    # For each plot track, relative to the total radius (1):
    # 1. total radius: 1
    # 2. total radius for the plot area (for each plot track): 1 - inner_radius
    if (inherits(radial, "CoordCircle")) {
        inner_radius <- radial$inner_radius[1L] / 0.5
        outer_radius <- radial$inner_radius[2L] / 0.5
    } else {
        # For `CoordRadial`
        # `0.4` is coord_radial used for scale size in ggplot2 to add extra
        # spaces for axis labels
        # https://github.com/tidyverse/ggplot2/issues/6284
        inner_radius <- radial$inner_radius[1L] / 0.4
        outer_radius <- radial$inner_radius[2L] / 0.4
        ParentRadial <- radial
        radial <- ggproto(NULL, ParentRadial,
            setup_panel_params = circle_panel_params
        )
    }
    plot_track <- sizes / sum(sizes) * (outer_radius - inner_radius)

    # For each plot, the plot size is calculated by adding the space for the
    # inner radius of each track.
    N <- length(plot_list)
    index <- seq_len(N)
    direction <- x@direction
    if (identical(direction, "outward")) {
        plot_sizes <- inner_radius + cumsum(plot_track)
    } else {
        plot_sizes <- outer_radius -
            cumsum(c(0, utils::head(plot_track, -1L)))
        # The plots are always build outward, so the order is reversed.
        index <- rev(index)
    }

    # For each plot, the inner radius is calculated as the difference between
    # the plot size and its track size.
    plot_inner <- plot_sizes - plot_track
    guides <- vector("list", N)
    plot_table <- NULL
    domain <- domain_init(x@domain)
    for (i in index) {
        plot_size <- plot_sizes[[i]]
        plot <- .subset2(plot_list, i)
        craftsman <- prop(plot, "craftsman") # `Craftsman` object
        plot_schemes <- ggalign_inherit(plot@schemes, schemes)
        # the actual plot
        plot <- craftsman$build_plot(plot@plot, domain = domain)

        plot <- craftsman$setup_circle_facet(plot, domain,
            sector_spacing = x@sector_spacing %||% (pi / 180)
        )
        plot <- craftsman$setup_circle_coord(plot, radial,
            # https://github.com/tidyverse/ggplot2/issues/6284
            # Use `0.5` to remove the extra spaces for axis label
            inner_radius = c(
                plot_inner[[i]] / plot_size,
                # for the outmost plot, we respect the outer radius defined by
                # the users, for others, we alway use 1 to remove any spacing
                # between two tracks
                if (i == N) outer_radius else 1
            ) * 0.5
        )
        plot <- craftsman$setup_circle_plot(plot, domain)

        # let `Craftsman` add other components
        plot <- craftsman$finish_plot(
            plot,
            schemes = plot_schemes, theme = theme
        )
        plot <- plot + ggplot2::labs(x = NULL, y = NULL) +
            theme(panel.border = element_blank())

        # copied from `ggplot2:::ggplot_gtable`
        data <- ggplot_build(plot)
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
        # For bbox, `ggplot2::polar_bbox` always take (0.5, 0.5) as origin
        bbox <- ggfun("polar_bbox")(
            plot_layout$coord$arc, margin = c(0, 0, 0, 0),
            inner_radius = plot_layout$coord$inner_radius
        )
        origin <- c(
            scales::rescale(0.5, from = bbox$x),
            scales::rescale(0.5, from = bbox$y)
        )
        spacing <- calc_element("panel.spacing.r", plot_theme)

        if (is.null(plot_table)) {
            plot_table <- gt
        } else {
            # define the panel size of the inner track
            rescale_factor <- last_plot_size / plot_size

            # the spacer between two plots
            if (identical(direction, "outward")) {
                spacer <- last_spacing
            } else {
                spacer <- spacing
            }
            if (is_theme_element(spacer, "blank") || is.null(spacer)) {
                spacer <- unit(0, "mm")
            }
            plot_table <- editGrob(plot_table, vp = viewport(
                width = unit(rescale_factor, "npc") - spacer,
                height = unit(rescale_factor, "npc") - spacer,
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
        guides[i] <- list(plot$guides$assemble(plot_theme))

        # assign value for next loop
        just <- origin
        last_plot_size <- plot_size # the last plot panel size
        last_spacing <- spacing
    }

    # attach the guide legends
    guides <- gather_guides(guides, zeroGrob())
    theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
    theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
    theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)
    theme$legend.box.spacing <- calc_element(
        "legend.box.spacing", theme
    ) %||% unit(0.2, "cm")
    legend_box <- .mapply(
        function(guides, guide_pos) assemble_guides(guides, guide_pos, theme),
        list(guides = guides, guide_pos = names(guides)),
        NULL
    )
    names(legend_box) <- names(guides)
    plot_table <- ggfun("table_add_legends")(plot_table, legend_box, theme)

    # Title
    title <- element_render(
        theme, "plot.title", prop(titles, "title"),
        margin_y = TRUE, margin_x = TRUE
    )
    title_height <- grobHeight(title)

    # Subtitle
    subtitle <- element_render(
        theme, "plot.subtitle", prop(titles, "subtitle"),
        margin_y = TRUE, margin_x = TRUE
    )
    subtitle_height <- grobHeight(subtitle)

    # whole plot annotation
    caption <- element_render(
        theme, "plot.caption", prop(titles, "caption"),
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
        error_call = quote(layout_theme())
    )

    caption_pos <- arg_match0(
        theme$plot.caption.position %||% "panel",
        values = c("panel", "plot"),
        arg_nm = "plot.caption.position",
        error_call = quote(layout_theme())
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

    if (is_theme_element(theme$plot.background)) {
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
    strip_pos <- find_strip_pos(plot_table, theme)

    # always add strips columns and/or rows
    plot_table <- add_strips(plot_table, strip_pos)
    setup_patch_title(plot_table, NULL, theme = theme)
}

#' @importFrom S7 prop
#' @importFrom ggplot2 ggproto
S7::method(patch, CircleLayout) <- function(x) {
    build <- ggalign_build(x)
    ggproto(NULL, Patch,
        setup = function(self, options = NULL) {
            # Preserve tag-related theme settings from the original layout
            # theme. These are intentionally not overridden so that
            # `PatchAlignpatches` retains full control over tag appearance and
            # positioning.
            if (!is.null(theme <- prop(options, "theme"))) {
                prop(options, "theme", check = FALSE) <- prop(build, "theme") +
                    (tag_theme(theme) + tag_theme(prop(x, "theme")))
            }
            ggproto_parent(Patch, self)$setup(options)
        },
        gtable = function(self, options) ggalign_gtable(build)
    )
}
