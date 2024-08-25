#' @importFrom grid unit.c
#' @export
build_alignpatches.HeatmapLayout <- function(layout) {
    patches <- heatmap_build(layout)
    plots <- .subset2(patches, "plots")
    sizes <- .subset2(patches, "sizes")
    design <- list(
        top = area(1, 2),
        left = area(2, 1),
        bottom = area(3, 2),
        right = area(2, 3),
        heatmap = area(2, 2)
    )
    sizes <- imap(list(
        height = c("top", "heatmap", "bottom"),
        width = c("left", "heatmap", "right")
    ), function(x, name) {
        out <- .subset(sizes, x)
        out$heatmap <- .subset2(.subset2(out, "heatmap"), name)
        out <- .subset(out, lengths(.subset(plots, x)) > 0L)
        do.call(unit.c, out)
    })
    keep <- lengths(plots) > 0L

    design <- trim_area(do.call(c, design[keep]))
    params <- slot(layout, "params")
    plot_grid(
        !!!plots[keep],
        design = design,
        heights = .subset2(sizes, "height"),
        widths = .subset2(sizes, "width"),
        # No parent layout, by default we'll always collect guides
        guides = .subset2(params, "guides") %|w|% TRUE
    )
}

#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom grid unit is.unit unit.c
heatmap_build <- function(heatmap, plot_data = waiver(), guides = waiver(),
                          free_labs = waiver(), free_sizes = waiver()) {
    params <- slot(heatmap, "params")
    mat <- slot(heatmap, "data")
    x_nobs <- get_nobs(heatmap, "x")
    y_nobs <- get_nobs(heatmap, "y")
    if (is.null(x_nobs) || is.null(y_nobs)) {
        cli::cli_abort(c(
            "You must provide {.arg data} argument to plot the heatmap",
            i = "Do you want to put this heatmap in a parent stack layout?"
        ))
    }
    xpanel <- get_panel(heatmap, "x") %||% factor(rep_len(1L, x_nobs))
    xindex <- get_index(heatmap, "x") %||% reorder_index(xpanel)

    ypanel <- get_panel(heatmap, "y") %||% factor(rep_len(1L, y_nobs))
    yindex <- get_index(heatmap, "y") %||% reorder_index(ypanel)

    # prepare free_labs and free_sizes
    heatmap_labs <- .subset2(params, "free_labs") %|w|% free_labs

    if (!is.waive(heatmap_labs)) {
        # prepare labs for child stack layout
        horizontal_labs <- intersect(heatmap_labs, c("t", "b"))
        vertical_labs <- intersect(heatmap_labs, c("l", "r"))
    } else {
        # inherit from the parent stack layout, by default we collapse labs
        heatmap_labs <- BORDERS
        horizontal_labs <- waiver()
        vertical_labs <- waiver()
    }

    # inherit from the parent stack layout
    heatmap_sizes <- .subset2(params, "free_sizes") %|w|% free_sizes
    if (!is.waive(heatmap_sizes)) {
        horizontal_sizes <- get_free_sizes(heatmap_sizes, c("t", "b"))
        vertical_sizes <- get_free_sizes(heatmap_sizes, c("l", "r"))
        if (length(horizontal_sizes) == 0L) horizontal_sizes <- NULL
        if (length(vertical_sizes) == 0L) vertical_sizes <- NULL
    } else {
        heatmap_sizes <- NULL
        # set child stack layout
        horizontal_sizes <- waiver()
        vertical_sizes <- waiver()
    }

    # read the plot ---------------------------------------
    p <- slot(heatmap, "plot")

    # always remove default axis titles -------------------
    # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
    # There are multiple ways to set labels in a plot, which take different
    # priorities. Here are the priorities from highest to lowest.
    # 1. The guide title.
    # 2. The scale name.
    # 3. The `labs()` function.
    # 4. The captured expression in aes().
    if (identical(.subset2(.subset2(p, "labels"), "x"), ".x")) {
        p$labels$x <- NULL
    }
    if (identical(.subset2(.subset2(p, "labels"), "y"), ".y")) {
        p$labels$y <- NULL
    }

    # set the default data -------------------------------
    data <- heatmap_build_data(mat, ypanel, yindex, xpanel, xindex)
    plot_data <- .subset2(params, "plot_data") %|w|% plot_data
    guides <- .subset2(params, "guides") %|w|% guides
    p <- finish_plot_data(p, plot_data %|w|% NULL, data = data)

    # setup the scales -----------------------------------
    do_row_facet <- nlevels(ypanel) > 1L
    do_column_facet <- nlevels(xpanel) > 1L

    facet_scales <- slot(heatmap, "facetted_pos_scales")
    xscales <- set_scales(
        plot = p,
        scale_name = "x",
        panel = xpanel,
        index = xindex,
        layout_labels = colnames(mat),
        facet_scales = facet_scales
    )
    # this will modify `p` in place
    p <- remove_scales(p, .subset2(xscales, 1L)$aesthetics)

    yscales <- set_scales(
        plot = p,
        scale_name = "y",
        panel = ypanel,
        index = yindex,
        layout_labels = rownames(mat),
        facet_scales = facet_scales
    )
    # this will modify `p` in place
    p <- remove_scales(p, .subset2(yscales, 1L)$aesthetics)

    # then we add facet -----------------------------------
    if (do_row_facet && do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.ypanel)),
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free", space = "free",
            drop = FALSE
        )
        p <- p + melt_facet(p$facet, default_facet) +
            ggh4x::facetted_pos_scales(x = xscales, y = yscales)
    } else if (do_row_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.ypanel)),
            scales = "free_y", space = "free_y",
            drop = FALSE
        )
        p <- p + melt_facet(p$facet, default_facet) +
            xscales +
            ggh4x::facetted_pos_scales(x = NULL, y = yscales)
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free_x", space = "free_x",
            drop = FALSE
        )
        p <- p + melt_facet(p$facet, default_facet) +
            yscales +
            ggh4x::facetted_pos_scales(x = xscales, y = NULL)
    } else {
        p <- p + xscales + yscales + melt_facet(p$facet, NULL)
    }

    # plot heatmap annotations ----------------------------
    stack_list <- lapply(HEATMAP_ANNOTATION_POSITION, function(position) {
        if (is_empty(stack <- slot(heatmap, position))) {
            return(list(plot = NULL, size = NULL))
        }
        if (is_horizontal(to_direction(position))) {
            panel <- xpanel
            index <- xindex
            free_sizes <- horizontal_sizes
            free_labs <- horizontal_labs
        } else {
            panel <- ypanel
            index <- yindex
            free_sizes <- vertical_sizes
            free_labs <- vertical_labs
        }
        stack_build(
            stack, plot_data, guides,
            free_labs = free_labs,
            free_sizes = free_sizes,
            extra_panel = panel,
            extra_index = index
        )
    })
    names(stack_list) <- HEATMAP_ANNOTATION_POSITION
    stack_list <- transpose(stack_list)
    plots <- .subset2(stack_list, 1L) # the annotation plot itself
    sizes <- .subset2(stack_list, 2L) # annotation size

    # By default, we won't remove sizes of the heatmap
    if (!is.null(heatmap_labs)) {
        p <- free_lab(p, heatmap_labs)
    }
    if (!is.null(heatmap_sizes)) {
        free_borders <- names(GGELEMENTS)[
            lengths(lapply(GGELEMENTS, intersect, heatmap_sizes)) > 0L
        ]
        if (length(free_borders)) {
            # here, we attach the borders into the panel
            p <- free_border(p, borders = free_borders)
        }
        p <- free_size(p, heatmap_sizes)
    }
    # by default, we always collapse the axis title
    plots <- c(plots, list(heatmap = p))
    sizes <- c(sizes, list(heatmap = .subset(params, c("width", "height"))))
    list(plots = plots, sizes = sizes)
}

plot_filler <- function() {
    p <- ggplot2::ggplot()
    add_class(p, "plot_filler")
}

heatmap_build_data <- function(matrix, row_panel, row_index,
                               column_panel, column_index) {
    ycoords <- data_frame0(
        .ypanel = row_panel[row_index],
        .yindex = row_index,
        .y = seq_along(row_index)
    )
    xcoords <- data_frame0(
        .xpanel = column_panel[column_index],
        .xindex = column_index,
        .x = seq_along(column_index)
    )
    coords <- merge(xcoords, ycoords, by = NULL, sort = FALSE, all = TRUE)
    ans <- melt_matrix(matrix)
    merge(ans, coords,
        by.x = c(".column_index", ".row_index"),
        by.y = c(".xindex", ".yindex"),
        sort = FALSE, all = TRUE
    )
}
