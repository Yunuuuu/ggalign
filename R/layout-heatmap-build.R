#' @importFrom grid unit.c
#' @export
ggalign_build.HeatmapLayout <- function(x) {
    patches <- heatmap_build(x)
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
    params <- x@params
    annotation <- x@annotation
    align_plots(
        !!!.subset(plots, keep),
        design = design,
        heights = .subset2(sizes, "height"),
        widths = .subset2(sizes, "width"),
        # No parent layout, by default we'll always collect guides
        guides = .subset2(params, "guides") %|w|% "tlbr",
        title = .subset2(annotation, "title"),
        subtitle = .subset2(annotation, "subtitle"),
        caption = .subset2(annotation, "caption"),
        theme = x@theme %||% default_theme()
    )
}

#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom grid unit is.unit unit.c
heatmap_build <- function(heatmap, plot_data = waiver(), guides = waiver(),
                          free_labs = waiver(), free_spaces = waiver(),
                          theme = waiver()) {
    params <- heatmap@params
    mat <- heatmap@data
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

    # prepare free_labs and free_spaces
    heatmap_labs <- .subset2(params, "free_labs") %|w|% free_labs

    if (is.null(heatmap_labs)) {
        horizontal_labs <- vertical_labs <- NULL
    } else if (!is.waive(heatmap_labs)) {
        # prepare labs for child stack layout
        horizontal_labs <- gsub("[lr]", "", heatmap_labs)
        vertical_labs <- gsub("[tb]", "", heatmap_labs)
        if (nchar(horizontal_labs) == 0L) horizontal_labs <- NULL
        if (nchar(vertical_labs) == 0L) vertical_labs <- NULL
    } else {
        # by default, we always collapse the axis title
        heatmap_labs <- "tlbr"
        horizontal_labs <- waiver()
        vertical_labs <- waiver()
    }

    # inherit from the parent stack layout
    heatmap_spaces <- .subset2(params, "free_spaces") %|w|% free_spaces
    if (is.null(heatmap_spaces)) {
        horizontal_spaces <- vertical_spaces <- NULL
    } else if (is.waive(heatmap_spaces)) {
        # By default, we won't remove border sizes of the heatmap
        heatmap_spaces <- NULL
        # set child stack layout
        horizontal_spaces <- waiver()
        vertical_spaces <- waiver()
    } else {
        horizontal_spaces <- gsub("[lr]", "", heatmap_spaces)
        vertical_spaces <- gsub("[tb]", "", heatmap_spaces)
        if (nchar(horizontal_spaces) == 0L) horizontal_spaces <- NULL
        if (nchar(vertical_spaces) == 0L) vertical_spaces <- NULL
    }

    # inherit from the parent stack layout
    theme <- inherit_theme(.subset2(params, "theme"), theme)

    # read the plot ---------------------------------------
    p <- heatmap@plot

    # set the default data -------------------------------
    data <- heatmap_build_data(mat, ypanel, yindex, xpanel, xindex)
    plot_data <- .subset2(params, "plot_data") %|w|% plot_data
    guides <- .subset2(params, "guides") %|w|% guides
    p <- finish_plot_data(p, plot_data %|w|% NULL, data = data)

    # setup the scales -----------------------------------
    do_row_facet <- nlevels(ypanel) > 1L
    do_column_facet <- nlevels(xpanel) > 1L

    facet_scales <- heatmap@facetted_pos_scales
    xscales <- set_scales(
        plot = p,
        scale_name = "x",
        panel = xpanel,
        index = xindex,
        layout_labels = colnames(mat),
        facet_scales = facet_scales
    )
    p <- remove_scales(p, .subset2(xscales, 1L)$aesthetics)

    yscales <- set_scales(
        plot = p,
        scale_name = "y",
        panel = ypanel,
        index = yindex,
        layout_labels = rownames(mat),
        facet_scales = facet_scales
    )
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
            scales = "free_y", space = "free",
            drop = FALSE
        )
        p <- p + melt_facet(p$facet, default_facet) +
            xscales +
            ggh4x::facetted_pos_scales(x = NULL, y = yscales)
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free_x", space = "free",
            drop = FALSE
        )
        p <- p + melt_facet(p$facet, default_facet) +
            yscales +
            ggh4x::facetted_pos_scales(x = xscales, y = NULL)
    } else {
        p <- p + xscales + yscales + melt_facet(p$facet, NULL)
    }

    # plot heatmap annotations ----------------------------
    stack_list <- lapply(.TLBR, function(position) {
        if (is_empty(stack <- slot(heatmap, position))) {
            return(list(plot = NULL, size = NULL))
        }
        if (is_horizontal(to_direction(position))) {
            panel <- xpanel
            index <- xindex
            free_spaces <- horizontal_spaces
            free_labs <- horizontal_labs
        } else {
            panel <- ypanel
            index <- yindex
            free_spaces <- vertical_spaces
            free_labs <- vertical_labs
        }
        ans <- stack_build(
            stack,
            plot_data = plot_data,
            guides = guides,
            free_labs = free_labs,
            free_spaces = free_spaces,
            theme = theme,
            extra_panel = panel,
            extra_index = index
        )
        # for heatmap annotation, we should always make them next to
        # the heatmap body
        if (!is.null(.subset2(ans, "plot"))) {
            ans$plot <- free_vp(
                .subset2(ans, "plot"),
                x = switch(position,
                    left = 1L,
                    right = 0L,
                    0.5
                ),
                y = switch(position,
                    top = 0L,
                    bottom = 1L,
                    0.5
                ),
                just = switch(position,
                    top = "bottom",
                    left = "right",
                    bottom = "top",
                    right = "left"
                )
            )
        }
        ans
    })
    names(stack_list) <- .TLBR
    stack_list <- transpose(stack_list)
    plots <- .subset2(stack_list, 1L) # the annotation plot itself
    sizes <- .subset2(stack_list, 2L) # annotation size
    p$theme <- (theme %||% default_theme()) + p$theme
    if (!is.null(heatmap_labs)) {
        p <- free_lab(p, heatmap_labs)
    }
    if (!is.null(heatmap_spaces)) {
        p <- free_space(free_border(p, heatmap_spaces), heatmap_spaces)
    }
    plots <- c(plots, list(heatmap = p))
    sizes <- c(sizes, list(heatmap = .subset(params, c("width", "height"))))
    list(plots = plots, sizes = sizes)
}

plot_filler <- function() {
    p <- ggplot2::ggplot()
    add_class(p, "plot_filler")
}

#' @importFrom stats reorder
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
    ans <- merge(ans, coords,
        by.x = c(".column_index", ".row_index"),
        by.y = c(".xindex", ".yindex"),
        sort = FALSE, all = TRUE
    )
    if (!is.null(.subset2(ans, ".row_names"))) {
        ans$.row_names <- reorder(
            .subset2(ans, ".row_names"),
            .subset2(ans, ".y"),
            order = FALSE
        )
    }
    if (!is.null(.subset2(ans, ".column_names"))) {
        ans$.column_names <- reorder(
            .subset2(ans, ".column_names"),
            .subset2(ans, ".x"),
            order = FALSE
        )
    }
    ans
}
