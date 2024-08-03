#' @importFrom patchwork area
#' @importFrom grid unit.c
#' @export
build_patchwork.LayoutHeatmap <- function(layout) {
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
    patchwork::wrap_plots(
        plots[keep],
        design = design,
        heights = .subset2(sizes, "height"),
        widths = .subset2(sizes, "width"),
        guides = .subset2(params, "guides"),
        align_axis_title = .subset2(params, "align_axis_title")
    )
}


#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom patchwork area
#' @importFrom grid unit is.unit unit.c
heatmap_build <- function(heatmap) {
    mat <- slot(heatmap, "data")
    params <- slot(heatmap, "params")

    xpanels <- get_panels(heatmap, "x") %||% factor(rep_len(1L, ncol(mat)))
    xindex <- get_index(heatmap, "x") %||% reorder_index(xpanels)

    ypanels <- get_panels(heatmap, "y") %||% factor(rep_len(1L, nrow(mat)))
    yindex <- get_index(heatmap, "y") %||% reorder_index(ypanels)

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
    data <- heatmap_build_data(mat, ypanels, yindex, xpanels, xindex)
    p$data <- data # change the default data.frame

    # add scales ---------------------------------------
    do_row_facet <- nlevels(ypanels) > 1L
    do_column_facet <- nlevels(xpanels) > 1L

    # following scales are templates used by both heatmap and annotation
    # here is the default scales
    default_xscales <- set_default_scales(
        "x", xpanels, xindex,
        .subset2(params, "xlabels"),
        nudge = .subset2(params, "xlabels_nudge")
    )
    default_yscales <- set_default_scales(
        "y", ypanels, yindex,
        .subset2(params, "ylabels"),
        nudge = .subset2(params, "ylabels_nudge")
    )

    # we extract user provided scales
    facet_scales <- slot(heatmap, "facetted_pos_scales")
    user_xscales <- extract_scales(
        p, "x", nlevels(xpanels), facet_scales
    )
    user_yscales <- extract_scales(
        p, "y", nlevels(ypanels), facet_scales
    )

    # here we set default value for user scales
    for (i in seq_along(default_xscales)) {
        user_xscales[[i]] <- melt_scale(
            .subset2(user_xscales, i),
            .subset2(default_xscales, i)
        )
        # we copy the `expand` from user input into the default for usage of
        # annotation scales
        default_xscales[[i]]$expand <- .subset2(user_xscales, i)$expand
    }

    for (i in seq_along(default_yscales)) {
        user_yscales[[i]] <- melt_scale(
            .subset2(user_yscales, i),
            .subset2(default_yscales, i)
        )
        # we copy the expand from user input
        #   into the default for usage of annotation
        default_yscales[[i]]$expand <- .subset2(user_yscales, i)$expand
    }

    # then we add facet -----------------------------------
    if (do_row_facet && do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.row_panel)),
            cols = ggplot2::vars(.data$.column_panel),
            scales = "free", space = "free"
        )
        p <- p + melt_facet(p$facet, default_facet) +
            ggh4x::facetted_pos_scales(x = user_xscales, y = user_yscales)
    } else if (do_row_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.row_panel)),
            scales = "free_y", space = "free_y"
        )
        p <- p + melt_facet(p$facet, default_facet) +
            user_xscales +
            ggh4x::facetted_pos_scales(x = NULL, y = user_yscales)
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.column_panel),
            scales = "free_x", space = "free_x"
        )
        p <- p + melt_facet(p$facet, default_facet) +
            user_yscales +
            ggh4x::facetted_pos_scales(x = user_xscales, y = NULL)
    } else {
        p <- p + user_xscales + user_yscales + melt_facet(p$facet, NULL)
    }

    # plot heatmap annotations ----------------------------
    stack_list <- lapply(GGHEAT_ELEMENTS, function(position) {
        if (is_empty(stack <- slot(heatmap, position))) {
            return(list(plot = NULL, size = NULL))
        }
        # we only allow add `Align` object into the heatmap annotation
        # although the stack layout can add `layout_heatmap`
        stack_build(stack)
    })
    names(stack_list) <- GGHEAT_ELEMENTS
    stack_list <- transpose(stack_list)
    plots <- .subset2(stack_list, 1L) # the annotation plot itself
    sizes <- .subset2(stack_list, 2L) # annotation size

    plots <- c(plots, list(heatmap = p))
    sizes <- c(sizes, list(heatmap = .subset(params, c("width", "height"))))
    list(plots = plots, sizes = sizes)
}

plot_filler <- function() {
    p <- ggplot2::ggplot()
    class(p) <- c("plot_filler", class(p))
    p
}

heatmap_build_data <- function(matrix, row_panels, row_index,
                               column_panels, column_index) {
    row_coords <- data_frame0(
        .row_panel = row_panels[row_index],
        .row_index = row_index,
        .y = seq_along(row_index)
    )
    column_coords <- data_frame0(
        .column_panel = column_panels[column_index],
        .column_index = column_index,
        .x = seq_along(column_index)
    )
    coords <- merge(column_coords, row_coords,
        by = NULL, sort = FALSE, all = TRUE
    )
    ans <- melt_matrix(matrix)
    merge(ans, coords, by = intersect(names(ans), names(coords)), all = TRUE)
}
