#' @export
print.ggheatmap <- function(x, ...) {
    p <- ggheat_build(x)
    print(p)
}

ggheat_build <- function(x, ...) UseMethod("ggheat_build")

#' @export
ggheat_build.default <- function(x, ...) {
    cli::cli_abort("{.arg x} must be a {.cls ggheatmap} object")
}

#' @importFrom rlang is_empty
#' @importFrom patchwork area
#' @importFrom grid unit is.unit unit.c
#' @export
ggheat_build.ggheatmap <- function(x, ...) {
    mat <- slot(x, "matrix")
    row_index <- slot(x, "row_index") %||% seq_len(nrow(mat))
    row_panels <- slot(x, "row_panels") %||%
        factor(rep_len(1L, length(row_index)))
    column_index <- slot(x, "column_index") %||% seq_len(ncol(mat))
    column_panels <- slot(x, "column_panels") %||%
        factor(rep_len(1L, length(column_index)))

    # reset index based on the panels
    row_index <- ggheat_index(row_panels, row_index)
    column_index <- ggheat_index(column_panels, column_index)

    # read the plot ---------------------------------------
    p <- slot(x, "heatmap")

    # set the default data -------------------------------
    data <- ggheat_build_data(
        mat, row_panels, row_index,
        column_panels, column_index
    )
    p$data <- data # change the default data.frame

    # add heatmap filling in the first layer
    p$layers <- append(p$layers,
        ggplot2::geom_tile(
            ggplot2::aes(.data$.x, .data$.y, fill = .data$value),
            width = 1L, height = 1L
        ),
        after = 0L
    )

    # add scales ---------------------------------------
    do_row_facet <- nlevels(row_panels) > 1L
    do_column_facet <- nlevels(column_panels) > 1L

    # here is the default scales
    # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
    # There are multiple ways to set labels in a plot, which take different
    # priorities. Here are the priorities from highest to lowest.
    # 1. The guide title.
    # 2. The scale name.
    # 3. The `labs()` function.
    # 4. The captured expression in aes().
    # following scales are templates used by both heatmap and annotation
    default_xscales <- ggheat_default_scale(
        "x", column_panels, column_index, colnames(mat)
    )
    default_yscales <- ggheat_default_scale(
        "y", row_panels, row_index, rownames(mat)
    )

    # we extract user provided scales
    facet_scales <- slot(x, "facetted_pos_scales")
    user_xscales <- ggheat_extract_scales(
        "x", p, nlevels(column_panels), facet_scales
    )
    user_yscales <- ggheat_extract_scales(
        "y", p, nlevels(row_panels), facet_scales
    )

    # here we set default value for user scales
    for (i in seq_along(default_xscales)) {
        user_xscales[[i]] <- ggheat_melt_scale(
            .subset2(user_xscales, i),
            .subset2(default_xscales, i)
        )
        # we copy the expand from into the default for usage of annotation
        default_xscales[[i]]$expand <- .subset2(user_xscales, i)$expand
    }

    for (i in seq_along(default_yscales)) {
        user_yscales[[i]] <- ggheat_melt_scale(
            .subset2(user_yscales, i),
            .subset2(default_yscales, i)
        )
        default_yscales[[i]]$expand <- .subset2(user_yscales, i)$expand
    }

    # then we add facet -----------------------------------
    if (do_row_facet && do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.row_panels),
            cols = ggplot2::vars(.data$.column_panels),
            scales = "free", space = "free"
        )
        p <- p + ggheat_melt_facet(p$facet, default_facet) +
            ggh4x::facetted_pos_scales(x = user_xscales, y = user_yscales)
    } else if (do_row_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.row_panels),
            scales = "free_y", space = "free_y"
        )
        p <- p + ggheat_melt_facet(p$facet, default_facet) +
            user_xscales +
            ggh4x::facetted_pos_scales(x = NULL, y = user_yscales)
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.column_panels),
            scales = "free_x", space = "free_x"
        )
        p <- p + ggheat_melt_facet(p$facet, default_facet) +
            user_yscales +
            ggh4x::facetted_pos_scales(x = user_xscales, y = NULL)
    } else {
        p <- p + user_xscales + user_yscales +
            ggheat_melt_facet(p$facet, NULL)
    }

    # prepare annotations ---------------------------------
    annotations <- lapply(GGHEAT_ELEMENTS, function(position) {
        annotations <- slot(x, position)
        if (length(annotations) == 0L) return(list(NULL, NULL)) # styler: off
        # default facet for annotation
        anno_facet <- switch_position(
            position,
            if (do_row_facet) {
                ggplot2::facet_grid(
                    rows = ggplot2::vars(.data$.panels),
                    scales = "free_y"
                )
            },
            if (do_column_facet) {
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panels),
                    scales = "free_x"
                )
            }
        )
        plots <- lapply(
            annotations,
            anno_build,
            index = switch_position(position, row_index, column_index),
            panels = switch_position(position, row_panels, column_panels),
            position = position,
            scales = switch_position(
                position,
                default_yscales,
                default_xscales
            ),
            facet = anno_facet
        )

        # we reorder annotation based on the `order` slot
        index <- order(vapply(annotations, slot, integer(1L), name = "order"))
        sizes <- lapply(annotations, slot, name = "size")
        sizes <- do.call(unit.c, sizes[index])
        plots <- .subset(plots, index)

        # combine all annotations
        keep <- lengths(plots) > 0L
        plots <- .subset(plots, keep)
        if (length(plots) == 0L) return(list(NULL, NULL)) # styler: off
        sizes <- sizes[keep]
        list(
            switch_position(
                position,
                patchwork::wrap_plots(plots,
                    nrow = 1L, widths = sizes
                ),
                patchwork::wrap_plots(plots,
                    ncol = 1L, heights = sizes
                )
            ),
            sum(sizes)
        )
    })
    names(annotations) <- GGHEAT_ELEMENTS
    annotations <- transpose(annotations)
    annotation_sizes <- .subset2(annotations, 2L) # annotation size
    annotations <- .subset2(annotations, 1L) # the annotation plot itself

    heatmap <- p + ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = -60, hjust = 0L)
    )
    layout <- list(
        top = area(1, 2),
        left = area(2, 1),
        bottom = area(3, 2),
        right = area(2, 3),
        heatmap = area(2, 2)
    )
    ans <- c(annotations, list(heatmap = heatmap))
    sizes <- c(annotation_sizes, list(heatmap = unit(1, "null")))
    sizes <- lapply(list(
        heights = c("top", "heatmap", "bottom"),
        widths = c("left", "heatmap", "right")
    ), function(nms) {
        sizes <- .subset(.subset(sizes, nms), lengths(.subset(ans, nms)) > 0L)
        do.call(unit.c, sizes)
    })
    keep <- lengths(ans) > 0L

    layout <- trim_area(do.call(c, layout[keep]))
    patchwork::wrap_plots(ans[keep],
        design = layout,
        heights = .subset2(sizes, "heights"),
        widths = .subset2(sizes, "widths"),
        guides = "collect"
    )
}

trim_area <- function(area) {
    w <- min(.subset2(area, "l"), .subset2(area, "r"))
    h <- min(.subset2(area, "t"), .subset2(area, "b"))
    area$l <- .subset2(area, "l") - w + 1
    area$r <- .subset2(area, "r") - w + 1
    area$t <- .subset2(area, "t") - h + 1
    area$b <- .subset2(area, "b") - h + 1
    area
}

ggheat_extract_scales <- function(scale_name, plot, n, facet_scales) {
    single_scale <- plot$scales$get_scales(scale_name)
    if (!is.null(facet_scales)) {
        scales <- .subset2(facet_scales, scale_name)
        if (n > 1L) {
            if (is_empty(scales)) {
                scales <- vector("list", n)
            } else if (length(scales) < n) {
                cli::cli_warn(
                    "Not enough facetted {.field {scale_name}} scales"
                )
            } else {
                scales <- .subset(scales, n)
            }
        } else if (length(scales) > 1L) { # if not
            axis <- switch(scale_name,
                x = "column",
                y = "row"
            )
            cli::cli_warn(sprintf(
                "heatmap {%s} is not splitted, won't use facet {%s} scale",
                axis, scale_name
            ))
            scales <- list(NULL)
        }
        # filling scales with normal scale
        lapply(scales, function(scale) scale %||% single_scale)
    } else {
        rep_len(list(single_scale), n)
    }
}

ggheat_melt_facet <- function(user_facet, default_facet) {
    if (is.null(default_facet)) { # no panels
        # we only support `FacetNull` if there have no panels
        if (inherits(user_facet, "FacetNull")) return(user_facet) # styler: off
        return(ggplot2::facet_null())
    }

    # we only support `FacetGrid` if there have multiple panels
    if (!inherits(user_facet, "FacetGrid")) return(default_facet) # styler: off
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

ggheat_melt_scale <- function(user_scale, default_scale) {
    if (is.null(user_scale)) {
        default_scale$clone()
    } else {
        # always reset the limits, breaks, and labels
        #  it's not possible for user to know the limits and breaks
        #  since a heatmap often reorder columns or rows.
        user_scale$limits <- default_scale$limits
        user_scale$breaks <- default_scale$breaks
        user_scale$labels <- default_scale$labels
        if (is.waiver(user_scale$expand)) {
            user_scale$expand <- default_scale$expand
        }
        user_scale
    }
}

ggheat_default_scale <- function(scale_name, panels, index, labels) {
    panels <- panels[index]
    labels <- labels[index]
    fn <- switch(scale_name,
        x = ggplot2::scale_x_continuous,
        y = ggplot2::scale_y_continuous
    )
    lapply(split(seq_along(index), panels), function(x) { # heatmap coords
        fn(
            name = NULL, # we by default always remove the annotation name
            limits = range(x) + c(-0.5, 0.5),
            breaks = x,
            labels = labels[x],
            expand = ggplot2::expansion()
        )
    })
}

ggheat_build_data <- function(matrix, row_panels, row_index,
                              column_panels, column_index) {
    row_coords <- tibble0(
        .row_panels = row_panels[row_index],
        .row_index = row_index,
        .y = seq_along(.data$.row_index)
    )
    column_coords <- tibble0(
        .column_panels = column_panels[column_index],
        .column_index = column_index,
        .x = seq_along(.data$.column_index)
    )
    coords <- merge(column_coords, row_coords,
        by = NULL, sort = FALSE, all = TRUE
    )
    ans <- melt_matrix(matrix)
    merge(ans, coords, by = intersect(names(ans), names(coords)), all = TRUE)
}

ggheat_index <- function(panels, index) {
    unlist(split(index, panels[index]), recursive = FALSE, use.names = FALSE)
}
