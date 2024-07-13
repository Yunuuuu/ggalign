#' @export
print.ggheatmap <- function(x, ...) {
    p <- ggheat_build(x)
    print(p)
}

ggscale_map <- function(plot, scale, value) {
    build <- ggplot2::ggplot_build(plot)
    if (!is.null(scale <- build$plot$scales$get_scales(scale))) {
        scale$map(value)
    } else {
        cli::cli_abort("Cannot find {.field {scale}}")
    }
}

ggheat_build <- function(x, ...) UseMethod("ggheat_build")

#' @export
ggheat_build.default <- function(x, ...) {
    cli::cli_abort("{.arg x} must be a {.cls ggheatmap} object")
}

#' @importFrom patchwork area
#' @importFrom grid unit is.unit unit.c
#' @export
ggheat_build.ggheatmap <- function(x, ...) {
    mat <- slot(x, "matrix")
    params <- slot(x, "params")
    row_coords <- ggheat_build_coords(
        nrow(mat),
        slot(x, "row_slice"),
        slot(x, "row_order")
    )
    column_coords <- ggheat_build_coords(
        ncol(mat),
        slot(x, "column_slice"),
        slot(x, "column_order")
    )
    xlabels <- .subset2(params, "xlabels") %||% colnames(mat)
    ylabels <- .subset2(params, "ylabels") %||% rownames(mat)

    do_row_facet <- length(unique(.subset2(row_coords, ".panel"))) > 1L
    do_column_facet <- length(unique(.subset2(column_coords, ".panel"))) > 1L

    # limit scales --------------------------------------
    # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
    # There are multiple ways to set labels in a plot, which take different
    # priorities. Here are the priorities from highest to lowest.
    # 1. The guide title.
    # 2. The scale name.
    # 3. The `labs()` function.
    # 4. The captured expression in aes().
    xscale <- ggplot2::scale_x_continuous(
        name = .subset2(params, "xlab"),
        # limits = c(0.5, ncol(mat) + 0.5),
        breaks = seq_len(ncol(mat)),
        labels = xlabels,
        expand = ggplot2::expansion()
    )
    yscale <- ggplot2::scale_y_continuous(
        name = .subset2(params, "ylab"),
        # limits = c(0.5, nrow(mat) + 0.5),
        breaks = seq_len(nrow(mat)),
        labels = ylabels,
        expand = ggplot2::expansion()
    )

    # prepare annotations ---------------------------------
    annotations <- lapply(GGHEAT_ELEMENTS, function(position) {
        annotations <- slot(x, position)
        if (length(annotations) == 0L) {
            return(list(NULL, NULL))
        } # styler: off
        plots <- lapply(
            annotations,
            anno_build,
            coords = switch_position(position, row_coords, column_coords),
            position = position,
            scale = switch_position(position, yscale, xscale),
            facet = switch_position(
                position,
                if (do_row_facet) {
                    ggplot2::facet_grid(
                        rows = ggplot2::vars(.data$.panel),
                        scales = "free_y"
                    )
                },
                if (do_column_facet) {
                    ggplot2::facet_grid(
                        cols = ggplot2::vars(.data$.panel),
                        scales = "free_x"
                    )
                }
            )
        )
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
                patchwork::wrap_plots(plots, nrow = 1L, widths = sizes),
                patchwork::wrap_plots(plots, ncol = 1L, heights = sizes)
            ),
            sum(sizes)
        )
    })
    names(annotations) <- GGHEAT_ELEMENTS
    annotations <- transpose(annotations)
    annotation_sizes <- .subset2(annotations, 2L) # annotation size
    annotations <- .subset2(annotations, 1L) # the annotation plot itself

    # prepare data for plot -------------------------------
    data <- ggheat_build_data(mat, row_coords, column_coords)

    # contruct the final plot ----------------------------
    p <- slot(x, "heatmap")
    p$data <- data # change the default data.frame
    p <- p +
        ggplot2::geom_tile(
            ggplot2::aes(.data$.x, .data$.y, fill = .data$value),
            width = 1L, height = 1L,
            data = data
        )

    p <- p + xscale + yscale
    if (do_row_facet && do_column_facet) {
        p <- p + ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.row_slice),
            cols = ggplot2::vars(.data$.column_slice),
            scales = "free"
        )
    } else if (do_row_facet) {
        p <- p + ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.row_slice),
            scales = "free_y"
        )
    } else if (do_column_facet) {
        p <- p + ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.column_slice),
            scales = "free_x"
        )
    }
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
        widths = .subset2(sizes, "widths")
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

ggheat_build_coords <- function(n, slice, order) {
    order <- order %||% seq_len(n)
    if (is.null(slice)) {
        slice <- list(order)
    } else {
        slice <- split(order, slice)
    }
    levels <- names(slice) %||% seq_along(slice)
    tibble0(
        .panel = factor(rep(levels, times = lengths(slice)), levels),
        .index = unlist(slice, recursive = FALSE, use.names = FALSE),
        .coord = seq_along(.data$.index)
    )
}

ggheat_build_data <- function(matrix, row_coords, column_coords) {
    row_coords <- rename(
        row_coords,
        c(.panel = ".row_slice", .index = ".row_index", .coord = ".y")
    )
    column_coords <- rename(
        column_coords,
        c(.panel = ".column_slice", .index = ".column_index", .coord = ".x")
    )
    coords <- merge(column_coords, row_coords,
        by = NULL, sort = FALSE, all = TRUE
    )
    ans <- melt_matrix(matrix)
    merge(ans, coords, by = intersect(names(ans), names(coords)), all = TRUE)
}
