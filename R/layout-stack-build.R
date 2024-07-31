#' @export
build_patchwork.LayoutStack <- function(layout) {
    .subset2(stack_build(layout), "plot")
}

#' @importFrom grid unit.c
#' @importFrom patchwork area
#' @importFrom rlang is_empty
stack_build <- function(x) {
    panels <- get_panels(x) %||% factor(rep_len(1L, nrow(slot(x, "data"))))
    index <- get_index(x) %||% reorder_index(panels)
    direction <- slot(x, "direction")
    plots <- slot(x, "plots")

    # we reorder the plots based on the `order` slot
    stack_index <- order(vapply(plots, function(plot) {
        if (is.align(plot)) {
            .subset2(plot, "order")
        } else {
            NA_integer_
        }
    }, integer(1L)))
    plots <- .subset(plots, stack_index)
    patches <- stack_patch(direction)
    params <- slot(x, "params")
    has_top <- FALSE
    has_bottom <- FALSE
    for (plot in plots) {
        if (is.align(plot)) {
            patch <- align_build(plot, panels = panels, index = index)
            patches <- stack_patch_add_align(
                patches,
                .subset2(patch, "plot"),
                .subset2(patch, "size")
            )
        } else if (is.ggheatmap(plot)) {
            patch <- heatmap_build(plot)
            heatmap_plots <- .subset2(patch, "plots")
            patches <- stack_patch_add_heatmap(
                patches, heatmap_plots,
                .subset2(patch, "sizes")
            )
            if (is_horizontal(direction)) {
                has_top <- has_top || !is.null(.subset2(heatmap_plots, "top"))
                has_bottom <- has_bottom ||
                    !is.null(.subset2(heatmap_plots, "bottom"))
            } else {
                has_top <- has_top || !is.null(.subset2(heatmap_plots, "left"))
                has_bottom <- has_bottom ||
                    !is.null(.subset2(heatmap_plots, "right"))
            }
        }
    }
    if (is_empty(.subset2(patches, "plots"))) {
        return(list(plot = NULL, size = NULL))
    }
    plot <- patchwork::wrap_plots(
        .subset2(patches, "plots"),
        design = area(
            .subset2(patches, "t"),
            .subset2(patches, "l"),
            .subset2(patches, "b"),
            .subset2(patches, "r")
        ),
        widths = switch_direction(
            direction,
            do.call(unit.c, attr(patches, "rel_sizes")),
            .subset2(params, "rel_sizes")[c(has_top, TRUE, has_bottom)]
        ),
        heights = switch_direction(
            direction,
            .subset2(params, "rel_sizes")[c(has_top, TRUE, has_bottom)],
            do.call(unit.c, attr(patches, "rel_sizes"))
        ),
        guides = .subset2(params, "guides")
    )
    list(plot = plot, size = slot(x, "size"))
}

stack_patch <- function(direction) {
    ans <- list(
        t = integer(), l = integer(), b = integer(), r = integer(),
        plots = list()
    )
    structure(ans, direction = direction, align = 1L, rel_sizes = list())
}

stack_patch_add_plot <- function(area, plot, t, l, b = t, r = l) {
    area$t <- c(.subset2(area, "t"), t)
    area$l <- c(.subset2(area, "l"), l)
    area$b <- c(.subset2(area, "b"), b)
    area$r <- c(.subset2(area, "r"), r)
    area$plots <- c(.subset2(area, "plots"), list(plot))
    area
}

#' @importFrom rlang is_empty
stack_patch_add_align <- function(area, plot, size) {
    if (is.null(plot)) {
        return(area)
    }
    if (is_horizontal(attr(area, "direction"))) {
        r_border <- .subset2(area, "r")
        if (is_empty(r_border)) r_border <- 0L
        l <- max(r_border) + 1L
        t <- attr(area, "align")
    } else {
        b_border <- .subset2(area, "b")
        if (is_empty(b_border)) b_border <- 0L
        t <- max(b_border) + 1L
        l <- attr(area, "align")
    }
    attr(area, "rel_sizes") <- c(attr(area, "rel_sizes"), list(size))
    stack_patch_add_plot(area, plot, t, l)
}

#' @importFrom grid unit.c unit
stack_patch_add_heatmap <- function(area, plots, sizes) {
    if (is_horizontal(attr(area, "direction"))) {
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "left"),
            .subset2(sizes, "left")
        )
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "heatmap"),
            .subset2(.subset2(sizes, "heatmap"), "width")
        )
        l <- max(.subset2(area, "r"))
        if (!is.null(top <- .subset2(plots, "top"))) {
            size <- .subset2(sizes, "top")
            if (attr(area, "align") == 1L) {
                area$t <- .subset2(area, "t") + 1L
                area$b <- .subset2(area, "b") + 1L
                attr(area, "align") <- attr(area, "align") + 1L
            }
            if (is_null_unit(size)) {
                top <- patchwork::wrap_plots(top, heights = size)
            } else {
                top <- patchwork::wrap_plots(
                    patchwork::plot_spacer(),
                    top,
                    ncol = 1L,
                    heights = unit.c(unit(1L, "npc") - size, size)
                )
            }
            area <- stack_patch_add_plot(area, top, t = 1L, l = l)
        }
        if (!is.null(bottom <- .subset2(plots, "bottom"))) {
            size <- .subset2(sizes, "bottom")
            if (is_null_unit(size)) {
                bottom <- patchwork::wrap_plots(bottom, heights = size)
            } else {
                bottom <- patchwork::wrap_plots(
                    bottom,
                    patchwork::plot_spacer(),
                    ncol = 1L,
                    heights = unit.c(unit(1L, "npc") - size, size)
                )
            }
            area <- stack_patch_add_plot(area, top, t = 3L, l = l)
        }
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "right"),
            .subset2(sizes, "right")
        )
    } else {
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "top"),
            .subset2(sizes, "top")
        )
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "heatmap"),
            .subset2(.subset2(sizes, "heatmap"), "height")
        )
        t <- max(.subset2(area, "b"))
        if (!is.null(left <- .subset2(plots, "left"))) {
            size <- .subset2(sizes, "left")
            if (attr(area, "align") == 1L) {
                area$l <- .subset2(area, "l") + 1L
                area$r <- .subset2(area, "r") + 1L
                attr(area, "align") <- attr(area, "align") + 1L
            }
            if (is_null_unit(size)) {
                left <- patchwork::wrap_plots(left, heights = size)
            } else {
                left <- patchwork::wrap_plots(
                    patchwork::plot_spacer(),
                    left,
                    nrow = 1L,
                    heights = unit.c(unit(1L, "npc") - size, size)
                )
            }
            area <- stack_patch_add_plot(area, left, t = t, l = 1L)
        }
        if (!is.null(right <- .subset2(plots, "right"))) {
            size <- .subset2(sizes, "right")
            if (is_null_unit(size)) {
                right <- patchwork::wrap_plots(right, heights = size)
            } else {
                right <- patchwork::wrap_plots(
                    right,
                    patchwork::plot_spacer(),
                    ncol = 1L,
                    heights = unit.c(unit(1L, "npc") - size, size)
                )
            }
            area <- stack_patch_add_plot(area, top, t = t, l = 3L)
        }
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "bottom"),
            .subset2(sizes, "bottom")
        )
    }
    area
}
