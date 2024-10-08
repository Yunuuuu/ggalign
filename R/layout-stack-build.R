#' @export
ggalign_build.StackLayout <- function(x) {
    layout_theme <- x@theme %||% default_theme()
    # we by default, collect all guides
    x@params$guides <- .subset2(x@params, "guides") %|w|% "tlbr"
    .subset2(stack_build(x, layout_theme = layout_theme), "plot") %||%
        align_plots()
}

#' @param panel,index layout of the axis vertically with the stack.
#' @importFrom grid unit.c
#' @importFrom rlang is_empty
#' @noRd
stack_build <- function(x, plot_data = waiver(), free_labs = waiver(),
                        free_spaces = waiver(), theme = waiver(),
                        layout_theme = waiver(),
                        extra_panel = NULL, extra_index = NULL) {
    if (is.na(nobs <- get_nobs(x))) { # no plots
        return(list(plot = NULL, size = NULL))
    }
    direction <- x@direction
    params <- x@params
    panel <- get_panel(x) %||% factor(rep_len(1L, nobs))
    index <- get_index(x) %||% reorder_index(panel)

    plots <- x@plots

    # we remove the plot without actual plot area
    keep <- vapply(plots, function(plot) {
        (is_align(plot) && !is.null(.subset2(plot, "plot"))) ||
            is_ggheatmap(plot)
    }, logical(1L), USE.NAMES = FALSE)
    plots <- .subset(plots, keep)
    if (is_empty(plots)) {
        return(list(plot = NULL, size = NULL))
    }

    # we reorder the plots based on the `order` slot
    plot_order <- vapply(plots, function(plot) {
        if (is_align(plot)) {
            .subset2(plot, "order")
        } else {
            plot@order
        }
    }, integer(1L), USE.NAMES = FALSE)
    plots <- .subset(plots, make_order(plot_order))

    # build the stack
    plot_data <- .subset2(params, "plot_data") %|w|% plot_data
    free_labs <- .subset2(params, "free_labs") %|w|% free_labs
    free_spaces <- .subset2(params, "free_spaces") %|w|% free_spaces
    theme <- inherit_theme(.subset2(params, "theme"), theme)
    patches <- stack_patch(direction)
    has_top <- FALSE
    has_bottom <- FALSE
    for (plot in plots) {
        if (is_align(plot)) {
            patch <- align_build(plot,
                panel = panel, index = index,
                extra_panel = extra_panel,
                extra_index = extra_index,
                plot_data = plot_data %|w|% NULL,
                free_labs = free_labs %|w|% "tlbr",
                free_spaces = free_spaces %|w|% NULL,
                theme = theme
            )
            patches <- stack_patch_add_align(
                patches,
                .subset2(patch, "plot"),
                .subset2(patch, "size")
            )
        } else if (is_ggheatmap(plot)) {
            # for a heatmap
            patch <- heatmap_build(plot,
                plot_data = plot_data,
                free_labs = free_labs,
                free_spaces = free_spaces,
                theme = theme,
                layout_theme = layout_theme
            )
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
    annotation <- x@annotation
    plot <- align_plots(
        !!!.subset2(patches, "plots"),
        design = area(
            .subset2(patches, "t"),
            .subset2(patches, "l"),
            .subset2(patches, "b"),
            .subset2(patches, "r")
        ),
        widths = switch_direction(
            direction,
            do.call(unit.c, attr(patches, "sizes")),
            .subset2(params, "sizes")[c(has_top, TRUE, has_bottom)]
        ),
        heights = switch_direction(
            direction,
            .subset2(params, "sizes")[c(has_top, TRUE, has_bottom)],
            do.call(unit.c, attr(patches, "sizes"))
        ),
        guides = .subset2(params, "guides"),
        title = .subset2(annotation, "title"),
        subtitle = .subset2(annotation, "subtitle"),
        caption = .subset2(annotation, "caption"),
        theme = layout_theme
    )
    list(plot = plot, size = .subset2(params, "size"))
}

stack_patch <- function(direction) {
    ans <- list(
        t = integer(), l = integer(), b = integer(), r = integer(),
        plots = list()
    )
    structure(ans, direction = direction, align = 1L, sizes = list())
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
    attr(area, "sizes") <- c(attr(area, "sizes"), list(size))
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
            if (attr(area, "align") == 1L) {
                area$t <- .subset2(area, "t") + 1L
                area$b <- .subset2(area, "b") + 1L
                attr(area, "align") <- attr(area, "align") + 1L
            }
            if (!is_null_unit(size <- .subset2(sizes, "top"))) {
                attr(top, "vp")$height <- size
            }
            area <- stack_patch_add_plot(area, top, t = 1L, l = l)
        }
        if (!is.null(bottom <- .subset2(plots, "bottom"))) {
            if (!is_null_unit(size <- .subset2(sizes, "bottom"))) {
                attr(bottom, "vp")$height <- size
            }
            area <- stack_patch_add_plot(area, bottom,
                t = attr(area, "align") + 1L, l = l
            )
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
            if (attr(area, "align") == 1L) {
                area$l <- .subset2(area, "l") + 1L
                area$r <- .subset2(area, "r") + 1L
                attr(area, "align") <- attr(area, "align") + 1L
            }
            if (!is_null_unit(size <- .subset2(sizes, "left"))) {
                attr(left, "vp")$width <- size
            }
            area <- stack_patch_add_plot(area, left, t = t, l = 1L)
        }
        if (!is.null(right <- .subset2(plots, "right"))) {
            if (!is_null_unit(size <- .subset2(sizes, "right"))) {
                attr(right, "vp")$width <- size
            }
            area <- stack_patch_add_plot(area, right,
                t = t, l = attr(area, "align") + 1L
            )
        }
        area <- stack_patch_add_align(
            area,
            .subset2(plots, "bottom"),
            .subset2(sizes, "bottom")
        )
    }
    area
}
