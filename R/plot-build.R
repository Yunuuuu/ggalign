#' @export
print.ggheatmap <- function(x, ...) {
    p <- ggheat_build(x)
    print(p)
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggheatmap <- function(plot) {
    plot <- ggheat_build(plot)
    ggplot_build(plot)
}

ggheat_build <- function(x, ...) UseMethod("ggheat_build")

#' @export
ggheat_build.default <- function(x, ...) {
    cli::cli_abort("{.arg x} must be a {.cls ggheatmap} object")
}

#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom patchwork area
#' @importFrom grid unit is.unit unit.c
#' @export
ggheat_build.ggheatmap <- function(x, ...) {
    mat <- slot(x, "matrix")
    params <- slot(x, "params")
    row_panels <- slot(x, "row_panels") %||%
        factor(rep_len(1L, nrow(mat)))

    # we have reverse the panel order for heatmap rows
    row_index <- slot(x, "row_index") %||%
        reorder_index(row_panels)

    column_panels <- slot(x, "column_panels") %||%
        factor(rep_len(1L, ncol(mat)))
    column_index <- slot(x, "column_index") %||%
        reorder_index(column_panels)

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
            aes(.data$.x, .data$.y, fill = .data$value),
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
        "x", column_panels, column_index,
        .subset2(params, "xlabels"),
        nudge = .subset2(params, "xlabels_nudge")
    )
    default_yscales <- ggheat_default_scale(
        "y", row_panels, row_index,
        .subset2(params, "ylabels"),
        nudge = .subset2(params, "ylabels_nudge")
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
            scale_name = "x",
            .subset2(user_xscales, i),
            .subset2(default_xscales, i),
            "ggheat"
        )
        # we copy the expand from user input
        #   into the default for usage of annotation
        default_xscales[[i]]$expand <- .subset2(user_xscales, i)$expand
    }

    for (i in seq_along(default_yscales)) {
        user_yscales[[i]] <- ggheat_melt_scale(
            scale_name = "y",
            .subset2(user_yscales, i),
            .subset2(default_yscales, i),
            "ggheat"
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
        p <- p + ggheat_melt_facet(p$facet, default_facet) +
            ggh4x::facetted_pos_scales(x = user_xscales, y = user_yscales)
    } else if (do_row_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.row_panel)),
            scales = "free_y", space = "free_y"
        )
        p <- p + ggheat_melt_facet(p$facet, default_facet) +
            user_xscales +
            ggh4x::facetted_pos_scales(x = NULL, y = user_yscales)
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.column_panel),
            scales = "free_x", space = "free_x"
        )
        p <- p + ggheat_melt_facet(p$facet, default_facet) +
            user_yscales +
            ggh4x::facetted_pos_scales(x = user_xscales, y = NULL)
    } else {
        p <- p + user_xscales + user_yscales +
            ggheat_melt_facet(p$facet, NULL)
    }

    # plot annotations ---------------------------------
    annotations_list <- lapply(GGHEAT_ELEMENTS, function(position) {
        annotations <- slot(x, position)
        if (is_empty(annotations)) {
            return(list(NULL, NULL))
        }
        panels <- switch_position(position, row_panels, column_panels)
        index <- switch_position(position, row_index, column_index)
        # default facet for annotation
        anno_facet <- switch_position(
            position,
            if (do_row_facet) {
                ggplot2::facet_grid(
                    rows = ggplot2::vars(fct_rev(.data$.panel)),
                    scales = "free_y", space = "free_y"
                )
            },
            if (do_column_facet) {
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free_x", space = "free_x"
                )
            }
        )
        anno_scales <- switch_position(
            position,
            default_yscales,
            default_xscales
        )
        annotations_build(
            annotations = annotations,
            panels = panels,
            index = index,
            scales = anno_scales,
            facet = anno_facet,
            position = position
        )
    })
    names(annotations_list) <- GGHEAT_ELEMENTS
    annotations_list <- transpose(annotations_list)
    annotation_sizes <- .subset2(annotations_list, 2L) # annotation size
    annotations <- .subset2(annotations_list, 1L) # the annotation plot itself

    ans <- c(annotations, list(heatmap = list(p)))
    sizes <- c(
        annotation_sizes,
        list(
            heatmap_width = .subset2(params, "width"),
            heatmap_height = .subset2(params, "height")
        )
    )
    ggheatmap_patchwork(ans, sizes,
        guides = .subset2(params, "guides"),
        axes = .subset2(params, "axes"),
        axis_titles = .subset2(params, "axis_titles")
    )
}

#' @importFrom patchwork area
#' @importFrom grid unit.c
ggheatmap_patchwork <- function(plots, sizes, guides, axes, axis_titles) {
    n_top <- length(.subset2(plots, "top"))
    n_left <- length(.subset2(plots, "left"))
    n_bottom <- length(.subset2(plots, "bottom"))
    n_right <- length(.subset2(plots, "right"))
    # nolint start
    top_area <- lapply(seq_len(n_top), function(t) area(t, n_left + 1L))
    left_area <- lapply(seq_len(n_left), function(l) area(n_top + 1L, l))
    heatmap_area <- list(area(n_top + 1L, n_left + 1L))
    right_area <- lapply(seq_len(n_right), function(r) {
        area(n_top + 1L, n_left + 1L + r)
    })
    bottom_area <- lapply(seq_len(n_bottom), function(b) {
        area(n_top + 1L + b, n_left + 1L)
    })
    # nolint end
    # flatten plots and layout
    plots <- unlist(plots, recursive = FALSE, use.names = FALSE)
    layout <- lapply(
        paste0(c(GGHEAT_ELEMENTS, "heatmap"), "_area"), function(area) {
            eval(as.symbol(area))
        }
    )
    layout <- unlist(layout, recursive = FALSE, use.names = FALSE)
    layout <- do.call(c, layout)
    sizes <- lapply(list(
        heights = c("top", "heatmap_height", "bottom"),
        widths = c("left", "heatmap_width", "right")
    ), function(nms) {
        sizes <- .subset(sizes, nms)
        sizes <- sizes[lengths(sizes) > 0L]
        do.call(unit.c, sizes)
    })

    Reduce(`+`, plots) +
        patchwork::plot_layout(
            design = layout,
            heights = .subset2(sizes, "heights"),
            widths = .subset2(sizes, "widths"),
            guides = guides,
            axes = axes,
            axis_titles = axis_titles
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

#' @importFrom rlang is_empty
ggheat_extract_scales <- function(scale_name, plot, n, facet_scales) {
    single_scale <- plot$scales$get_scales(scale_name)
    if (!is.null(facet_scales)) {
        scales <- .subset2(facet_scales, scale_name)
        if (n > 1L) {
            if (is_empty(scales)) {
                scales <- vector("list", n)
            } else if (length(scales) < n) {
                cli::cli_warn("No enough facetted {.field {scale_name}} scales")
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
        # filling scales with user normal scale
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

    # will change the user input, so we must use `ggproto_clone`
    user_facet <- ggproto_clone(user_facet)
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

ggheat_melt_scale <- function(scale_name, user_scale, default_scale, type) {
    if (is.null(user_scale)) {
        ans <- default_scale$clone()
    } else {
        ans <- user_scale$clone()
        # always reset the limits
        # should we merge user provided limits for heatmap only?
        # and all annotation will use this limits too.
        ans$limits <- default_scale$limits

        #  it's not possible for user to know the `breaks` or `labels`
        #  since a heatmap often reorder columns or rows.
        #  so we always set the breaks or labels into the default if user not
        #  remove it.
        if (!is.null(ans$breaks) && !is.waiver(ans$breaks)) {
            cli::cli_warn(c(
                "{.arg breaks} will be ignored",
                i = sprintf(
                    "try to set {.arg {%s}labels_nudge}",
                    if (type == "ggheat") scale_name,
                    "in {.fn %s}", type
                )
            ), call = ans$call)
        }
        if (!is.null(ans$breaks)) {
            ans$breaks <- default_scale$breaks
        }

        if (!is.null(ans$labels) && !is.waiver(ans$labels)) {
            cli::cli_warn(c(
                "{.arg labels} will be ignored",
                i = sprintf(
                    "try to set {.arg {%s}labels}",
                    if (type == "ggheat") scale_name,
                    "in {.fn %s}", type
                )
            ), call = ans$call)
        }

        if (!is.null(ans$labels)) {
            ans$labels <- default_scale$labels
        }

        # for heatmap, we only set the default expand when it's waiver.
        # for annotation, we always override the expand
        if (type != "ggheat" || is.waiver(ans$expand)) {
            ans$expand <- default_scale$expand
        }
    }
    ans
}

ggheat_default_scale <- function(scale_name, panels, index, labels, nudge,
                                 expand = ggplot2::expansion()) {
    if (is.numeric(nudge)) {
        nudge <- nudge[index]
    } else if (is.waiver(nudge)) {
        nudge <- rep_len(0, length(index))
    }
    panels <- panels[index]
    # For row, ggplot arrange panels from top to bottom,
    # we always choose to reverse the panel order
    if (scale_name == "y") panels <- fct_rev(panels)
    labels <- labels[index]
    fn <- switch(scale_name,
        x = ggplot2::scale_x_continuous,
        y = ggplot2::scale_y_continuous
    )
    data <- split(seq_along(index), panels)
    if (!is.list(expand)) expand <- rep_len(list(expand), length(data))
    .mapply(function(x, expand) {
        if (is.null(nudge)) {
            breaks <- NULL
            labels <- NULL
        } else {
            breaks <- x + nudge[x]
            labels <- labels[x]
        }
        fn(
            name = NULL, # we by default always remove the annotation name
            limits = range(x) + c(-0.5, 0.5),
            breaks = breaks,
            labels = labels,
            expand = ggplot2::expansion()
        )
    }, list(x = data, expand = expand), NULL)
}

ggheat_build_data <- function(matrix, row_panels, row_index,
                              column_panels, column_index) {
    row_coords <- tibble0(
        .row_panel = row_panels[row_index],
        .row_index = row_index,
        .y = seq_along(row_index)
    )
    column_coords <- tibble0(
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
