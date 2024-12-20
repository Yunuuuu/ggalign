#' @importFrom grid unit.c
#' @export
ggalign_build.QuadLayout <- function(x) {
    x <- default_layout(x)
    patches <- quad_build(x)
    plots <- .subset2(patches, "plots")
    sizes <- .subset2(patches, "sizes")
    design <- list(
        top = area(1, 2),
        left = area(2, 1),
        main = area(2, 2),
        bottom = area(3, 2),
        right = area(2, 3)
    )
    sizes <- imap(list(
        height = c("top", "main", "bottom"),
        width = c("left", "main", "right")
    ), function(x, name) {
        out <- .subset(sizes, x)
        out$main <- .subset2(.subset2(out, "main"), name)
        out <- .subset(
            out,
            !vapply(.subset(plots, x), is.null, logical(1L), USE.NAMES = FALSE)
        )
        do.call(unit.c, out)
    })
    keep <- !vapply(plots, is.null, logical(1L), USE.NAMES = FALSE)
    design <- trim_area(vec_c(!!!vec_set_names(vec_slice(design, keep), NULL)))
    titles <- x@titles
    align_plots(
        !!!.subset(plots, keep),
        design = design,
        heights = .subset2(sizes, "height"),
        widths = .subset2(sizes, "width"),
        guides = .subset2(.subset2(x@schemes, "scheme_align"), "guides"),
        theme = x@theme
    ) + layout_title(
        title = .subset2(titles, "title"),
        subtitle = .subset2(titles, "subtitle"),
        caption = .subset2(titles, "caption")
    )
}

quad_build <- function(quad, schemes = NULL, theme = NULL,
                       direction = NULL) {
    UseMethod("quad_build")
}

#######################################################################
#' @param schemes,theme Parameters from parent layout
#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom grid unit is.unit unit.c
#' @export
#' @noRd
quad_build.QuadLayout <- function(quad, schemes = NULL, theme = NULL,
                                  direction = NULL) {
    data <- quad@data
    schemes <- inherit_parent_layout_schemes(quad, schemes)
    theme <- inherit_parent_layout_theme(quad, theme, direction = direction)
    row_coords <- setup_discrete_design(quad@horizontal)
    column_coords <- setup_discrete_design(quad@vertical)
    if (!(is.null(row_coords) && is.null(column_coords)) &&
        (is.function(data) || is.null(data))) {
        cli_abort(c(
            sprintf(
                "You must provide {.arg data} argument to plot %s",
                object_name(quad)
            ),
            i = "Do you want to put this in a parent stack layout?"
        ))
    }

    # prepare action for vertical and horizontal stack layout
    vertical_align <- horizontal_align <- align <-
        .subset2(schemes, "scheme_align")
    if (!is.null(layout_labs <- .subset2(align, "free_labs")) &&
        !is.waive(layout_labs)) {
        # prepare labs for child stack layout
        horizontal_align$free_labs <- gsub("[lr]", "", layout_labs)
        vertical_align$free_labs <- gsub("[tb]", "", layout_labs)
        if (!nzchar(horizontal_align$free_labs)) {
            horizontal_align["free_labs"] <- list(NULL)
        }
        if (!nzchar(vertical_align$free_labs)) {
            vertical_align["free_labs"] <- list(NULL)
        }
    }

    # inherit from the parent stack layout
    if (!is.null(layout_spaces <- .subset2(align, "free_spaces")) &&
        !is.waive(layout_spaces)) {
        horizontal_align$free_spaces <- gsub("[lr]", "", layout_spaces)
        vertical_align$free_spaces <- gsub("[tb]", "", layout_spaces)
        if (!nzchar(horizontal_align$free_spaces)) {
            horizontal_align["free_spaces"] <- list(NULL)
        }
        if (!nzchar(vertical_align$free_spaces)) {
            vertical_align["free_spaces"] <- list(NULL)
        }
    }

    # plot annotations ----------------------------
    stack_list <- lapply(.TLBR, function(position) {
        if (is_empty(stack <- slot(quad, position))) {
            return(list(plot = NULL, size = NULL))
        }
        pschemes <- schemes
        # inherit from horizontal align or vertical align
        if (is_horizontal(to_direction(position))) {
            extra_coords <- column_coords
            pschemes$scheme_align <- horizontal_align
        } else {
            extra_coords <- row_coords
            pschemes$scheme_align <- vertical_align
        }
        plot <- stack_build(
            stack,
            schemes = pschemes,
            theme = theme,
            extra_coords = extra_coords
        )
        if (is.null(plot)) {
            size <- NULL
        } else {
            size <- stack@sizes
        }
        list(plot = plot, size = size)
    })
    names(stack_list) <- .TLBR
    stack_list <- list_transpose(stack_list)
    plots <- .subset2(stack_list, 1L) # the annotation plot itself
    sizes <- .subset2(stack_list, 2L) # annotation size

    # read the plot ---------------------------------------
    p <- quad@plot

    # add default data ----------------------------------
    p$data <- quad_build_data(data, row_coords, column_coords)

    # setup the facet -----------------------------------
    do_row_facet <- !is.null(row_coords) &&
        nlevels(.subset2(row_coords, "panel")) > 1L
    do_column_facet <- !is.null(column_coords) &&
        nlevels(.subset2(column_coords, "panel")) > 1L

    if (do_row_facet && do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.ypanel),
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free", space = "free",
            drop = FALSE, as.table = FALSE
        )
    } else if (do_row_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(.data$.ypanel),
            scales = "free_y", space = "free",
            drop = FALSE, as.table = FALSE
        )
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free_x", space = "free",
            drop = FALSE, as.table = FALSE
        )
    } else {
        # we only support `FacetNull` if there have no panel
        default_facet <- ggplot2::facet_null()
    }

    # set limits ----------------------------------------
    if (!is.null(column_coords)) {
        column_coords$labels <- .subset(
            colnames(data),
            .subset2(column_coords, "index")
        )
    }
    if (!is.null(row_coords)) {
        row_coords$labels <- .subset(
            vec_names(data),
            .subset2(row_coords, "index")
        )
    }

    # set the facets and coord ---------------------------
    # we don't align observations for `quad_free()`
    aligned_axes <- c("x", "y")[
        c(!is.null(column_coords), !is.null(row_coords))
    ]
    if (length(aligned_axes)) {
        p <- p +
            align_melt_facet(default_facet, p$facet, strict = TRUE) +
            cartesian_coord(aligned_axes, object_name(quad)) +
            discrete_ggalign(x = column_coords, y = row_coords)
    }
    p <- p + theme_recycle()

    # add action ----------------------------------------
    p <- plot_add_schemes(p, inherit_schemes(quad@body_schemes, schemes))
    if (!is.null(row_coords)) {
        p <- p + theme(panel.spacing.y = calc_element("panel.spacing.y", theme))
    }
    if (!is.null(column_coords)) {
        p <- p + theme(panel.spacing.x = calc_element("panel.spacing.x", theme))
    }

    # collect all plots and sizes ----------------------
    plots <- append(plots, list(main = p), 2L)
    sizes <- append(
        sizes,
        list(main = list(width = quad@width, height = quad@height)),
        3L
    )
    list(plots = plots, sizes = sizes)
}

#' @importFrom stats reorder
quad_build_data <- function(data, row_coords, column_coords) {
    if (is.null(data) || (is.null(row_coords) && is.null(column_coords))) {
        # ggplot use waiver() to indicates the NULL data
        return(data %||% waiver())
    }
    if (!is.null(row_coords)) {
        row_panel <- .subset2(row_coords, "panel")
        row_index <- .subset2(row_coords, "index")
        row_coords <- data_frame0(
            .ypanel = row_panel,
            .yindex = row_index,
            .y = seq_along(row_index)
        )
    }
    if (!is.null(column_coords)) {
        column_panel <- .subset2(column_coords, "panel")
        column_index <- .subset2(column_coords, "index")
        column_coords <- data_frame0(
            .xpanel = column_panel,
            .xindex = column_index,
            .x = seq_along(column_index)
        )
    }
    if (!is.null(row_coords) && !is.null(column_coords)) {
        coords <- cross_join(row_coords, column_coords)
        by.x <- c(".column_index", ".row_index")
        by.y <- c(".xindex", ".yindex")
    } else if (is.null(row_coords)) {
        coords <- column_coords
        by.x <- ".column_index"
        by.y <- ".xindex"
    } else {
        coords <- row_coords
        by.x <- ".row_index"
        by.y <- ".yindex"
    }
    ans <- fortify_data_frame.matrix(data)
    ans <- full_join(ans, coords, by.x = by.x, by.y = by.y)
    if (!is.null(.subset2(ans, ".row_names")) &&
        !is.null(row_coords)) {
        ans$.discrete_y <- reorder(
            .subset2(ans, ".row_names"),
            .subset2(ans, ".y"),
            order = FALSE
        )
    }
    if (!is.null(.subset2(ans, ".column_names")) &&
        !is.null(column_coords)) {
        ans$.discrete_x <- reorder(
            .subset2(ans, ".column_names"),
            .subset2(ans, ".x"),
            order = FALSE
        )
    }
    ggalign_attr_restore(ans, data)
}
