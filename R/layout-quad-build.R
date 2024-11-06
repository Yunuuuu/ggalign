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
        bottom = area(3, 2),
        right = area(2, 3),
        main = area(2, 2)
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
        guides = .subset2(.subset2(x@controls, "plot_align"), "guides"),
        theme = x@theme
    ) + layout_title(
        title = .subset2(titles, "title"),
        subtitle = .subset2(titles, "subtitle"),
        caption = .subset2(titles, "caption")
    )
}

quad_build <- function(quad, controls = quad@controls) UseMethod("quad_build")

#######################################################################
#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom grid unit is.unit unit.c
#' @export
quad_build.QuadLayout <- function(quad, controls = quad@controls) {
    data <- quad@data
    if (is.function(data) || is.null(data)) {
        cli::cli_abort(c(
            "You must provide {.arg data} argument to plot {.fn {quad@name}}",
            i = "Do you want to put this in a parent stack layout?"
        ))
    }
    row_params <- set_layout_params(quad@horizontal)
    column_params <- set_layout_params(quad@vertical)

    # prepare action for vertical and horizontal stack layout
    vertical_align <- horizontal_align <- align <-
        .subset2(controls, "plot_align")
    if (!is.null(layout_labs <- .subset2(align, "free_labs")) &&
        !is.waive(layout_labs)) {
        # prepare labs for child stack layout
        horizontal_align$free_labs <- gsub("[lr]", "", layout_labs)
        vertical_align$free_labs <- gsub("[tb]", "", layout_labs)
        if (nchar(horizontal_align$free_labs) == 0L) {
            horizontal_align["free_labs"] <- list(NULL)
        }
        if (nchar(vertical_align$free_labs) == 0L) {
            vertical_align["free_labs"] <- list(NULL)
        }
    }

    # inherit from the parent stack layout
    if (!is.null(layout_spaces <- .subset2(align, "free_spaces")) &&
        !is.waive(layout_spaces)) {
        horizontal_align$free_spaces <- gsub("[lr]", "", layout_spaces)
        vertical_align$free_spaces <- gsub("[tb]", "", layout_spaces)
        if (nchar(horizontal_align$free_spaces) == 0L) {
            horizontal_align["free_spaces"] <- list(NULL)
        }
        if (nchar(vertical_align$free_spaces) == 0L) {
            vertical_align["free_spaces"] <- list(NULL)
        }
    }

    # plot annotations ----------------------------
    stack_list <- lapply(.TLBR, function(position) {
        if (is_empty(stack <- slot(quad, position))) {
            return(list(plot = NULL, size = NULL))
        }
        stack_controls <- controls
        # inherit from horizontal align or vertical align
        if (is_horizontal(to_direction(position))) {
            params <- column_params
            stack_controls$plot_align <- horizontal_align
        } else {
            params <- row_params
            stack_controls$plot_align <- vertical_align
        }
        plot <- stack_build(stack,
            controls = inherit_controls(stack@controls, stack_controls),
            extra_layout = params
        )
        if (!is.null(plot)) {
            # for annotation, we should always make them next to
            # the main body
            plot <- free_vp(
                plot,
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

            # whether we should override the `guides` collection for the whole
            # annotation stack
            free_guides <- .subset2(stack@heatmap, "free_guides")
            if (!is.waive(free_guides)) plot <- free_guide(plot, free_guides)
            # we also apply the `free_spaces` for the whole annotation stack
            free_spaces <- .subset2(
                .subset2(stack_controls, "plot_align"), "free_spaces"
            ) %|w|% NULL
            if (!is.null(free_spaces)) {
                plot <- free_space(free_border(plot, free_spaces), free_spaces)
            }
            size <- .subset2(stack@heatmap, "size")
        } else {
            size <- NULL
        }
        list(plot = plot, size = size)
    })
    names(stack_list) <- .TLBR
    stack_list <- transpose(stack_list)
    plots <- .subset2(stack_list, 1L) # the annotation plot itself
    sizes <- .subset2(stack_list, 2L) # annotation size

    # read the plot ---------------------------------------
    p <- quad@plot

    # add default data ----------------------------------
    p$data <- quad_build_data(data, row_params, column_params)

    # setup the facet -----------------------------------
    do_row_facet <- !is.null(row_params) &&
        nlevels(.subset2(row_params, "panel")) > 1L
    do_column_facet <- !is.null(column_params) &&
        nlevels(.subset2(column_params, "panel")) > 1L

    if (do_row_facet && do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.ypanel)),
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free", space = "free",
            drop = FALSE
        )
    } else if (do_row_facet) {
        default_facet <- ggplot2::facet_grid(
            rows = ggplot2::vars(fct_rev(.data$.ypanel)),
            scales = "free_y", space = "free",
            drop = FALSE
        )
    } else if (do_column_facet) {
        default_facet <- ggplot2::facet_grid(
            cols = ggplot2::vars(.data$.xpanel),
            scales = "free_x", space = "free",
            drop = FALSE
        )
    } else {
        # we only support `FacetNull` if there have no panel
        default_facet <- ggplot2::facet_null()
    }

    # set limits ----------------------------------------
    if (is.null(column_params)) {
        xlim_list <- NULL
    } else {
        column_params$labels <- colnames(data)
        xlim_list <- set_limits("x", column_params)
        if (!is.null(row_params)) {
            xlim_list <- vec_rep(xlim_list,
                times = nlevels(.subset2(row_params, "panel"))
            )
        }
    }
    if (is.null(row_params)) {
        ylim_list <- NULL
    } else {
        row_params$labels <- vec_names(data)
        ylim_list <- set_limits("y", row_params)
        if (!is.null(column_params)) {
            ylim_list <- vec_rep_each(ylim_list,
                times = nlevels(.subset2(column_params, "panel"))
            )
        }
    }

    # set the facets and coord ---------------------------
    # we don't align observations for `quad_free()`
    if (!is.null(row_params) || !is.null(column_params)) {
        p <- p + quad_melt_facet(p$facet, default_facet) +
            facet_ggalign(x = column_params, y = row_params) +
            coord_ggalign(xlim_list = xlim_list, ylim_list = ylim_list)
    }

    # add action ----------------------------------------
    p <- plot_add_controls(p, inherit_controls(quad@body_controls, controls))

    # collect all plots and sizes ----------------------
    plots <- c(plots, list(main = p))
    sizes <- c(sizes, list(
        main = list(width = quad@width, height = quad@height)
    ))
    list(plots = plots, sizes = sizes)
}

#' @importFrom ggplot2 ggproto
quad_melt_facet <- function(user_facet, default_facet) {
    if (inherits(default_facet, "FacetNull")) { # no panel
        # we only support `FacetNull` if there have no panel
        if (inherits(user_facet, "FacetNull")) return(user_facet) # styler: off
        return(default_facet)
    }

    if (!inherits(user_facet, "FacetGrid")) return(default_facet) # styler: off

    # re-dispatch parameters
    params <- user_facet$params

    # we always fix the grid rows and cols
    params$rows <- default_facet$params$rows
    params$cols <- default_facet$params$cols
    params$drop <- default_facet$params$drop

    # if the default is free, it must be free
    params$free$x <- params$free$x || default_facet$params$free$x
    params$space_free$x <- params$space_free$x ||
        default_facet$params$space_free$x
    params$free$y <- params$free$y || default_facet$params$free$y
    params$space_free$y <- params$space_free$x ||
        default_facet$params$space_free$y
    ggproto(NULL, user_facet, params = params)
}

#' @importFrom stats reorder
quad_build_data <- function(data, row_params, column_params) {
    if (is.null(data) || (is.null(row_params) && is.null(column_params))) {
        return(data)
    }
    if (!is.null(row_params)) {
        row_panel <- .subset2(row_params, "panel")
        row_index <- .subset2(row_params, "index")
        row_coords <- data_frame0(
            .ypanel = row_panel[row_index],
            .yindex = row_index,
            .y = seq_along(row_index)
        )
    }
    if (!is.null(column_params)) {
        column_panel <- .subset2(column_params, "panel")
        column_index <- .subset2(column_params, "index")
        column_coords <- data_frame0(
            .xpanel = column_panel[column_index],
            .xindex = column_index,
            .x = seq_along(column_index)
        )
    }
    if (!is.null(row_params) && !is.null(column_params)) {
        coords <- vec_expand_grid(y = row_coords, x = column_coords)
        coords <- vec_cbind(coords$x, coords$y)
        by.x <- c(".column_index", ".row_index")
        by.y <- c(".xindex", ".yindex")
    } else if (is.null(row_params)) {
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
        !is.null(.subset2(ans, ".y"))) {
        ans$.row_names <- reorder(
            .subset2(ans, ".row_names"),
            .subset2(ans, ".y"),
            order = FALSE
        )
    }
    if (!is.null(.subset2(ans, ".column_names")) &&
        !is.null(.subset2(ans, ".x"))) {
        ans$.column_names <- reorder(
            .subset2(ans, ".column_names"),
            .subset2(ans, ".x"),
            order = FALSE
        )
    }
    restore_attr_ggalign(ans, data)
}
