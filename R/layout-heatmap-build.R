#' @importFrom grid unit.c
#' @export
ggalign_build.HeatmapLayout <- function(x) {
    x <- layout_default(x)
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
        out <- .subset(
            out,
            !vapply(.subset(plots, x), is.null, logical(1L), USE.NAMES = FALSE)
        )
        do.call(unit.c, out)
    })
    keep <- !vapply(plots, is.null, logical(1L), USE.NAMES = FALSE)

    design <- trim_area(do.call(c, design[keep]))
    titles <- x@titles
    align_plots(
        !!!.subset(plots, keep),
        design = design,
        heights = .subset2(sizes, "height"),
        widths = .subset2(sizes, "width"),
        guides = .subset2(x@action, "guides"),
        theme = x@theme
    ) + layout_title(
        title = .subset2(titles, "title"),
        subtitle = .subset2(titles, "subtitle"),
        caption = .subset2(titles, "caption")
    )
}

#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom grid unit is.unit unit.c
heatmap_build <- function(heatmap, action = heatmap@action) {
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

    # setup the facet -----------------------------------
    do_row_facet <- nlevels(ypanel) > 1L
    do_column_facet <- nlevels(xpanel) > 1L

    # then we add facet -----------------------------------
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

    x_params <- list(panel = xpanel, index = xindex, labels = colnames(mat))
    y_params <- list(panel = ypanel, index = yindex, labels = rownames(mat))
    xlim_list <- set_limits("x", x_params)
    ylim_list <- set_limits("y", y_params)

    # prepare action for vertical and horizontal stack layout
    vertical_action <- horizontal_action <- action
    if (!is.null(layout_labs <- .subset2(action, "free_labs")) &&
        !is.waive(layout_labs)) {
        # prepare labs for child stack layout
        horizontal_action$free_labs <- gsub("[lr]", "", layout_labs)
        vertical_action$free_labs <- gsub("[tb]", "", layout_labs)
        if (nchar(horizontal_action$free_labs) == 0L) {
            horizontal_action["free_labs"] <- list(NULL)
        }
        if (nchar(vertical_action$free_labs) == 0L) {
            vertical_action["free_labs"] <- list(NULL)
        }
    }

    # inherit from the parent stack layout
    if (!is.null(layout_spaces <- .subset2(action, "free_spaces")) &&
        !is.waive(layout_spaces)) {
        horizontal_action$free_spaces <- gsub("[lr]", "", layout_spaces)
        vertical_action$free_spaces <- gsub("[tb]", "", layout_spaces)
        if (nchar(horizontal_action$free_spaces) == 0L) {
            horizontal_action["free_spaces"] <- list(NULL)
        }
        if (nchar(vertical_action$free_spaces) == 0L) {
            vertical_action["free_spaces"] <- list(NULL)
        }
    }

    # plot heatmap annotations ----------------------------
    stack_list <- lapply(.TLBR, function(position) {
        if (is_empty(stack <- slot(heatmap, position))) {
            return(list(plot = NULL, size = NULL))
        }
        if (is_horizontal(to_direction(position))) {
            panel <- xpanel
            index <- xindex
            stack_action <- inherit_action(stack@action, horizontal_action)
        } else {
            panel <- ypanel
            index <- yindex
            stack_action <- inherit_action(stack@action, vertical_action)
        }
        plot <- stack_build(
            stack,
            action = stack_action,
            extra_panel = panel,
            extra_index = index
        )
        if (!is.null(plot)) {
            # for heatmap annotation, we should always make them next to
            # the heatmap body
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
            free_spaces <- .subset2(stack_action, "free_spaces") %|w|% NULL
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
    p <- heatmap@plot

    # add heatmap filling in the first layer --------------
    if (!is.null(filling <- heatmap@filling)) {
        # we always ensure the filling layer has a fill mapping
        if (is.null(.subset2(p$mapping, "fill"))) {
            mapping <- aes(.data$.x, .data$.y, fill = .data$value)
        } else {
            mapping <- aes(.data$.x, .data$.y)
        }
        if (is.waive(filling)) {
            if (get_nobs(heatmap, "x") * get_nobs(heatmap, "y") > 20000L) {
                cli::cli_inform(c(">" = "heatmap built with {.fn geom_raster}"))
                filling <- "raster"
            } else {
                cli::cli_inform(c(">" = "heatmap built with {.fn geom_tile}"))
                filling <- "tile"
            }
        }
        p <- p + layer_order(switch(filling,
            raster = ggplot2::geom_raster(mapping = mapping),
            tile = ggplot2::geom_tile(mapping = mapping)
        ))
    }

    # set the default data -------------------------------
    p$data <- heatmap_build_data(mat, ypanel, yindex, xpanel, xindex)

    # set the facets and coord ---------------------------
    p <- p + heatmap_melt_facet(p$facet, default_facet) +
        facet_ggalign(x = x_params, y = y_params) +
        coord_ggalign(
            xlim_list = rep(xlim_list, times = nlevels(ypanel)),
            ylim_list = rep(ylim_list, each = nlevels(xpanel))
        )

    # add action -----------------------------------------
    p <- plot_add_action(p, inherit_action(heatmap@body_action, action))

    # add class to set the default color mapping
    p <- add_class(p, "ggalign_heatmap")
    plots <- c(plots, list(heatmap = p))
    sizes <- c(sizes, list(
        heatmap = list(width = heatmap@width, height = heatmap@height)
    ))
    list(plots = plots, sizes = sizes)
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggalign_heatmap <- function(plot) {
    with_options(
        NextMethod(),
        ggplot2.discrete.fill = heatmap_fill("discrete"),
        ggplot2.continuous.fill = heatmap_fill("continuous")
    )
}

heatmap_fill <- function(type) {
    type <- match.arg(type, c("discrete", "continuous"))
    opt <- sprintf("%s.heatmap_%s_fill", pkg_nm(), type)
    if (is.null(ans <- getOption(opt, default = NULL))) {
        if (type == "continuous") {
            ans <- function(...) {
                ggplot2::scale_fill_gradient2(low = "blue", high = "red")
            }
        } else {
            ans <- getOption("ggplot2.discrete.fill")
        }
    } else if (inherits(ans, "Scale")) {
        ans <- rlang::new_function(rlang::exprs(... = ), ans)
    }
    ans
}

#' @importFrom ggplot2 ggproto
heatmap_melt_facet <- function(user_facet, default_facet) {
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

#' @importFrom vctrs vec_expand_grid vec_cbind
#' @importFrom stats reorder
heatmap_build_data <- function(matrix, row_panel, row_index,
                               column_panel, column_index) {
    coords <- vec_expand_grid(
        y = data_frame0(
            .ypanel = row_panel[row_index],
            .yindex = row_index,
            .y = seq_along(row_index)
        ),
        x = data_frame0(
            .xpanel = column_panel[column_index],
            .xindex = column_index,
            .x = seq_along(column_index)
        )
    )
    coords <- vec_cbind(coords$x, coords$y)
    ans <- melt_matrix(matrix)
    ans <- full_join(ans, coords,
        by.x = c(".column_index", ".row_index"),
        by.y = c(".xindex", ".yindex")
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
    restore_attr_ggalign(ans, matrix)
}
