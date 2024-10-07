#' @importFrom grid unit.c
#' @export
ggalign_build.HeatmapLayout <- function(x) {
    layout_theme <- x@theme %||% default_theme()
    patches <- heatmap_build(x, layout_theme = layout_theme)
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
    annotation <- x@annotation
    align_plots(
        !!!.subset(plots, keep),
        design = design,
        heights = .subset2(sizes, "height"),
        widths = .subset2(sizes, "width"),
        # No parent layout, by default we'll always collect guides
        guides = .subset2(x@params, "guides") %|w|% "tlbr",
        title = .subset2(annotation, "title"),
        subtitle = .subset2(annotation, "subtitle"),
        caption = .subset2(annotation, "caption"),
        theme = layout_theme
    )
}

#' @importFrom ggplot2 aes
#' @importFrom rlang is_empty
#' @importFrom grid unit is.unit unit.c
heatmap_build <- function(heatmap, plot_data = waiver(),
                          free_labs = waiver(), free_spaces = waiver(),
                          theme = waiver(), layout_theme = waiver()) {
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
    p <- finish_plot_data(p, plot_data %|w|% NULL, data = data)

    # setup plot theme ----------------------------------
    p$theme <- theme + p$theme

    # setup the facet -----------------------------------
    do_row_facet <- nlevels(ypanel) > 1L
    do_column_facet <- nlevels(xpanel) > 1L

    x_layout <- list(
        panel = xpanel,
        index = xindex,
        labels = colnames(mat)
    )
    y_layout <- list(
        panel = ypanel,
        index = yindex,
        labels = rownames(mat)
    )

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
    xlim_list <- set_limits("x", x_layout)
    ylim_list <- set_limits("y", y_layout)
    p <- p + heatmap_melt_facet(p$facet, default_facet) +
        facet_ggalign(x = x_layout, y = y_layout) +
        coord_ggalign(
            xlim_list = rep(xlim_list, times = nlevels(ypanel)),
            ylim_list = rep(ylim_list, each = nlevels(xpanel))
        )

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
            free_labs = free_labs,
            free_spaces = free_spaces,
            theme = theme,
            layout_theme = layout_theme,
            extra_panel = panel,
            extra_index = index
        )
        if (!is.null(.subset2(ans, "plot"))) {
            # for annotation stack, we handle the free_guides
            free_guides <- .subset2(stack@params, "free_guides")
            if (!is.waive(free_guides)) {
                ans$plot <- free_guide(.subset2(ans, "plot"), free_guides)
            }
            # for heatmap annotation, we should always make them next to
            # the heatmap body
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
    p <- add_class(p, "ggalign_heatmap")
    if (!is.waive(free_guides <- .subset2(params, "free_guides"))) {
        p <- free_guide(p, free_guides)
    }
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

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggalign_heatmap <- function(plot) {
    old_discrete_fill <- getOption("ggplot2.discrete.fill")
    old_continuous_fill <- getOption("ggplot2.continuous.fill")
    on.exit({
        options(ggplot2.discrete.fill = old_discrete_fill)
        options(ggplot2.continuous.fill = old_continuous_fill)
    })
    # change the default fill scale --------------------------
    options(ggplot2.discrete.fill = heatmap_fill("discrete"))
    options(ggplot2.continuous.fill = heatmap_fill("continuous"))
    NextMethod()
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

#' @importFrom data.table data.table setDF merge.data.table
#' @importFrom stats reorder
heatmap_build_data <- function(matrix, row_panel, row_index,
                               column_panel, column_index) {
    ycoords <- data.table(
        .ypanel = row_panel[row_index],
        .yindex = row_index,
        .y = seq_along(row_index),
        k = 1L
    )
    xcoords <- data.table(
        .xpanel = column_panel[column_index],
        .xindex = column_index,
        .x = seq_along(column_index),
        k = 1L
    )
    coords <- xcoords[ycoords, on = "k", allow.cartesian = TRUE]
    coords[, "k" := NULL]
    ans <- melt_matrix(matrix)
    ans <- merge(ans, coords,
        by.x = c(".column_index", ".row_index"),
        by.y = c(".xindex", ".yindex"),
        sort = FALSE, all = TRUE
    )
    setDF(ans)
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
