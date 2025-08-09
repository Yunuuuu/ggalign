#' @importFrom grid unit.c
#' @export
`ggalign_build.ggalign::QuadLayout` <- function(x) {
    x <- layout_init(x)
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
        guides = prop(schemes_get(x@schemes, "scheme_align"), "guides"),
        theme = x@theme
    ) + layout_title(
        title = .subset2(titles, "title"),
        subtitle = .subset2(titles, "subtitle"),
        caption = .subset2(titles, "caption")
    ) + layout_tags(NULL)
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
#' @importFrom S7 convert
#' @export
#' @noRd
`quad_build.ggalign::QuadLayout` <- function(quad, schemes = NULL, theme = NULL,
                                             direction = NULL) {
    data <- quad@data
    row_domain <- domain_init(quad@horizontal)
    column_domain <- domain_init(quad@vertical)
    if (is.function(data)) {
        cli_abort(c(
            "{.arg data} cannot be a {.cls function}",
            i = sprintf(
                "Did you want to add %s to a {.fn stack_layout}?",
                object_name(quad)
            )
        ))
    }
    if (is_discrete_domain(row_domain) &&
        is.na(prop(row_domain, "nobs"))) {
        cli_abort(sprintf(
            "you must initialize %s before drawing the main plot",
            object_name(quad),
        ))
    }
    if (is_discrete_domain(column_domain) &&
        is.na(prop(column_domain, "nobs"))) {
        cli_abort(sprintf(
            "you must initialize %s before drawing the main plot",
            object_name(quad),
        ))
    }
    schemes <- inherit_parent_layout_schemes(quad, schemes)
    if (is.null(direction)) {
        spacing <- NULL
    } else if (is_horizontal(direction)) {
        spacing <- "y"
    } else {
        spacing <- "x"
    }
    theme <- inherit_parent_layout_theme(quad, theme, spacing = spacing)

    # prepare action for vertical and horizontal stack layout
    vertical_align <- horizontal_align <- the_align <-
        schemes_get(schemes, "scheme_align")
    if (!is.null(layout_labs <- prop(the_align, "free_labs")) &&
        !is.waive(layout_labs)) {
        # prepare labs for child stack layout
        prop(horizontal_align, "free_labs", check = FALSE) <-
            gsub("[lr]", "", layout_labs)
        prop(vertical_align, "free_labs", check = FALSE) <-
            gsub("[tb]", "", layout_labs)
        if (!nzchar(prop(horizontal_align, "free_labs"))) {
            prop(horizontal_align, "free_labs", check = FALSE) <- NULL
        }
        if (!nzchar(prop(vertical_align, "free_labs"))) {
            prop(vertical_align, "free_labs", check = FALSE) <- NULL
        }
    }

    # inherit from the parent stack layout
    if (!is.null(layout_spaces <- prop(the_align, "free_spaces")) &&
        !is.waive(layout_spaces)) {
        prop(horizontal_align, "free_spaces", check = FALSE) <-
            gsub("[lr]", "", layout_spaces)
        prop(vertical_align, "free_spaces", check = FALSE) <-
            gsub("[tb]", "", layout_spaces)
        if (!nzchar(prop(horizontal_align, "free_spaces"))) {
            prop(horizontal_align, "free_spaces", check = FALSE) <- NULL
        }
        if (!nzchar(prop(vertical_align, "free_spaces"))) {
            prop(vertical_align, "free_spaces", check = FALSE) <- NULL
        }
    }

    # plot annotations ----------------------------
    stack_list <- lapply(.TLBR, function(position) {
        if (is_empty(stack <- prop(quad, position))) {
            return(list(plot = NULL, size = NULL))
        }
        pschemes <- schemes
        # inherit from horizontal align or vertical align
        if (is_horizontal(to_direction(position))) {
            extra_domain <- column_domain
            pschemes <- schemes_set(pschemes, horizontal_align, check = FALSE)
        } else {
            extra_domain <- row_domain
            pschemes <- schemes_set(pschemes, vertical_align, check = FALSE)
        }
        plot <- stack_build(
            stack,
            schemes = pschemes,
            theme = theme,
            extra_domain = extra_domain
        )
        if (is.null(plot)) {
            size <- NULL
        } else {
            size <- convert(stack@sizes, S3_unit)
        }
        list(plot = plot, size = size)
    })
    names(stack_list) <- .TLBR
    stack_list <- list_transpose(stack_list)
    plots <- .subset2(stack_list, 1L) # the annotation plot itself
    sizes <- .subset2(stack_list, 2L) # annotation size

    # read the plot ---------------------------------------
    p <- quad@plot

    # set the facets and coord ---------------------------
    p <- align_quad_facet(p, row_domain, column_domain, object_name(quad))
    p <- gguse_linear_coord(p, ggplot2::coord_cartesian(), object_name(quad))

    p <- p +
        layout_align(
            x = column_domain, y = row_domain,
            xlabels = colnames(data), ylabels = vec_names(data)
        )

    # add default data ----------------------------------
    p <- gguse_data(p, quad_build_data(data, row_domain, column_domain))

    # add action ----------------------------------------
    p <- plot_add_scheme(p, scheme_inherit(schemes, quad@body_schemes))
    p <- p + theme(
        panel.spacing.x = calc_element("panel.spacing.x", theme),
        panel.spacing.y = calc_element("panel.spacing.y", theme)
    )
    p <- p + theme_recycle()

    # collect all plots and sizes ----------------------
    plots <- append(plots, list(main = p), 2L)
    sizes <- append(
        sizes,
        list(main = list(
            width = convert(quad@width, S3_unit),
            height = convert(quad@height, S3_unit)
        )),
        3L
    )
    list(plots = plots, sizes = sizes)
}

#' @importFrom stats reorder
quad_build_data <- function(data, row_domain, column_domain) {
    if (is.null(data) ||
        (!is_discrete_domain(row_domain) &&
            !is_discrete_domain(column_domain))) {
        return(data)
    }
    if (is_discrete_domain(row_domain)) {
        row_panel <- prop(row_domain, "panel")
        row_index <- prop(row_domain, "index")
        row_data <- data_frame0(
            .panel_y = row_panel,
            .index_y = row_index,
            .y = seq_along(row_index)
        )
    }
    if (is_discrete_domain(column_domain)) {
        column_panel <- prop(column_domain, "panel")
        column_index <- prop(column_domain, "index")
        column_data <- data_frame0(
            .panel_x = column_panel,
            .index_x = column_index,
            .x = seq_along(column_index)
        )
    }
    if (is_discrete_domain(row_domain) && is_discrete_domain(column_domain)) {
        panel_data <- cross_join(row_data, column_data)
        by.x <- c(".column_index", ".row_index")
        by.y <- c(".index_x", ".index_y")
    } else if (is_discrete_domain(column_domain)) {
        panel_data <- column_data
        by.x <- ".column_index"
        by.y <- ".index_x"
    } else {
        panel_data <- row_data
        by.x <- ".row_index"
        by.y <- ".index_y"
    }
    ans <- fortify_data_frame.matrix(data)
    ans <- full_join(ans, panel_data, by.x = by.x, by.y = by.y)
    if (!is.null(.subset2(ans, ".row_names")) &&
        is_discrete_domain(row_domain)) {
        ans$.discrete_y <- reorder(
            .subset2(ans, ".row_names"),
            .subset2(ans, ".y"),
            order = FALSE
        )
    }
    if (!is.null(.subset2(ans, ".column_names")) &&
        is_discrete_domain(column_domain)) {
        ans$.discrete_x <- reorder(
            .subset2(ans, ".column_names"),
            .subset2(ans, ".x"),
            order = FALSE
        )
    }
    ggalign_data_restore(ans, data)
}
