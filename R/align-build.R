#' @importFrom ggplot2 theme element_blank
#' @importFrom rlang inject
align_build <- function(align, panel, index, controls, extra_layout) {
    # let `Align` to determine how to draw
    # 1. add default layer
    # 2. add plot data
    object <- .subset2(align, "Object") # `Align` object

    # we lock the Align object to prevent user from modifying this object
    # in `$draw` method, we shouldn't do any calculations in `$draw` method
    object$lock()
    on.exit(object$unlock())

    direction <- .subset2(object, "direction")
    params <- .subset2(object, "params")
    draw_params <- params[
        vec_set_intersect(
            names(params),
            align_method_params(
                object$draw,
                c("plot", "panel", "index", "extra_panel", "extra_index")
            )
        )
    ]
    if (is.null(extra_layout)) {
        extra_panel <- NULL
        extra_index <- NULL
    } else {
        extra_panel <- .subset2(extra_layout, "panel")
        extra_index <- .subset2(extra_layout, "index")
    }
    plot <- inject(object$draw(
        .subset2(align, "plot"),
        panel, index, extra_panel,
        extra_index, !!!draw_params
    ))

    # only when user use the internal facet, we'll setup the limits
    if (.subset2(align, "facet")) {
        # set up facets
        if (nlevels(panel) > 1L) {
            default_facet <- switch_direction(
                direction,
                ggplot2::facet_grid(
                    rows = ggplot2::vars(fct_rev(.data$.panel)),
                    scales = "free_y", space = "free",
                    drop = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free_x", space = "free",
                    drop = FALSE
                )
            )
        } else {
            default_facet <- ggplot2::facet_null()
        }
        layout <- list(
            panel = panel,
            index = index,
            labels = .subset(.subset2(object, "labels"), index),
            limits = .subset2(align, "limits")
        )
        plot <- plot + align_melt_facet(plot$facet, default_facet, direction) +
            switch_direction(
                direction,
                coord_ggalign(y = layout),
                coord_ggalign(x = layout)
            )
    }
    # remove axis titles, text, ticks used for alignment
    if (isTRUE(.subset2(align, "no_axes"))) {
        controls$plot_theme <- .subset2(controls, "plot_theme") +
            theme_no_axes(switch_direction(direction, "y", "x"))
    }
    plot <- plot_add_controls(plot, controls)
    list(plot = plot, size = .subset2(align, "size"))
}

#' @importFrom ggplot2 ggproto
align_melt_facet <- function(user_facet, default_facet, direction) {
    if (inherits(user_facet, "FacetGrid")) {
        # re-dispatch parameters
        params <- user_facet$params

        if (inherits(default_facet, "FacetGrid")) {
            # we always fix the grid rows and cols
            params$rows <- default_facet$params$rows %||% params$rows
            params$cols <- default_facet$params$cols %||% params$cols
            params$drop <- default_facet$params$drop

            # if the default is free, it must be free
            params$free$x <- params$free$x || default_facet$params$free$x
            params$space_free$x <- params$space_free$x ||
                default_facet$params$space_free$x
            params$free$y <- params$free$y || default_facet$params$free$y
            params$space_free$y <- params$space_free$x ||
                default_facet$params$space_free$y
        } else if (is_horizontal(direction)) {
            # for horizontal stack, we cannot facet by rows
            if (!is.null(params$rows)) {
                cli_warn(
                    "Canno facet by rows in {.field {direction}} stack"
                )
                params$rows <- NULL
            }
        } else if (!is.null(params$cols)) {
            # for vertical stack, we cannot facet by cols
            cli_warn("Canno facet by cols in {.field {direction}} stack")
            params$cols <- NULL
        }
        ggproto(NULL, user_facet, params = params)
    } else if (inherits(user_facet, "FacetNull")) {
        if (inherits(default_facet, "FacetNull")) { # no facet
            user_facet
        } else { # we have facet
            default_facet
        }
    } else {
        default_facet
    }
}

remove_scales <- function(plot, scale_aesthetics) {
    scales <- .subset2(plot, "scales")$clone()
    if (any(prev_aes <- scales$find(scale_aesthetics))) {
        scales$scales <- scales$scales[!prev_aes]
    }
    plot$scales <- scales
    plot
}

#' @importFrom rlang is_empty
extract_scales <- function(plot, axis, n_panel, facet_scales) {
    # if no facets, or if no facet scales, we replicate the single scale
    # object to match the panel numbers
    if (n_panel > 1L &&
        !is.null(facet_scales) &&
        !is_empty(ans <- .subset2(facet_scales, axis))) {
    } else {
        ans <- rep_len(list(plot$scales$get_scales(axis)), n_panel)
    }
    ans
}
