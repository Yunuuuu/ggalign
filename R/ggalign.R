#' Add ggplot by Aligning discrete or continuous variable
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `ggalign()` is similar to `ggplot` in that it initializes a `ggplot` data and
#' `mapping`. `ggalign()` allowing you to provide data in various formats,
#' including matrices, data frames, or simple vectors. By default, it will
#' inherit from the layout. If a function, it will apply with the layout matrix.
#' `ggalign()` focuses on integrating plots into a layout by aligning the axes.
#'
#' @param data The following options can be used:
#'   - `NULL`: No data is set.
#'   - [`waiver()`][ggplot2::waiver]: Inherits the data from the layout matrix.
#'   - A `function` (including purrr-like lambda syntax): Applied to the layout
#'     matrix to transform the data before use. To transform the final plot
#'     data, please use [`scheme_data()`].
#'   - A `matrix`, `data.frame`, or atomic vector.
#' @inheritParams ggplot2::ggplot
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`fortify_data_frame()`].
#' @param size The relative size of the plot, can be specified as a
#' [`unit()`][grid::unit]. Note that for [`circle_layout()`], all size values
#' will be interpreted as relative sizes, as this layout type adjusts based on
#' the available space in the circular arrangement.
#' @param no_axes `r lifecycle::badge("deprecated")` Please add
#' [`theme()`][ggplot2::theme] directly to the ggplot instead.
#' @param active A [`active()`] object that defines the context settings when
#'   added to a layout.
#' @section ggplot2 specification:
#' `ggalign` initializes a ggplot object. The underlying data is created using
#' [`fortify_data_frame()`]. Please refer to it for more details.
#'
#' When aligning discrete variables, `ggalign()` always applies a default
#' mapping for the axis of the data index in the layout. Specifically:
#'
#'  - `aes(y = .data$.y)` is used for the horizontal `stack_layout()` (including
#'    left and right annotations).
#'  - `aes(x = .data$.x)` is used for the vertical `stack_layout()` (including
#'    top and bottom annotations) and `circle_layout()`.
#'
#' The following columns will be added to the data frame to align discrete
#' variables:
#'
#'  - `.panel`: The panel for the aligned axis. Refers to the `x-axis` for
#'    vertical `stack_layout()` (including top and bottom annotations), and the
#'    `y-axis` for horizontal `stack_layout()` (including left and right
#'    annotations).
#'
#'  - `.names` ([`vec_names()`][vctrs::vec_names]) and `.index`
#'    ([`vec_size()`][vctrs::vec_size()]/[`NROW()`]): Character names (if
#'    available) and the integer index of the original data.
#'
#'  - `.x`/`.y` and `.discrete_x`/`.discrete_y`: Integer indices for `x`/`y`
#'    coordinates, and a factor of the data labels (only applicable when names
#'    exist).
#'
#' It is recommended to use `.x`/`.y`, or `.discrete_x`/`.discrete_y` as the
#' `x`/`y` mapping.
#'
#' If the data inherits from [`quad_layout()`]/[`ggheatmap()`], additional
#' columns will be added:
#'
#'  - `.extra_panel`: Provides the panel information for the column (left or
#'    right annotation) or row (top or bottom annotation).
#'  - `.extra_index`: The index information for the column (left or right
#'    annotation) or row (top or bottom annotation).
#'
#' @inheritSection align Discrete Axis Alignment
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     ggalign() +
#'     geom_point(aes(y = value))
#'
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top(size = 0.5) +
#'     align_dendro(k = 3L) +
#'     ggalign(data = NULL, size = 0.2) +
#'     geom_tile(aes(y = 1L, fill = .panel))
#'
#' @importFrom ggplot2 ggplot aes
#' @importFrom rlang list2
#' @export
ggalign <- function(data = waiver(), mapping = aes(), ..., size = NULL,
                    active = NULL, no_axes = deprecated()) {
    if (inherits(data, "uneval")) {
        cli_abort(c(
            "{.arg data} cannot be {.obj_type_friendly {data}}",
            "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
        ))
    }
    if (lifecycle::is_present(no_axes)) {
        lifecycle::deprecate_stop(
            "1.1.0",
            "ggalign(no_axes = )",
            details = "Please add `theme()` to the ggplot instead"
        )
    }
    assert_active(active)
    active <- active(use = TRUE) + active
    new_craftbox(
        AlignGg,
        input_data = allow_lambda(data),
        data_params = list2(...),
        plot = ggplot(mapping = mapping),
        size = size,
        schemes = default_schemes(data, th = theme_no_strip()),
        active = active
    )
}

#' @importFrom rlang inject
#' @importFrom ggplot2 ggproto ggplot
AlignGg <- ggproto("AlignGg", Craftsman,
    interact_layout = function(self, layout) {
        layout_name <- self$layout_name
        input_data <- self$input_data
        object_name <- object_name(self)
        layout_data <- layout@data

        # inherit data from the layout
        if (is.function(input_data)) {
            if (is.null(layout_data)) {
                cli_abort(c(
                    sprintf(
                        "{.arg data} in %s cannot be a function",
                        object_name
                    ),
                    i = sprintf("no data was found in %s", layout_name)
                ))
            }
            data <- input_data(layout_data)
        } else if (is_waiver(input_data)) {
            data <- layout_data %|w|% NULL
            # for data inherit from the layout, and the layout data is from
            # the quad-layout, we'll integrate the `extra_domain`
            self$use_extra_domain <- is_stack_layout(layout) &&
                isTRUE(layout@heatmap$quad_matrix)
        } else {
            data <- input_data
        }
        plot_data <- inject(
            fortify_data_frame(data, !!!self$data_params, call = self$call)
        )

        # for discrete domain, # we need ensure the nobs is the same
        if (is_discrete_domain(domain <- layout@domain)) {
            if (!is.null(data)) {
                if (is.na(layout_nobs <- prop(domain, "nobs"))) {
                    layout_nobs <- NROW(data)
                    if (layout_nobs == 0L) {
                        cli_abort("{.arg data} cannot be empty",
                            call = self$call
                        )
                    }
                } else if (NROW(data) != layout_nobs) {
                    cli_abort(sprintf(
                        "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
                        object_name, NROW(data), layout_name, layout_nobs
                    ))
                }
                prop(domain, "nobs") <- layout_nobs

                # we always add `.index` to align the observations
                # For matrix-like object
                if (!is.data.frame(data) &&
                    vec_is(dim(data), integer(), size = 2L)) {
                    plot_data$.index <- vec_rep(seq_len(NROW(data)), NCOL(data))
                } else {
                    plot_data$.index <- seq_len(NROW(data))
                }
                layout@domain <- domain
                self$labels <- vec_names(data) %||% vec_names(layout_data)
                # always remove names, we'll add it in `build_plot()`
                plot_data$.names <- NULL
            } else {
                self$labels <- vec_names(layout_data)
            }
            self$add_mapping <- TRUE
        }
        self$data <- ggalign_data_restore(plot_data, layout_data)
        layout
    },
    init_plot = function(self, plot) {
        direction <- self$direction
        ggadd_default(
            plot,
            mapping = if (isTRUE(self$add_mapping)) {
                switch_direction(
                    direction,
                    aes(y = .data$.y),
                    aes(x = .data$.x)
                )
            },
            theme = if (is.null(self$data)) {
                # remove the title and text of axis vertically with the layout
                theme_no_axes(switch_direction(direction, "x", "y"))
            }
        ) + switch_direction(
            direction,
            ggplot2::labs(y = NULL),
            ggplot2::labs(x = NULL)
        )
    },

    #' @importFrom stats reorder
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        data <- self$data

        # if inherit from the parent layout
        if (isTRUE(self$use_extra_domain) &&
            is_discrete_domain(extra_domain) &&
            !is.na(prop(extra_domain, "nobs"))) {
            # if the data is inherit from the `quad_layout()`
            # the data must be a matrix
            extra_plot_data <- data_frame0(
                .extra_panel = prop(extra_domain, "panel"),
                .extra_index = prop(extra_domain, "index")
            )
        } else {
            extra_plot_data <- NULL
        }

        if (!is_discrete_domain(domain)) {
            if (is.data.frame(data) && is.data.frame(extra_plot_data)) {
                data <- full_join(data, extra_plot_data,
                    by.x = ".column_index", by.y = ".extra_index"
                )
            }
            # Treat the first column as the facet column
            # For continuous domains (used by circle_genomic()), ensure valid
            # facets
            if (is_continuous_domain(domain) &&
                !is.null(prop(domain, "facet_lvls")) &&
                is.data.frame(data) && ncol(data) > 0L) {
                missing <- is.na(data[[1L]])
                if (any(missing)) {
                    cli_warn("Removing {.val {sum(missing)}} rows with missing {.field facet_lvls}")
                    data <- vec_slice(data, !missing)
                }
                data[[1L]] <- factor(data[[1L]],
                    levels = prop(domain, "facet_lvls")
                )
            }
            return(gguse_data(plot, data))
        } else if (is.na(prop(domain, "nobs"))) {
            cli_abort(c(
                sprintf(
                    "you must initialize {.field nobs} of %s before drawing %s",
                    self$layout_name, object_name(self)
                ),
                i = "Or you should use {.fn ggfree}"
            ), call = self$call)
        }
        direction <- self$direction
        axis <- to_coord_axis(direction)
        panel <- prop(domain, "panel")
        index <- prop(domain, "index")
        coord_name <- paste0(".", axis)
        plot_data <- data_frame0(
            .panel = panel,
            .index = index,
            # `data_frame0` will omit `NULL`
            .names = .subset(self$labels, index)
        )
        plot_data[[coord_name]] <- seq_along(index)
        if (!is.null(.subset2(plot_data, ".names"))) {
            plot_data[[paste0(".discrete_", axis)]] <- reorder(
                .subset2(plot_data, ".names"),
                .subset2(plot_data, coord_name),
                order = FALSE
            )
        }

        if (!is.null(extra_plot_data)) {
            plot_data <- cross_join(plot_data, extra_plot_data)
            if (!is.null(data)) {
                plot_data <- full_join(data, plot_data,
                    by.x = c(".column_index", ".index"),
                    by.y = c(".extra_index", ".index")
                )
            }
        } else if (!is.null(data)) {
            plot_data <- full_join(data, plot_data, by = ".index")
        }
        gguse_data(plot, ggalign_data_restore(plot_data, data))
    },

    # let Craftsman to add schemes and theme acoordingly
    finish_plot = function(self, plot, schemes, theme) {
        direction <- self$direction
        # remove axis titles, text, ticks used for alignment
        schemes <- ggalign_update(
            schemes,
            theme_no_axes(switch_direction(direction, "y", "x"))
        )
        plot <- plot_add_scheme(plot, schemes)
        if (is_horizontal(direction)) {
            theme <- theme(
                panel.spacing.y = calc_element("panel.spacing.y", theme)
            )
        } else {
            theme <- theme(
                panel.spacing.x = calc_element("panel.spacing.x", theme)
            )
        }
        plot <- plot + theme
        ggremove_margin(plot, direction) + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(Craftsman, self)$summary(plot)
        c(header, "  Add plot by Aligning discrete or continuous variable")
    }
)

#' Create ggplot object with layout panel data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because we realised that it's a special case of
#' the [`ggalign()`] function. Please use `ggalign(data = NULL)` instead.
#' @export
#' @keywords internal
align_panel <- function(...) {
    lifecycle::deprecate_stop("0.0.5", "align_panel()", "ggalign(data = NULL)")
}

#' @export
#' @rdname align_panel
ggpanel <- function(...) {
    lifecycle::deprecate_stop("0.0.5", "ggpanel()", "ggalign(data = NULL)")
}
