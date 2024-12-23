#' Create ggplot object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `ggalign()` is similar to `ggplot` in that it initializes a `ggplot` data
#' and `mapping`. Same with other `align_*` functions. `ggalign()` allowing you
#' to provide data in various formats, including matrices, data frames, or
#' simple vectors. By default, it will inherit from the layout. If a function,
#' it will apply with the layout matrix.
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
#' [`unit`][grid::unit]. Note that for [`circle_layout()`], all size values will
#' be interpreted as relative sizes, as this layout type adjusts based on the
#' available space in the circular arrangement.
#' @param no_axes `r lifecycle::badge('experimental')` Logical; if `TRUE`,
#'   removes axes elements for the alignment axis using [`theme_no_axes()`]. By
#'   default, will use the option-
#'   `r code_quote(sprintf("%s.align_no_axes", pkg_nm()))`.
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
#'  - `.x`/`.y` and `.discrete_x`/`.discrete_y`: Integer indices for `x`/`y`
#'    coordinates, and a factor of the data labels (only applicable when names
#'    exist).
#'
#'  - `.names` ([`vec_names()`][vctrs::vec_names]) and `.index`
#'    ([`vec_size()`][vctrs::vec_size()]/[`NROW()`]): Character names (if
#'    available) and the integer index of the original data.
#'
#' If the data inherits from [`quad_layout()`]/[`ggheatmap()`], additional
#' columns will be added:
#'
#'  - `.extra_panel`: Provides the panel information for the column (left or
#'    right annotation) or row (top or bottom annotation).
#'  - `.extra_index`: The index information for the column (left or right
#'    annotation) or row (top or bottom annotation).
#'
#' @inheritSection align_discrete Discrete Axis Alignment
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     ggalign() +
#'     geom_point(aes(y = value))
#'
#' # if data is `NULL`, a data frame with following column will be created
#' # (`.panel`, `.index`, `.names`, `.x`/`.y`, `.discrete_x`/`.discrete_y`)
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
                    no_axes = NULL, active = NULL) {
    if (inherits(data, "uneval")) {
        cli_abort(c(
            "{.arg data} cannot be {.obj_type_friendly {data}}",
            "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
        ))
    }
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    new_ggalign_plot(
        AlignGg,
        input_data = data, params = list2(...),
        plot = ggplot(mapping = mapping),
        size = size,
        schemes = default_schemes(data, th = theme_no_panel()),
        no_axes = no_axes, active = active
    )
}

#' @importFrom rlang inject
#' @importFrom ggplot2 ggproto ggplot
AlignGg <- ggproto("AlignGg", AlignProto,
    setup_design = function(self, layout_data, design) {
        object_name <- self$object_name
        layout_name <- self$layout_name
        input_data <- self$input_data
        # inherit data from the layout
        if (is.function(input_data)) {
            if (is.null(layout_data)) {
                cli_abort(c(
                    "{.arg data} in {.var {object_name}} cannot be a function",
                    i = sprintf("no data was found in %s", layout_name)
                ))
            }
            data <- input_data(layout_data)
        } else if (is.waive(input_data)) {
            data <- layout_data %|w|% NULL
        } else {
            data <- input_data
        }
        ans <- inject(fortify_data_frame(data, !!!self$params))

        # for discrete design, # we need ensure the nobs is the same
        if (is_discrete_design(design)) {
            if (!is.null(data)) {
                if (is.null(layout_nobs <- design$nobs)) {
                    layout_nobs <- NROW(data)
                } else if (NROW(data) != layout_nobs) {
                    cli_abort(sprintf(
                        "{.var %s} (nobs: %d) is not compatible with the %s (nobs: %d)",
                        object_name, NROW(data), layout_name, layout_nobs
                    ))
                }
                design["nobs"] <- list(layout_nobs)
                # we always add `.index` to align the observations
                if (is.matrix(input_data)) {
                    ans$.index <- vec_rep(
                        seq_len(NROW(input_data)),
                        NCOL(input_data)
                    )
                } else {
                    ans$.index <- seq_len(NROW(data))
                }
            }
            self$labels <- vec_names(data) %||% vec_names(layout_data)
            self$add_mapping <- TRUE
            # always remove names, we'll add it in `build_plot()`
            ans$.names <- NULL
        }
        self$data <- ggalign_attr_restore(ans, layout_data)
        design
    },
    setup_plot = function(self, plot) {
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
                switch_direction(
                    direction,
                    theme(
                        axis.text.x = element_blank(),
                        axis.ticks.x = element_blank()
                    ),
                    theme(
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank()
                    )
                )
            }
        ) + switch_direction(
            direction,
            ggplot2::labs(y = NULL),
            ggplot2::labs(x = NULL)
        )
    },

    #' @importFrom stats reorder
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        data <- self$data
        if (is_continuous_design(design)) {
            return(gguse_data(plot, data))
        } else if (is.null(.subset2(design, "nobs"))) {
            cli_abort(
                c(
                    "you must provide {.arg data} to initialize the layout",
                    i = sprintf("no data was found in %s", self$layout_name),
                    i = "Or you should use {.fn ggfree}"
                ),
                call = self$call
            )
        }
        direction <- self$direction
        axis <- to_coord_axis(direction)
        panel <- .subset2(design, "panel")
        index <- .subset2(design, "index")
        if (is_continuous_design(extra_design)) {
            extra_panel <- NULL
            extra_index <- NULL
        } else {
            extra_panel <- .subset2(extra_design, "panel")
            extra_index <- .subset2(extra_design, "index")
        }
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

        # if inherit from the parent layout
        if (is.waive(.subset2(self, "input_data")) && !is.null(extra_panel)) {
            # if the data is inherit from the `quad_layout()`
            # the data must be a matrix
            plot_data <- cross_join(plot_data, data_frame0(
                .extra_panel = extra_panel, .extra_index = extra_index
            ))
            if (!is.null(data)) {
                plot_data <- full_join(data, plot_data,
                    by.x = c(".column_index", ".index"),
                    by.y = c(".extra_index", ".index")
                )
            }
        } else if (!is.null(data)) {
            plot_data <- full_join(data, plot_data, by = ".index")
        }
        gguse_data(plot, ggalign_attr_restore(plot_data, data))
    },

    # let AlignProto to add schemes and theme acoordingly
    finish_plot = function(self, plot, schemes, theme) {
        direction <- self$direction
        # remove axis titles, text, ticks used for alignment
        if (isTRUE(self$no_axes)) {
            schemes$scheme_theme <- .subset2(schemes, "scheme_theme") +
                theme_no_axes(switch_direction(direction, "y", "x"))
        }
        plot <- plot_add_schemes(plot, schemes)
        if (is_horizontal(direction)) {
            theme <- theme(
                panel.spacing.y = calc_element("panel.spacing.y", theme)
            )
        } else {
            theme <- theme(
                panel.spacing.x = calc_element("panel.spacing.x", theme)
            )
        }
        plot + theme + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(AlignProto, self)$summary(plot)
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
