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
#' @param data A flexible input that specifies the data to be used
#'   - `NULL`: No data is set.
#'   - [`waiver()`][ggplot2::waiver]: Try to use the layout matrix.
#'   - A `function` (including purrr-like lambda syntax) that is applied to the
#'     layout matrix. If you want to transform the final plot data, please use
#'     [`scheme_data()`].
#'   - A `matrix`, `data frame`, or atomic vector.
#' @inheritParams ggplot2::ggplot
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`fortify_data_frame()`].
#' @param size The relative size of the plot, can be specified as a
#' [`unit`][grid::unit].
#' @param no_axes `r lifecycle::badge('experimental')` Logical; if `TRUE`,
#'   removes axes elements for the alignment axis using [`theme_no_axes()`]. By
#'   default, will use the option-
#'   `r code_quote(sprintf("%s.align_no_axes", pkg_nm()))`.
#' @param active A [`active()`] object that defines the context settings when
#'   added to a layout.
#' @section ggplot2 specification:
#' `ggalign` initializes a ggplot `data` and `mapping`.
#'
#' `ggalign()` always applies a default mapping for the axis of the data index
#' in the layout. This mapping is `aes(y = .data$.y)` for horizontal stack
#' layout (including left and right annotation) and `aes(x = .data$.x)`
#' for vertical stack layout (including top and bottom annotation).
#'
#' The data in the underlying `ggplot` object will contain following columns:
#'
#'  - `.panel`: the panel for the aligned axis. It means `x-axis` for vertical
#'    stack layout (including top and bottom annotation), `y-axis` for
#'    horizontal stack layout (including left and right annotation).
#'
#'  - `.x`/`y` and `.discrete_x`/`.discrete_y`: an integer index of `x`/`y`
#'    coordinates and a factor of the data labels (only applicable when names
#'    exists).
#'
#'  - `.names` ([`vec_names()`][vctrs::vec_names]) and `.index`
#'    ([`vec_size()`][vctrs::vec_size()]/[`NROW()`]): a character names (only
#'    applicable when names exists) and an integer index of the original data.
#'
#'  - `.row_names` and `.row_index`: the row names and an integer of
#'    row index of the original matrix (only applicable if `data` is a
#'    `matrix`).
#'
#'  - `.column_names` and `.column_index`: the column names and column index of
#'    the original matrix (only applicable if `data` is a `matrix`).
#'
#'  - `value`: the actual value (only applicable if `data` is a `matrix` or
#'    atomic vector).
#'
#' `matrix` input will be automatically melted into a long foramted data frame.
#'
#' Atomic vector will be put in the `value` column of the data frame.
#'
#' In the case where the input data is already a data frame, following columns
#' (`.panel`, `.index`, `.names`, `.x`/`.y`, `.discrete_x`/`.discrete_y`) are
#' added to the data frame.
#'
#' It is recommended to use `.x`/`.y`, or `.discrete_x`/`.discrete_y` as the
#' `x`/`y` mapping.
#'
#' If the data inherits from [`quad_layout()`]/[`ggheatmap()`], an additional
#' column will be added.
#'
#'  - `.extra_panel`: the panel information for column (left or right
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

#' @export
summary.AlignGg <- function(object, ...) c(FALSE, FALSE)

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
            data <- layout_data
        } else {
            data <- input_data
        }
        ans <- fortify_data_frame(data)

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
                self$labels <- vec_names(data) %||% vec_names(layout_data)
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
    build_plot = function(self, plot, design, extra_design, previous_design) {
        direction <- .subset2(self, "direction")
        axis <- to_coord_axis(direction)
        panel <- .subset2(design, "panel")
        index <- .subset2(design, "index")

        # use linear coordinate
        plot <- gguse_linear_coord(plot, self$layout_name)

        # set limits and default scales
        if (is_horizontal(direction)) {
            plot <- plot + ggalign_design(
                y = design,
                ylabels = .subset(self$labels, index)
            )
        } else {
            plot <- plot + ggalign_design(
                x = design,
                xlabels = .subset(self$labels, index)
            )
        }

        data <- self$data
        if (is_continuous_design(design)) {
            return(gguse_data(plot, data))
        }

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

        # we'll setup the facets for the discrete design
        if (nlevels(panel) > 1L) {
            default_facet <- switch_direction(
                direction,
                ggplot2::facet_grid(
                    rows = ggplot2::vars(.data$.panel),
                    scales = "free_y", space = "free",
                    drop = FALSE, as.table = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free_x", space = "free",
                    drop = FALSE, as.table = FALSE
                )
            )
        } else {
            default_facet <- facet_stack(direction = direction)
        }
        plot <- gguse_data(plot, ggalign_attr_restore(plot_data, data))
        gguse_facet(plot, default_facet)
    },

    # let AlignProto to add schemes and theme acoordingly
    finish_plot = function(self, plot, schemes, theme) {
        ggproto_parent(AlignDiscrete, self)$finish_plot(plot, schemes, theme)
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
