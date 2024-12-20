#' Create ggplot object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `align_gg()` is similar to `ggplot` in that it initializes a `ggplot` data
#' and `mapping`. Same with other `align_*` functions. `align_gg()` allowing you
#' to provide data in various formats, including matrices, data frames, or
#' simple vectors. By default, it will inherit from the layout. If a function,
#' it will apply with the layout matrix.  `ggalign` is an alias of `align_gg`.
#'
#' @inheritParams ggplot2::ggplot
#' @param data A flexible input that specifies the data to be used
#' - `NULL`: No data is set.
#' - [`waiver()`][ggplot2::waiver]: Uses the layout matrix.
#' - A `function` (including purrr-like lambda syntax) that is applied to the
#'   layout matrix, and must return a matrix. If you want to transform the final
#'   plot data, please use [`scheme_data()`].
#' - A `matrix`, `data frame`, or atomic vector.
#' @inheritParams align
#' @param set_context `r lifecycle::badge("deprecated")` Please use `active`
#' argument instead.
#' @param order `r lifecycle::badge("deprecated")` Please use `active` argument
#' instead.
#' @param name `r lifecycle::badge("deprecated")` Please use `active` argument
#' instead.
#' @importFrom ggplot2 aes
#'
#' @section ggplot2 specification:
#' `align_gg` initializes a ggplot `data` and `mapping`.
#'
#' `align_gg()` always applies a default mapping for the axis of the data index
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
#' @inheritSection align Axis Alignment for Observations
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
#' @importFrom ggplot2 ggplot
#' @export
align_gg <- function(data = waiver(), mapping = aes(), size = NULL,
                     limits = TRUE, facet = TRUE,
                     no_axes = NULL, active = NULL, set_context = deprecated(),
                     order = deprecated(), name = deprecated(),
                     free_guides = deprecated(), free_spaces = deprecated(),
                     plot_data = deprecated(), theme = deprecated(),
                     free_labs = deprecated()) {
    if (inherits(data, "uneval")) {
        cli_abort(c(
            "{.arg data} cannot be {.obj_type_friendly {data}}",
            "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
        ))
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    active <- deprecate_active(active, "align_gg",
        set_context = set_context, order = order, name = name
    )
    align(AlignGg,
        plot = ggplot(mapping = mapping),
        size = size, data = data,
        schemes = default_schemes(data, th = theme_no_panel()),
        free_guides = free_guides,
        free_labs = free_labs, free_spaces = free_spaces,
        plot_data = plot_data, theme = theme,
        facet = facet, limits = limits, no_axes = no_axes, active = active
    )
}

#' @usage NULL
#' @export
#' @rdname align_gg
ggalign <- align_gg

#' @export
summary.AlignGg <- function(object, ...) c(FALSE, FALSE)

#' @importFrom ggplot2 ggproto ggplot
AlignGg <- ggproto("AlignGg", Align,
    nobs = function(self) { # no input data
        axis <- to_coord_axis(.subset2(self, "direction"))
        cli_abort(c(
            sprintf("You cannot add %s", object_name(self)),
            i = "layout {axis}-axis is not initialized or you must provide {.arg data}"
        ), call = .subset2(self, "call"))
    },
    setup_data = function(self, params, data) {
        ans <- fortify_data_frame(data)
        # we always add `.index` to align the observations
        if (is.matrix(data)) {
            ans$.index <- vec_rep(seq_len(NROW(data)), NCOL(data))
        } else {
            ans$.index <- seq_len(NROW(data))
        }
        ans$.names <- NULL # always remove names, we'll add it in `draw()`
        ans
    },
    setup_plot = function(self, plot) {
        direction <- self$direction
        ggadd_default(
            plot,
            mapping = switch_direction(
                direction,
                aes(y = .data$.y),
                aes(x = .data$.x)
            ),
            theme = if (is.null(.subset2(self, "input_data"))) {
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
    draw = function(self, plot, panel, index, extra_panel, extra_index) {
        data <- .subset2(self, "data")
        direction <- .subset2(self, "direction")
        axis <- to_coord_axis(direction)
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
        plot$data <- ggalign_attr_restore(plot_data, data)
        plot
    }
)

#' Create ggplot object with layout panel data
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function was deprecated because we realised that it's a special case of
#' the [`ggalian()`][align_gg] function. Please use `ggalign(data = NULL)`
#' instead.
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
