#' Create ggplot object
#'
#' `ggalign` is an alias of `align_gg`.
#'
#' @param data A flexible input that specifies the data to be used
#' - `NULL`: No data is set.
#' - [`waiver()`]: Uses the layout matrix.
#' - A `function` (including purrr-like lambda syntax) that is applied to the
#'   layout matrix, and must return a matrix. If you want to transform the final
#'   plot data, please use `action` argument.
#' - A `matrix`, `data frame`, or atomic vector. If an atomic vector is
#'   provided, it is converted into a single-column data frame.
#' @inheritParams align_dendro
#' @inheritParams align
#' @inheritParams heatmap_layout
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
#'  - `.panel`: the panel for current aligned axis. It means `x-axis` for
#'    vertical stack layout, `y-axis` for horizontal stack layout.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates
#'
#'  - `.names` and `.index`: A factor of the names and an integer of index of
#'    the original data. ([`vec_names()`][vctrs::vec_names] and
#'    [`vec_size()`][vctrs::vec_size()])
#'
#'  - `.row_names` and `.row_index`: the row names and an integer of
#'    row index of the original matrix (only applicable if `data` is a
#'    `matrix`)..
#'
#'  - `.column_names` and `.column_index`: the column names and column index of
#'    the original matrix (only applicable if `data` is a `matrix`).
#'
#'  - `value`: the actual value (only applicable if `data` is a `matrix` or
#'    atomic vector).
#'
#' In the case where the input data is already a data frame, three additional
#' columns (`.names`, `.index`, and `.panel`) are added to the data frame.
#'
#' If the data inherits from [`quad_layout()`]/[`ggheatmap()`], an additional
#' column will be added.
#'
#'  - `.extra_panel`: the panel information for column (left or right
#'    annotation) or row (top or bottom annotation).
#'
#' @return A `"AlignGG"` object.
#' @inheritSection align Aligned Axis
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     ggalign() +
#'     geom_point(aes(y = value))
#'
#' # if data is `NULL`, a three column data frame
#' # will be created (`.panel`, `.index`, `.x`/`.y`)
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top(size = 0.5) +
#'     align_dendro(k = 3L) +
#'     ggalign(data = NULL, size = 0.2) +
#'     geom_tile(aes(y = 1L, fill = .panel))
#'
#' @export
align_gg <- function(mapping = aes(), size = NULL, action = NULL,
                     data = waiver(), limits = TRUE, facet = TRUE,
                     context = NULL, set_context = deprecated(),
                     order = deprecated(), name = deprecated(),
                     free_guides = deprecated(), free_spaces = deprecated(),
                     plot_data = deprecated(), theme = deprecated(),
                     free_labs = deprecated()) {
    assert_mapping(mapping)
    assert_s3_class(context, "plot_context", null_ok = TRUE)
    context <- update_context(context, new_context(
        active = TRUE, order = NA_integer_, name = NA_character_
    ))
    context <- deprecate_context(context, "align_gg",
        set_context = set_context, order = order, name = name
    )
    align(AlignGG,
        params = list(mapping = mapping),
        size = size, data = data, action = action %||% waiver(),
        free_guides = free_guides,
        free_labs = free_labs, free_spaces = free_spaces,
        plot_data = plot_data, theme = theme,
        facet = facet, limits = limits, context = context
    )
}

#' @usage NULL
#' @export
#' @rdname align_gg
ggalign <- align_gg

#' @importFrom vctrs vec_names
#' @importFrom ggplot2 ggproto
AlignGG <- ggproto("AlignGG", Align,
    nobs = function(self) {
        axis <- to_coord_axis(.subset2(self, "direction"))
        cli::cli_abort(c(
            "You cannot add {.fn {snake_class(self)}}",
            i = "layout {axis}-axis is not initialized or you must provide {.arg data}"
        ), call = .subset2(self, "call"))
    },
    setup_data = function(self, params, data) {
        ans <- fortify_data_frame(data)
        # we always add `.names` and `.index` to align the observations
        if (is.matrix(data)) {
            if (!is.null(old_names <- vec_names(data))) {
                ans$.names <- vec_rep(old_names, NCOL(data))
            }
            ans$.index <- vec_rep(seq_len(vec_size(data)), NCOL(data))
        } else {
            if (!is.null(old_names <- vec_names(data))) {
                ans$.names <- old_names
            }
            ans$.index <- seq_len(vec_size(data))
        }
        ans
    },
    ggplot = function(self, mapping) {
        direction <- .subset2(self, "direction")
        ans <- ggplot2::ggplot(
            mapping = add_default_mapping(mapping, switch_direction(
                direction,
                aes(y = .data$.y),
                aes(x = .data$.x)
            ))
        )
        if (is.null(.subset2(self, "input_data"))) {
            # remove the title and text of axis vertically with the layout
            ans <- ans + switch_direction(
                direction,
                theme(
                    axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()
                ),
                theme(
                    axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank()
                )
            )
        }
        ans
    },

    #' @importFrom vctrs vec_expand_grid vec_cbind
    #' @importFrom stats reorder
    draw = function(self, panel, index, extra_panel, extra_index) {
        data <- .subset2(self, "data")
        direction <- .subset2(self, "direction")
        axis <- to_coord_axis(direction)
        coord_name <- paste0(".", axis)
        ans <- data_frame0(.panel = panel[index], .index = index)
        ans[[coord_name]] <- seq_along(index)
        # if inherit from the parent layout
        if (is.waive(.subset2(self, "input_data")) && !is.null(extra_panel)) {
            # if the data is inherit from the heatmap data
            # Align object always regard row as the observations
            ans <- vec_expand_grid(col = data_frame0(
                .extra_panel = extra_panel[extra_index],
                .extra_index = extra_index
            ), row = ans)
            ans <- vec_cbind(ans$col, ans$row)
            if (!is.null(data)) {
                ans <- full_join(data, ans,
                    by.x = c(".column_index", ".index"),
                    by.y = c(".extra_index", ".index")
                )
            }
        } else if (!is.null(data)) {
            ans <- full_join(data, ans, by.x = ".index", by.y = ".index")
        }
        if (!is.null(.subset2(ans, ".names"))) {
            ans$.names <- reorder(
                .subset2(ans, ".names"),
                .subset2(ans, coord_name),
                order = FALSE
            )
        }
        plot <- .subset2(self, "plot")
        plot$data <- restore_attr_ggalign(ans, data)
        plot
    }
)
