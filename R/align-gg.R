#' Create ggplot object with a customized data
#'
#' `ggalign` is just an alias of `align_gg`.
#'
#' @param mapping Additional default list of aesthetic mappings to use for plot.
#' @inheritParams align
#' @importFrom ggplot2 aes
#' @inheritParams ggplot2::ggplot
#'
#' @section ggplot2 specification:
#' `align_gg` initializes a `ggplot` data and `mapping`.
#'
#' The internal will always use a default mapping of `aes(y = .data$.y)` or
#' `aes(x = .data$.x)`.
#'
#' For ggplot usage, matrix (including a simple vector) data is converted into a
#' long-format data frame, similar to the process utilized in `ggheatmap`. But
#' note that the long-format data frame does not contain `.xpanel` or `.ypanel`
#' column, as `align_gg` can only have one facet axis. In the case where the
#' input data is already a data frame, three additional columns-(`.row_names`,
#' `.row_index`, and `.panel`)—are added to the data frame.
#'
#' The data in the underlying `ggplot` object contains following columns:
#'
#'  - `.panel`: the panel for current layout axis.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates
#'
#'  - `.row_names` and `.row_index`: the row names and row index of the original
#'    matrix or data frame.
#'
#'  - `.column_names` and `.column_index`: the column names and column index of
#'    the original matrix (only applicable if `data` is a `matrix`).
#'
#'  - `value`: the actual matrix value  (only applicable if `data` is a
#'    `matrix`).
#'
#' if data is inherit from the [heatmap][ggheatmap] layout, an additional column
#' will be added.
#'
#'  - `.extra_panel`: the panel information for column (left or right
#'    annotation) or row (top or bottom annotation).
#'
#' @return A `AlignGG` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     ggalign() +
#'     geom_point(aes(y = value))
#' @importFrom rlang caller_call current_call
#' @export
align_gg <- function(data = NULL, mapping = aes(), size = NULL,
                     plot_data = waiver(), limits = TRUE, facet = TRUE,
                     set_context = TRUE, order = NULL, name = NULL) {
    assert_mapping(mapping)
    align(AlignGG,
        params = list(mapping = mapping),
        size = size, data = data %||% waiver(),
        plot_data = plot_data,
        facet = facet, limits = limits,
        set_context = set_context, order = order, name = name
    )
}

#' @export
#' @rdname align_gg
ggalign <- align_gg

AlignGG <- ggplot2::ggproto("AlignGG", Align,
    setup_data = function(self, params, data) {
        # matrix: will be reshaped to the long-format data.frame
        # data.frame: won't do any thing special
        if (is.matrix(data)) {
            data <- melt_matrix(data)
        } else {
            data <- as_tibble0(data, rownames = ".row_names")
            data$.row_index <- seq_len(nrow(data))
        }
        data
    },
    ggplot = function(self, mapping) {
        direction <- .subset2(self, "direction")
        ans <- ggplot2::ggplot(mapping = mapping) +
            align_theme(direction)

        add_default_mapping(ans, switch_direction(
            direction,
            aes(y = .data$.y),
            aes(x = .data$.x)
        ))
    },
    draw = function(self, panel, index, extra_panel, extra_index) {
        data <- .subset2(self, "data")
        direction <- .subset2(self, "direction")
        axis <- to_coord_axis(direction)
        if (is.waive(.subset2(self, "input_data")) && !is.null(extra_panel)) {
            # Align object always regard row as the observations
            row_coords <- data_frame0(
                .panel = panel[index],
                .index = index
            )
            row_coords[[paste0(".", axis)]] <- seq_along(index)
            column_coords <- data_frame0(
                .extra_panel = extra_panel[extra_index],
                .extra_index = extra_index
            )
            coords <- merge(column_coords, row_coords,
                by = NULL, sort = FALSE, all = TRUE
            )
            data <- merge(data, coords,
                by.x = c(".column_index", ".row_index"),
                by.y = c(".extra_index", ".index"),
                sort = FALSE, all = TRUE
            )
        } else {
            coords <- data_frame0(.panel = panel[index], .index = index)
            coords[[paste0(".", axis)]] <- seq_along(index)
            data <- merge(data, coords,
                by.x = ".row_index", by.y = ".index",
                sort = FALSE, all = TRUE
            )
        }
        plot <- .subset2(self, "plot")
        plot$data <- data
        plot
    }
)
