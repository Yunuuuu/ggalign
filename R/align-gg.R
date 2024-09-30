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
#' `align_gg()` always applies a default mapping for the axis of the data index
#' in the layout. This mapping is `aes(y = .data$.y)` for horizontal stack
#' layout (including left and right heatmap annotation) and `aes(x = .data$.x)`
#' for vertical stack layout (including top and bottom heatmap annotation).
#'
#' For ggplot usage, matrix (including a simple vector) data is converted into a
#' long-format data frame. The data in the underlying `ggplot` object will
#' contain following columns:
#'
#'  - `.panel`: the panel for current layout axis. It means `x-axis` for
#'    vertical stack layout, `y-axis` for horizontal stack layout.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates
#'
#'  - `.row_names` and `.row_index`: A factor of the row names and an integer of
#'    row index of the original matrix or data frame.
#'
#'  - `.column_names` and `.column_index`: the column names and column index of
#'    the original matrix (only applicable if `data` is a `matrix`).
#'
#'  - `value`: the actual matrix value  (only applicable if `data` is a
#'    `matrix`).
#'
#' In the case where the input data is already a data frame, three additional
#' columns (`.row_names`, `.row_index`, and `.panel`) are added to the data
#' frame.
#'
#' If the data is inherit from [heatmap_layout()], an additional column will be
#' added.
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
#' @export
align_gg <- function(mapping = aes(), size = NULL,
                     free_guides = waiver(), free_spaces = waiver(),
                     plot_data = waiver(), theme = waiver(),
                     free_labs = waiver(),
                     data = NULL, limits = TRUE, facet = TRUE,
                     set_context = TRUE, order = NULL, name = NULL) {
    assert_mapping(mapping)
    align(AlignGG,
        params = list(mapping = mapping),
        size = size, data = data %||% waiver(),
        free_guides = free_guides,
        free_labs = free_labs, free_spaces = free_spaces,
        plot_data = plot_data, theme = theme,
        facet = facet, limits = limits,
        set_context = set_context, order = order, name = name
    )
}

#' @export
#' @rdname align_gg
ggalign <- align_gg

#' @importFrom data.table setDF
#' @importFrom ggplot2 ggproto
#' @importFrom stats reorder
AlignGG <- ggproto("AlignGG", Align,
    setup_data = function(self, params, data) {
        # matrix: will be reshaped to the long-format data.frame
        # data.frame: won't do any thing special
        if (is.matrix(data)) {
            data <- melt_matrix(data)
            setDF(data)
        } else {
            if (!is.null(old_rownames <- rownames(data))) {
                data$.row_names <- old_rownames
            }
            data$.row_index <- seq_len(nrow(data))
        }
        data
    },
    ggplot = function(self, mapping) {
        direction <- .subset2(self, "direction")
        ans <- ggplot2::ggplot(mapping = mapping)

        add_default_mapping(ans, switch_direction(
            direction,
            aes(y = .data$.y),
            aes(x = .data$.x)
        ))
    },
    #' @importFrom data.table as.data.table data.table setDF merge.data.table
    draw = function(self, panel, index, extra_panel, extra_index) {
        data <- as.data.table(.subset2(self, "data"))
        direction <- .subset2(self, "direction")
        axis <- to_coord_axis(direction)
        coord_name <- paste0(".", axis)
        if (is.waive(.subset2(self, "input_data")) && !is.null(extra_panel)) {
            # if the data is inherit from the heatmap data
            # Align object always regard row as the observations
            row_coords <- data.table(
                .panel = panel[index],
                .index = index,
                k = 1L
            )
            row_coords[, (coord_name) := seq_along(index)]
            column_coords <- data.table(
                .extra_panel = extra_panel[extra_index],
                .extra_index = extra_index,
                k = 1L
            )
            coords <- column_coords[row_coords,
                on = "k", allow.cartesian = TRUE
            ]
            coords[, "k" := NULL]
            data <- merge(data, coords,
                by.x = c(".column_index", ".row_index"),
                by.y = c(".extra_index", ".index"),
                sort = FALSE, all = TRUE
            )
        } else {
            coords <- data.table(.panel = panel[index], .index = index)
            coords[, (coord_name) := seq_along(index)]
            data <- merge(data, coords,
                by.x = ".row_index", by.y = ".index",
                sort = FALSE, all = TRUE
            )
        }
        setDF(data)
        if (!is.null(.subset2(data, ".row_names"))) {
            data$.row_names <- reorder(
                .subset2(data, ".row_names"),
                .subset2(data, coord_name),
                order = FALSE
            )
        }
        plot <- .subset2(self, "plot")
        plot$data <- data
        plot
    }
)
