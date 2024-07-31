#' Heatmap annotation with `AlignGG`
#'
#' @inheritParams align
#' @importFrom ggplot2 aes
#' @inheritParams ggplot2::ggplot
#'
#' @section ggplot2 details:
#' `align_gg` initializes a `ggplot` data and `mapping`. The data input can be a
#' matrix, a data frame, or a simple vector that will be converted into a
#' one-column matrix, and can inherit from the heatmap matrix.
#'
#' But for ggplot usage, matrix (including a simple vector) data is converted
#' into a long-format data frame, similar to the process utilized in
#' `ggheatmap`.  But note that the long-format data frame does not contain
#' `.row_panel` or `.column_panel` column, as annotations can only have one
#' facet axis. In the case where the input data is already a data frame, three
#' additional columns-(`.row_names`, `.row_index`, and `.panel`)—are added to
#' the data frame.
#'
#' The data in the underlying `ggplot` object contains following columns:
#'
#'  - `.panel`: the panel for current annotation
#'
#'  - `.row_names` and `.row_index`: the row names and row index of the original
#'    matrix or data frame.
#'
#'  - `.column_names` and `.column_index`: the row and column index of the
#'    original matrix (only applicable if `data` is a `matrix`).
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates
#'
#'  - `value`: the actual matrix value  (only applicable if `data` is a
#'    `matrix`).
#'
#' @return A `AlignGG` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     gganno() +
#'     geom_point(aes(y = value))
#' @importFrom rlang caller_call current_call
#' @export
align_gg <- function(mapping = aes(), data = NULL, size = NULL,
                     labels = NULL, labels_nudge = NULL,
                     set_context = TRUE, order = NULL, name = NULL) {
    assert_mapping(mapping)
    align(AlignGG,
        params = list(mapping = mapping),
        labels = labels, labels_nudge = labels_nudge,
        size = size, data = data,
        set_context = set_context, order = order, name = name
    )
}

#' @export
#' @rdname align_gg
ggalign <- align_gg

AlignGG <- ggplot2::ggproto("AlignGG", Align,
    setup_data = function(self, data, params) {
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
        ans <- ggplot2::ggplot(mapping = mapping) +
            ggplot2::theme_bw()

        add_default_mapping(ans, switch_direction(
            .subset2(self, "direction"),
            aes(y = .data$.y),
            aes(x = .data$.x)
        ))
    },
    draw = function(self, panels, index) {
        data <- .subset2(self, "data")
        axis <- to_coord_axis(.subset2(self, "direction"))
        coords <- data_frame0(.panel = panels[index], .index = index)
        coords[[paste0(".", axis)]] <- seq_along(index)
        data <- merge(data, coords,
            by.x = ".row_index", by.y = ".index",
            sort = FALSE, all = TRUE
        )
        plot <- .subset2(self, "plot")
        plot$data <- data
        plot
    }
)
