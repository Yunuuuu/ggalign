#' Create ggplot object in the layout panels
#'
#' This is similar with `ggalign()` function, but it will always use the layout
#' panels data. `ggpanels` is just an alias of `align_panels`.
#'
#' @param mapping Additional default list of aesthetic mappings to use for plot.
#' @inheritParams align
#' @importFrom ggplot2 aes
#' @inheritParams ggplot2::ggplot
#'
#' @section ggplot2 details:
#' `align_panels` initializes a `ggplot` data and `mapping`.
#'
#' The internal will always use a default mapping of `aes(y = .data$.y)` or
#' `aes(x = .data$.x)`.
#'
#' The data in the underlying `ggplot` object contains following columns:
#'
#'  - `.panel`: the panel for current layout axis.
#'
#'  - `.index`: the index of the original layout axis.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates
#'
#' @return A `AlignPanels` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     ggalign() +
#'     geom_point(aes(y = value))
#' @importFrom rlang caller_call current_call
#' @export
align_panels <- function(mapping = aes(), size = NULL, plot_data = waiver(),
                         set_context = TRUE, order = NULL, name = NULL) {
    assert_mapping(mapping)
    align(AlignPanels,
        params = list(mapping = mapping),
        size = size, data = NULL, plot_data = plot_data,
        set_context = set_context, order = order, name = name
    )
}

#' @export
#' @rdname align_panels
ggpanels <- align_panels

AlignPanels <- ggplot2::ggproto("AlignPanels", Align,
    setup_data = function(self, data, params) NULL,
    ggplot = function(self, mapping) {
        ans <- ggplot2::ggplot(mapping = mapping)

        add_default_mapping(ans, switch_direction(
            .subset2(self, "direction"),
            aes(y = .data$.y),
            aes(x = .data$.x)
        ))
    },
    draw = function(self, panels, index, extra_panels, extra_index) {
        axis <- to_coord_axis(.subset2(self, "direction"))
        data <- data_frame0(.panel = panels[index], .index = index)
        data[[paste0(".", axis)]] <- seq_along(index)
        plot <- .subset2(self, "plot")
        plot$data <- data
        plot
    }
)
