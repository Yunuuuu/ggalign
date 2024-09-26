#' Create ggplot object with layout panel data
#'
#' This is similar with `ggalign()` function, but it will always use the layout
#' panel data. `ggpanel` is just an alias of `align_panel`.
#'
#' @param mapping Additional default list of aesthetic mappings to use for plot.
#' @inheritParams align
#' @importFrom ggplot2 aes
#' @inheritParams ggplot2::ggplot
#'
#' @section ggplot2 specification:
#' `align_panel` initializes a `ggplot` data and `mapping`.
#'
#' `align_panel()` always applies a default mapping for the axis of the data
#' index in the layout. This mapping is `aes(y = .data$.y)` for horizontal stack
#' layout (including left and right heatmap annotation) and `aes(x = .data$.x)`
#' for vertical stack layout (including top and bottom heatmap annotation).
#'
#' The data in the underlying `ggplot` object contains following columns:
#'
#'  - `.panel`: the panel for current layout axis. It means `x-axis` for
#'    vertical stack layout, `y-axis` for horizontal stack layout.
#'
#'  - `.index`: the index of the original layout data.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates
#'
#' @return A `AlignPanel` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     ggalign() +
#'     geom_point(aes(y = value))
#' @importFrom rlang caller_call current_call
#' @export
align_panel <- function(mapping = aes(), size = NULL,
                        free_spaces = waiver(), plot_data = waiver(),
                        theme = waiver(), free_labs = waiver(),
                        limits = TRUE, facet = TRUE,
                        set_context = TRUE, order = NULL, name = NULL) {
    assert_mapping(mapping)
    align(AlignPanel,
        params = list(mapping = mapping),
        size = size, data = NULL,
        free_labs = free_labs, free_spaces = free_spaces,
        plot_data = plot_data, theme = theme,
        set_context = set_context, order = order, name = name
    )
}

#' @export
#' @rdname align_panel
ggpanel <- align_panel

#' @importFrom ggplot2 ggproto
AlignPanel <- ggproto("AlignPanel", Align,
    nobs = function(self) {
        axis <- to_coord_axis(.subset2(self, "direction"))
        cli::cli_abort(c(
            "You cannot add {.fn {snake_class(self)}}",
            i = "layout {axis}-axis is not initialized"
        ), call = .subset2(self, "call"))
    },
    ggplot = function(self, mapping) {
        direction <- .subset2(self, "direction")
        ans <- ggplot2::ggplot(mapping = mapping) +
            # remove the title and text of axis vertically with the layout
            switch_direction(
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

        add_default_mapping(ans, switch_direction(
            direction,
            aes(y = .data$.y),
            aes(x = .data$.x)
        ))
    },
    draw = function(self, panel, index, extra_panel, extra_index) {
        axis <- to_coord_axis(.subset2(self, "direction"))
        data <- data_frame0(.panel = panel[index], .index = index)
        data[[paste0(".", axis)]] <- seq_along(index)
        plot <- .subset2(self, "plot")
        plot$data <- data
        plot
    }
)
