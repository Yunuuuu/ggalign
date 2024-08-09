#' @importFrom ggplot2 theme element_blank
default_theme <- function(...) {
    ggplot2::theme_classic(...) +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
}

heatmap_theme <- function(...) {
    default_theme(...)
}

#' @importFrom ggplot2 theme element_blank
align_theme <- function(direction, ...) {
    default_theme() +
        # remove the title and text of axis parallelly with layout
        switch_direction(
            direction,
            theme(
                axis.title.y = element_blank(),
                axis.text.y = element_blank(),
                axis.ticks.y = element_blank()
            ),
            theme(
                axis.title.x = element_blank(),
                axis.text.x = element_blank(),
                axis.ticks.x = element_blank()
            )
        )
}
