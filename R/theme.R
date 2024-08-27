#' @importFrom ggplot2 theme_classic
default_theme <- function(...) theme_classic(...)

#' @importFrom ggplot2 theme element_blank
heatmap_theme <- function(...) {
    default_theme(...) +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
}

#' @importFrom ggplot2 theme element_blank
align_theme <- function(direction, ...) {
    default_theme() +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        ) +
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
