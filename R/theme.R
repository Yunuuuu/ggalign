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
    default_theme(...) +
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

#' @importFrom ggplot2 theme_get
complete_theme <- function(theme) {
    if (is.null(complete <- attr(theme, "complete")) || !complete) {
        theme <- theme_get() + theme
    }
    theme
}

#' @importFrom ggplot2 register_theme_elements el_def
theme_elements <- function() {
    register_theme_elements(
        element_tree = list(
            plot.patch_title = el_def("element_text", "text"),
            plot.patch_title.top = el_def("element_text", "text"),
            plot.patch_title.left = el_def("element_text", "text"),
            plot.patch_title.bottom = el_def("element_text", "text"),
            plot.patch_title.right = el_def("element_text", "text"),
            plot.patch_title.position = el_def("character"),
            plot.patch_title.position.top = el_def("character"),
            plot.patch_title.position.left = el_def("character"),
            plot.patch_title.position.bottom = el_def("character"),
            plot.patch_title.position.right = el_def("character")
        )
    )
}
