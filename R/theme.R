#' @importFrom ggplot2 theme_classic
default_theme <- function(...) {
    theme_classic(...) +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
}

inherit_theme <- function(theme, parent) {
    if (is.null(theme)) return(NULL) # styler: off
    # if parent theme is not set, we use NULL
    parent <- parent %|w|% NULL
    if (is.waive(theme)) { # inherit from parent theme
        theme <- parent
    } else if (!is.null(parent)) { # add parent layout theme
        theme <- parent + theme
    }
    theme
}

#' @importFrom utils packageVersion
#' @importFrom ggplot2 theme_get
complete_theme <- function(theme) {
    if (!is_theme_complete(theme)) {
        theme <- if (packageVersion("ggplot2") > "3.5.1") {
            getFromNamespace("ggplot2", "complete_theme")(theme)
        } else {
            theme_get() + theme
        }
    }
    theme
}

is_theme_complete <- function(x) isTRUE(attr(x, "complete", exact = TRUE))

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
