default_theme <- function() {
    opt <- sprintf("%s.default_theme", pkg_nm())
    if (is.null(ans <- getOption(opt, default = NULL))) {
        ans <- theme_ggalign()
    } else if (!inherits(ans, "theme")) {
        cli::cli_abort(c(
            "{.arg {opt}} must be a {.fn theme} object",
            i = "You have provided a {.obj_type_friendly {ans}}"
        ))
    }
    ans
}

#' Complete theme for layout plots
#'
#' Default theme for `r rd_layout()`.
#'
#' @details
#' You can use the option `r rd_values(sprintf("%s.default_theme", pkg_nm()))`
#' to change the default theme.
#'
#' @inheritDotParams ggplot2::theme_classic
#' @importFrom ggplot2 theme_classic
#' @return A [theme][ggplot2::theme] object.
#' @examples
#' old <- options(ggalign.default_theme = theme_bw())
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     align_dendro(k = 3L)
#' options(ggalign.default_theme = old)
#' @export
theme_ggalign <- function(...) {
    theme_classic(...) +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
}

inherit_theme <- function(theme, parent) {
    if (is.null(theme)) return(default_theme()) # styler: off
    # if parent theme is not set, we use the default theme
    parent <- parent %|w|% default_theme()
    if (is.waive(theme)) { # inherit from parent theme
        theme <- parent
    } else if (!is.null(parent)) { # add parent layout theme
        theme <- parent + theme
    }
    theme
}

#' @importFrom rlang try_fetch
#' @importFrom ggplot2 theme_get
complete_theme <- function(theme) {
    if (!is_theme_complete(theme <- theme %||% theme_get())) {
        theme <- try_fetch(
            ggfun("complete_theme")(theme),
            error = function(cnd) theme_get() + theme
        )
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
