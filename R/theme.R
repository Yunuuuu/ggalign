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
#' You can use the option `r code_quote(sprintf("%s.default_theme", pkg_nm()))`
#' to change the default theme.
#'
#' @inheritDotParams ggplot2::theme_classic
#' @importFrom ggplot2 theme_classic
#' @return A [theme][ggplot2::theme] object.
#' @examples
#' old <- options(ggalign.default_theme = theme_bw())
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
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

#' Remove axis elements
#'
#' @param axes Which axes elements should be removed? A string containing
#' one or more of `r oxford_and(.tlbr)`.
#' @param text If `TRUE`, will remove the axis labels.
#' @param ticks If `TRUE`, will remove the axis ticks.
#' @param title If `TRUE`, will remove the axis title.
#' @param line If `TRUE`, will remove the axis line.
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples 
#' p <- ggplot() + geom_point(aes(x = wt, y = qsec), data = mtcars)
#' p + theme_no_axes()
#' p + theme_no_axes("b")
#' p + theme_no_axes("l")
#' @importFrom rlang arg_match0
#' @importFrom ggplot2 theme element_blank
#' @export
theme_no_axes <- function(axes = "tlbr", text = TRUE, ticks = TRUE,
                          title = TRUE, line = FALSE) {
    assert_position(axes)
    positions <- setup_pos(axes)
    el <- list(text = text, ticks = ticks, title = title, line = line)
    el <- names(el)[vapply(el, isTRUE, logical(1L), USE.NAMES = FALSE)]
    el <- vec_expand_grid(pos = positions, el = el)
    el <- paste("axis",
        .subset2(el, "el"),
        ifelse(.subset2(el, "pos") %in% c("top", "bottom"), "x", "y"),
        .subset2(el, "pos"),
        sep = "."
    )
    el <- vec_set_names(el, el)
    theme(!!!lapply(el, function(x) element_blank()), validate = FALSE)
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
