#' Plot default theme options
#'
#' @inherit ggplot2::theme
#' @param ... A [`theme()`][ggplot2::theme] object or additional element
#' specifications not part of base ggplot2. In general, these should also be
#' defined in the `⁠element tree`⁠ argument. [`Splicing`][rlang::splice] a list
#' is also supported.
#' @examples
#' plot_theme()
#' @importFrom ggplot2 theme
#' @export
plot_theme <- rlang::new_function(
    rlang::fn_fmls(theme), # we can utilize the editor completion
    quote({
        elements <- ggfun("find_args")(..., complete = NULL, validate = NULL)
        ans <- theme(!!!elements)
        th <- NULL
        for (i in seq_len(...length())) {
            if (inherits(t <- ...elt(i), "theme")) {
                th <- ggfun("add_theme")(th, t)
            }
        }
        new_plot_theme(ggfun("add_theme")(th, ans))
    })
)

#' @importFrom ggplot2 theme
new_plot_theme <- function(th = theme()) {
    UseMethod("new_plot_theme", th)
}

#' @export
new_plot_theme.theme <- function(th = theme()) {
    attrs <- attributes(th)
    attrs <- vec_slice(
        attrs, vec_set_difference(names(attrs), c("names", "class"))
    )
    rlang::inject(new_option(
        name = "plot_theme", th, !!!attrs,
        class = c("plot_theme", class(th))
    ))
}

#' @export
new_plot_theme.plot_theme <- function(th = theme()) th

#' @export
update_option.plot_theme <- function(new_option, old_option, object_name) {
    ggfun("add_theme")(old_option, new_option, object_name)
}

#' @export
inherit_option.plot_theme <- function(option, poption) {
    # By default, we'll always complete the theme when building the layout
    # so parent always exist.
    poption + option
}

#' @export
plot_add.plot_theme <- function(option, plot) {
    # setup plot theme
    plot$theme <- option + .subset2(plot, "theme")
    plot
}

#' Theme for Layout Plots
#'
#' Default theme for `r rd_layout()`.
#'
#' @details
#' You can change the default theme using the option
#' `r code_quote(sprintf("%s.default_theme", pkg_nm()))`. This option should be
#' set to a function that returns a [`theme()`][ggplot2::theme] object.
#'
#' @inheritDotParams ggplot2::theme_classic
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples
#' # Setting a new default theme
#' old <- options(ggalign.default_theme = function() theme_bw())
#'
#' # Creating a heatmap with the new theme
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro(k = 3L)
#'
#' # Restoring the old default theme
#' options(ggalign.default_theme = old)
#' @importFrom ggplot2 theme_classic
#' @export
theme_ggalign <- function(...) {
    theme_classic(...) +
        theme(
            axis.line = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
}

default_theme <- function() {
    opt <- sprintf("%s.default_theme", pkg_nm())
    if (is.null(ans <- getOption(opt, default = NULL))) {
        return(theme_ggalign())
    }
    if (is.function(ans <- allow_lambda(ans))) {
        if (!inherits(ans <- rlang::exec(ans), "theme")) {
            cli::cli_abort(c(
                "{.arg {opt}} must return a {.fn theme} object",
                i = "You have provided {.obj_type_friendly {ans}}"
            ))
        }
    } else {
        cli::cli_abort(c(
            "{.arg {opt}} must be a {.cls function}",
            i = "You have provided {.obj_type_friendly {ans}}"
        ))
    }
    ans
}

#' Remove axis elements
#'
#' @param axes Which axes elements should be removed? A string containing
#' one or more of `r oxford_and(c(.tlbr, "x", "y"))`.
#' @param text If `TRUE`, will remove the axis labels.
#' @param ticks If `TRUE`, will remove the axis ticks.
#' @param title If `TRUE`, will remove the axis title.
#' @param line If `TRUE`, will remove the axis line.
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples
#' p <- ggplot() +
#'     geom_point(aes(x = wt, y = qsec), data = mtcars)
#' p + theme_no_axes()
#' p + theme_no_axes("b")
#' p + theme_no_axes("l")
#' @importFrom rlang arg_match0
#' @importFrom ggplot2 theme element_blank
#' @export
theme_no_axes <- function(axes = "xy", text = TRUE, ticks = TRUE,
                          title = TRUE, line = FALSE) {
    assert_string(axes, empty_ok = FALSE)
    if (grepl("[^tlbrxy]", axes)) {
        cli::cli_abort(sprintf(
            "{.arg axes} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    }
    positions <- .subset(
        list(
            t = "top", l = "left", b = "bottom", r = "right",
            x = c("top", "bottom"), y = c("left", "right")
        ),
        split_position(axes)
    )
    positions <- vec_unique(unlist(positions, FALSE, FALSE))
    el <- list(text = text, ticks = ticks, title = title, line = line)
    el <- names(el)[vapply(el, isTRUE, logical(1L), USE.NAMES = FALSE)]
    el <- vec_expand_grid(pos = positions, el = el)
    el <- paste("axis",
        .subset2(el, "el"),
        ifelse(.subset2(el, "pos") %in% c("top", "bottom"), "x", "y"),
        .subset2(el, "pos"),
        sep = "."
    )
    el <- vec_set_names(vec_rep(list(element_blank()), length(el)), el)
    theme(!!!el, validate = FALSE)
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
