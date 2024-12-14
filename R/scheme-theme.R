#' Plot default theme
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' `scheme_theme()` serves as the default theme and will always be overridden by
#' any `theme()` settings applied directly to the plot. The default theme
#' (`scheme_theme()`) is applied first, followed by any specific `theme()`
#' settings, even if `theme()` is added before `scheme_theme()`.
#'
#' @inherit ggplot2::theme
#' @param ... A [`theme()`][ggplot2::theme] object or additional element
#' specifications not part of base ggplot2. In general, these should also be
#' defined in the `element tree` argument. [`Splicing`][rlang::splice] a list
#' is also supported.
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 8)
#' ggheatmap(small_mat) +
#'     scheme_theme(plot.background = element_rect(fill = "red"))
#'
#' # `scheme_theme()` serves as the default theme and will always be
#' # overridden by any `theme()` settings applied directly to the plot
#' ggheatmap(small_mat) +
#'     theme(plot.background = element_rect(fill = "blue")) +
#'     scheme_theme(plot.background = element_rect(fill = "red"))
#'
#' @importFrom ggplot2 theme
#' @importFrom rlang inject
#' @export
scheme_theme <- rlang::new_function(
    # We utilize editor completion by listing all `theme()` arguments here.
    # By placing `...` at the beginning, we can check if the first
    # following argument is a `theme()` object rather than individual theme
    # elements.
    c(
        rlang::exprs(... = ),
        .subset(
            rlang::fn_fmls(theme),
            vec_set_difference(names(rlang::fn_fmls(theme)), "...")
        )
    ),
    quote({
        elements <- ggfun("find_args")(..., complete = NULL, validate = NULL)
        ans <- inject(theme(!!!elements)) # for ggplot2 version < 3.5.0
        th <- NULL
        for (i in seq_len(...length())) {
            if (inherits(t <- ...elt(i), "theme")) {
                th <- ggfun("add_theme")(th, t)
            }
        }
        new_scheme_theme(ggfun("add_theme")(th, ans))
    })
)

#' @importFrom ggplot2 theme
new_scheme_theme <- function(th = theme()) {
    # I don't know why, if I omit the `object = th` argument, it won't work
    UseMethod("new_scheme_theme", th)
}

#' @importFrom rlang inject
#' @export
new_scheme_theme.theme <- function(th = theme()) {
    attrs <- attributes(th)
    attrs <- vec_slice(
        attrs, vec_set_difference(names(attrs), c("names", "class"))
    )
    inject(new_scheme(
        name = "scheme_theme", th, !!!attrs,
        class = c("scheme_theme", class(th))
    ))
}

#' @export
new_scheme_theme.scheme_theme <- function(th = theme()) th

###############################################################
#' @export
update_scheme.scheme_theme <- function(new, old, object_name) {
    ggfun("add_theme")(old, new, object_name)
}

#' @export
inherit_scheme.scheme_theme <- function(scheme, pscheme) {
    pscheme + scheme
}

#' @export
plot_add_scheme.scheme_theme <- function(plot, scheme) {
    # setup plot theme
    plot$theme <- scheme + .subset2(plot, "theme")
    plot
}
