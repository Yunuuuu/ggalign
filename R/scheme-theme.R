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
#' @importFrom S7 new_object S7_object
#' @importFrom ggplot2 theme is_theme
#' @export
scheme_theme <- S7::new_class(
    "scheme_theme", Scheme,
    properties = list(theme = S3_class_theme),
    constructor = rlang::new_function(
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
            th_element <- theme(!!!elements)
            th <- NULL
            for (i in seq_len(...length())) {
                if (is_theme(t <- ...elt(i))) {
                    th <- ggfun("add_theme")(th, t)
                }
            }
            theme <- ggfun("add_theme")(th, th_element)
            new_object(S7_object(), theme = theme)
        })
    )
)

###############################################################
#' @importFrom S7 prop prop<-
S7::method(scheme_init, scheme_theme) <- function(scheme) {
    prop(scheme, "theme", check = FALSE) <-
        complete_theme(default_theme()) + prop(scheme, "theme")
    scheme
}

#' @importFrom S7 S7_inherits prop
S7::method(scheme_update, list(scheme_theme, scheme_theme)) <-
    function(e1, e2, e2name) {
        prop(e1, "theme", check = FALSE) <- ggfun("add_theme")(
            prop(e1, "theme"), prop(e2, "theme"), e2name
        )
        e1
    }

#' @importFrom S7 S7_inherits prop
S7::method(scheme_update, list(Schemes, S3_class_theme)) <-
    function(e1, e2, e2name) {
        scheme_update(e1, scheme_theme(e2), e2name)
    }

#' @importFrom S7 S7_inherits prop
S7::method(scheme_update, list(scheme_theme, S3_class_theme)) <-
    function(e1, e2, e2name) {
        prop(e1, "theme", check = FALSE) <- ggfun("add_theme")(
            prop(e1, "theme"), e2, e2name
        )
        e1
    }

#' @importFrom S7 prop prop<-
S7::method(scheme_inherit, list(scheme_theme, scheme_theme)) <-
    function(e1, e2) {
        # `align_plots` control how to inherit `guides` from the layout
        # we don't need to inherit it here
        prop(e2, "theme", check = FALSE) <- prop(e1, "theme") +
            prop(e2, "theme")
        e2
    }

#' @importFrom ggplot2 class_ggplot
#' @importFrom S7 prop
local(S7::method(plot_add_scheme, list(class_ggplot, scheme_theme)) <-
    function(plot, scheme, ...) {
        plot$theme <- prop(scheme, "theme") + plot$theme
        plot
    })
