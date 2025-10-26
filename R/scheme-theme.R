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
#' @include ggplot-theme.R
#' @export
scheme_theme <- S7::new_class(
    "scheme_theme", Scheme,
    properties = list(theme = ggplot2::class_theme),
    constructor = S7_theme_constructor
)

###############################################################
#' @importFrom S7 prop prop<-
#' @importFrom ggplot2 complete_theme
S7::method(ggalign_init, scheme_theme) <- function(x) {
    prop(x, "theme", check = FALSE) <-
        complete_theme(default_theme() + prop(x, "theme"))
    x
}

#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(scheme_theme, scheme_theme)) <-
    function(x, object, ...) {
        prop(x, "theme", check = FALSE) <- ggalign_update(
            prop(x, "theme"), prop(object, "theme"), ...
        )
        x
    }

#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(scheme_theme, ggplot2::class_theme)) <-
    function(x, object, ...) {
        prop(x, "theme", check = FALSE) <- ggalign_update(
            prop(x, "theme"), object, ...
        )
        x
    }

#' @importFrom S7 prop prop<-
S7::method(ggalign_update, list(ggplot2::class_theme, ggplot2::class_theme)) <-
    function(x, object, ...) ggfun("add_theme")(x, object, ...)

#' @importFrom S7 prop
S7::method(ggalign_update, list(Schemes, ggplot2::class_theme)) <-
    function(x, object, ...) ggalign_update(x, scheme_theme(object), ...)

#' @importFrom S7 prop prop<-
S7::method(ggalign_inherit, list(scheme_theme, scheme_theme)) <-
    function(x, object, ...) ggalign_update(object, x, ...)

#' @importFrom S7 prop
S7::method(ggalign_update, list(ggplot2::class_ggplot, scheme_theme)) <-
    function(x, object, ...) {
        x$theme <- prop(object, "theme") + x$theme
        x
    }
