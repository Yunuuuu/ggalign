#' Modify theme of the layout
#'
#' @inherit ggplot2::theme
#' @param ... A [`theme()`][ggplot2::theme] object or additional element
#' specifications not part of base ggplot2. In general, these should also be
#' defined in the `element tree` argument. [`Splicing`][rlang::splice] a list
#' is also supported.
#'
#' @details
#' A [`theme()`][ggplot2::theme] object used to customize various elements of
#' the layout, including `guides`, `title`, `subtitle`, `caption`, `margins`,
#' `panel.border`, and `background`. By default, the theme will inherit from the
#' parent `layout`.
#'
#' - `guides`, `panel.border`, and `background` will always be used even for the
#' nested `alignpatches` object.
#'
#' - `title`, `subtitle`, `caption`, and `margins` will be added for the
#' top-level `alignpatches` object only.
#'
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(
#'     p1 + theme(plot.background = element_blank()),
#'     p2 + theme(plot.background = element_blank()),
#'     p3 + theme(plot.background = element_blank())
#' ) +
#'     layout_theme(plot.background = element_rect(fill = "red"))
#' @include ggplot-theme.R
#' @export
layout_theme <- S7::new_class(
    "layout_theme",
    properties = list(theme = ggplot2::class_theme),
    constructor = S7_theme_constructor
)

#' @importFrom ggplot2 is_waiver
local({
    S7::method(`+`, list(layout_theme, layout_theme)) <-
        function(e1, e2) {
            prop(e1, "theme") <- prop(e1, "theme") + prop(e2, "theme")
            e1
        }
    S7::method(`+`, list(layout_theme, ggplot2::class_theme)) <-
        function(e1, e2) {
            prop(e1, "theme") <- prop(e1, "theme") + e2
            e1
        }
    S7::method(`+`, list(S7::class_any, layout_theme)) <-
        function(e1, e2) {
            if (is.null(e1)) {
                return(e2)
            }
            stop_incompatible_op("+", e1, e2)
        }
    S7::method(`+`, list(layout_theme, S7::class_any)) <-
        function(e1, e2) {
            if (is.null(e2)) {
                return(e1)
            }
            stop_incompatible_op("+", e1, e2)
        }
})

##############################################################
#' Add layout annotation
#'
#' This function is a placeholder for future extensions.
#' If you're trying to apply a theme, use [layout_theme()] instead.
#'
#' @param ... Currently unused. May accept a theme in the future.
#' @param theme A theme object. If not `waiver()`, an error will be raised.
#'
#' @return None. This function is used for input validation.
#' @importFrom ggplot2 is_theme is_waiver
#' @export
#' @keywords internal
layout_annotation <- function(..., theme = waiver()) {
    if (is_theme(...elt(1L)) || !is_waiver(theme)) {
        cli_abort("Please use {.fn layout_theme} instead; {.fn layout_annotation} is reserved for future extensions.")
    }
}
