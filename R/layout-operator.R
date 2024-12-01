#' Layout operator
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#'  - `+`: Adds elements to the active plot in the active layout.
#'  - `&`: Applies elements to all plots in the layout.
#'  - `-`: Adds elements to multiple plots in the layout.
#'
#' @details
#' The `+` operator is straightforward and should be used as needed.
#'
#' In order to reduce code repetition `ggalign` provides two operators for
#' adding ggplot elements (geoms, themes, facets, etc.) to multiple/all plots in
#' `r rd_layout()`: `-` and `&`. See `vignette("operator")` for details.
#'
#' @param e1 A `r rd_layout()`.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' ggheatmap(small_mat) +
#'     anno_top() +
#'     ggalign() +
#'     geom_point(aes(y = value))
#'
#' # `&` operator apply it to all plots
#' ggheatmap(small_mat) +
#'     anno_top() +
#'     align_dendro() &
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#'
#' # If the active layout is the annotation stack, the `-` operator will only
#' # add the elements to all plots in the active annotation stack:
#' ggheatmap(small_mat) +
#'     anno_left(size = 0.2) +
#'     align_dendro(aes(color = branch), k = 3L) +
#'     align_dendro(aes(color = branch), k = 3L) -
#'     # Modify the the color scales of all plots in the left annotation
#'     scale_color_brewer(palette = "Dark2")
#'
#' # If the active layout is the `ggstack()`/`stack_layout()` itself, `-`
#' # applies the elements to all plots in the layout except the nested
#' # `ggheatmap()`/`quad_layout()`.
#' stack_alignv(small_mat) +
#'     align_dendro() +
#'     ggtitle("I'm from the parent stack") +
#'     ggheatmap() +
#'     # remove any active context
#'     stack_active() +
#'     align_dendro() +
#'     ggtitle("I'm from the parent stack") -
#'     # Modify the the color scales of all plots in the stack layout except the
#'     # heatmap layout
#'     scale_color_brewer(palette = "Dark2") -
#'     # set the background of all plots in the stack layout except the heatmap
#'     # layout
#'     theme(plot.background = element_rect(fill = "red"))
#'
#' @aliases +.QuadLayout &.QuadLayout -.QuadLayout
#' @aliases +.HeatmapLayout &.HeatmapLayout -.HeatmapLayout
#' @aliases +.ggheatmap &.ggheatmap -.ggheatmap
#' @aliases +.ggside &.ggside -.ggside
#' @aliases +.StackLayout &.StackLayout -.StackLayout
#' @aliases +.CrossLayout
#' @name layout-operator
NULL

utils::globalVariables(".Generic")

methods::setMethod("Ops", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code {.Generic}} with a single argument.",
            "i" = "Did you accidentally put {.code {.Generic}} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    switch(.Generic, # nolint
        `+` = layout_add(e1, e2, e2name),
        `-` = layout_subtract(e1, e2, e2name),
        `&` = layout_and_add(e1, e2, e2name),
        stop_incompatible_op(.Generic, e1, e2)
    )
})

#################################################################
layout_add <- function(layout, object, object_name) {
    UseMethod("layout_add")
}

#' @export
layout_add.QuadLayout <- function(layout, object, object_name) {
    quad_layout_add(object, layout, object_name)
}

#' @export
layout_add.StackLayout <- function(layout, object, object_name) {
    stack_layout_add(object, layout, object_name)
}

#################################################################
layout_subtract <- function(layout, object, object_name) {
    UseMethod("layout_subtract")
}

#' @export
layout_subtract.QuadLayout <- function(layout, object, object_name) {
    quad_layout_subtract(object, layout, object_name)
}

#' @export
layout_subtract.StackLayout <- function(layout, object, object_name) {
    stack_layout_subtract(object, layout, object_name)
}

#################################################################
layout_and_add <- function(layout, object, object_name) {
    UseMethod("layout_and_add")
}

#' @export
layout_and_add.QuadLayout <- function(layout, object, object_name) {
    quad_layout_and_add(object, layout, object_name)
}

#' @export
layout_and_add.StackLayout <- function(layout, object, object_name) {
    stack_layout_and_add(object, layout, object_name)
}
