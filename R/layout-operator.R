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
#' `r rd_layout()`: `-` and `&`.
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
#' # If the active layout is the `stack_layout()` itself, `-`
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
#' @include layout-.R
#' @name layout-operator
local(S7::method(`+`, list(LayoutProto, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    })

local(S7::method(`+`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (is.null(e2)) return(e1) # styler: off
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    ggalign_update(e1, e2, e2name)
})

local(S7::method(`+`, list(LayoutProto, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot use {.code -} with a single argument.",
            "i" = "Did you accidentally put {.code -} on a new line?"
        ))
    })

#' @include layout-.R
local(S7::method(`-`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    layout_apply_selected(e1, e2, e2name)
})

local(S7::method(`+`, list(LayoutProto, S7::class_missing)) <-
    function(e1, e2) {
        cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    })

#' @include layout-.R
local(S7::method(`&`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2, env = caller_env(2L)))
    layout_apply_all(e1, e2, e2name)
})

#' @include utils-ggplot.R
local(
    for (left in list(QuadLayout, ChainLayout)) {
        for (right in list(
            ggplot2::class_ggplot,
            S7::new_S3_class("quad_active"),
            S7::new_S3_class("quad_anno"),
            layout_title,
            layout_theme,
            layout_tags,
            layout_design,
            CraftBox,
            ChainLayout,
            QuadLayout,
            Domain
        )) {
            S7::method(layout_apply_selected, list(left, right)) <-
                function(x, object, objectname) {
                    cli_abort(c(
                        sprintf("Cannot add %s with {.code -}", objectname),
                        i = "Try to use {.code +} instead"
                    ))
                }
            S7::method(layout_apply_all, list(left, right)) <-
                function(x, object, objectname) {
                    cli_abort(c(
                        sprintf("Cannot add %s with {.code &}", objectname),
                        i = "Try to use {.code +} instead"
                    ))
                }
        }
    }
)
