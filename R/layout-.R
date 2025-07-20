#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @importFrom ggplot2 is_theme
#' @include schemes.R
#' @keywords internal
LayoutProto <- S7::new_class("LayoutProto",
    properties = list(
        current = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be of length 1")
                }
            },
            default = NA_integer_
        ),
        schemes = schemes, # used to provide global parameters for all plots
        titles = S7::class_list,
        annotation = S7::class_list,
        theme = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (!is.null(value) && !is_theme(value)) {
                    return("must be a 'theme' object")
                }
            },
            default = NULL
        )
    )
)

S7::method(print, LayoutProto) <- print.alignpatches

#' @importFrom grid grid.draw
S7::method(grid.draw, LayoutProto) <- grid.draw.alignpatches

#' @export
S7::method(alignpatch, LayoutProto) <- function(x) alignpatch(ggalign_build(x))

#' Subset a `Layout` object
#'
#' Used by [`ggplot_build`][ggplot2::ggplot_build] and
#' [`ggsave`][ggplot2::ggsave]
#'
#' @param x A `Layout` object
#' @param name A string of slot name in `Layout` object.
#' @return The slot value.
#' @importFrom S7 prop
#' @export
#' @keywords internal
S7::method(`$`, LayoutProto) <- function(x, name) prop(x, name)

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
#' @name layout-operator
S7::method(`+`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_add(e1, e2, e2name)
}

S7::method(`-`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code -} with a single argument.",
            "i" = "Did you accidentally put {.code -} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_subtract(e1, e2, e2name)
}

S7::method(`&`, list(LayoutProto, S7::class_any)) <- function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_and_add(e1, e2, e2name)
}

layout_add <- S7::new_generic(
    "layout_add", c("layout", "object"),
    function(layout, object, object_name) S7::S7_dispatch()
)

layout_subtract <- S7::new_generic(
    "layout_subtract", c("layout", "object"),
    function(layout, object, object_name) S7::S7_dispatch()
)

layout_and_add <- S7::new_generic(
    "layout_and_add", c("layout", "object"),
    function(layout, object, object_name) S7::S7_dispatch()
)

S7::method(layout_subtract, S3_ggplot) <- function(layout, object, object_name) {
    cli_abort(c(
        sprintf("Cannot add %s with {.code -}", object_name),
        i = "Try to use {.code +} instead"
    ))
}

S7::method(layout_and_add, S3_ggplot) <- function(layout, object, object_name) {
    cli_abort(c(
        sprintf("Cannot add %s with {.code &}", object_name),
        i = "Try to use {.code +} instead"
    ))
}
