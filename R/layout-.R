# Will ensure serialisation includes a link to the ggalign namespace
# Copied from patchwork
namespace_link <- function() NULL

# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
# TO-DO: use S7
#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @keywords internal
methods::setClass("Layout",
    list(
        active = "ANY", # current active plot
        controls = "list", # used to provide global parameters for all plots
        # control the layout, `theme` will also be used by `ggsave`
        titles = "list",
        annotation = "list", # To-DO add `pacth_titles` for layout
        theme = "ANY",
        `_namespace` = "ANY"
    ),
    prototype = list(
        active = NULL, titles = list(), annotation = list(), theme = NULL,
        `_namespace` = namespace_link
    )
)

#' @export
print.Layout <- print.alignpatches

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.Layout <- grid.draw.alignpatches

#' @export
alignpatch.Layout <- function(x) alignpatch(ggalign_build(x))

#' Print Layout object
#'
#' @param object A `r rd_layout()`.
#' @return The input invisiblely.
#' @importFrom methods show
#' @export
#' @keywords internal
methods::setMethod("show", "Layout", function(object) {
    print(object)
})

#' Subset a `Layout` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A `Layout` object
#' @param name A string of slot name in `Layout` object.
#' @return The slot value.
#' @importFrom methods slot
#' @export
#' @keywords internal
methods::setMethod("$", "Layout", function(x, name) {
    slot(x, name)
})

#########################################################
#' Layout operator
#'
#'  - `+`: adds elements to the active plot in the active layout.
#'  - `&`: applies elements to all plots in the layout.
#'  - `-`:
#'    * `quad_layout()`: Adds elements to all plots in the active layout, as
#'      well as to any nested layouts within it.
#'    * `stack_layout()`:
#' @details
#' In order to reduce code repetition `ggalign` provides two operators for
#' adding ggplot elements (geoms, themes, facets, etc.) to multiple/all plots in
#' `r rd_layout()`: `-` and `&`.
#'
#' Like `patchwork`, `&` add the element to all plots in the layout. If the
#' element is a [theme][ggplot2::theme], this will also modify the layout
#' theme.
#'
#' The key difference between `&` and `-` is in how they behave in
#' [`quad_layout()`]. The `-` operator only applies the element to the current
#' active context in [`quad_layout()`], if no active context, it will apply to
#' all plots in the [`quad_layout()`]. Using `-` might seem unintuitive if you
#' think of the operator as "subtract", the underlying reason is that `-` is the
#' only operator in the same precedence group as `+`.
#'
#' @param e1 A `r rd_layout()`.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @examples
#' mat <- matrix(rnorm(56), nrow = 7)
#' ggheatmap(mat) +
#'     hmanno("t") +
#'     ggalign() +
#'     geom_point(aes(y = value))
#'
#' ggheatmap(mat) +
#'     anno_top() +
#'     align_dendro() &
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#' ggheatmap(mat) +
#'     anno_top() +
#'     align_dendro() -
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#'
#' # used in the layout, define the default action for all plots in the layout
#' ggheatmap(matrix(rnorm(72), nrow = 8)) -
#'     theme(plot.background = element_rect(fill = "red"))
#'
#' # You can also add it for a single plot
#' ggheatmap(matrix(rnorm(72), nrow = 8)) -
#'     theme(plot.background = element_rect(fill = "red")) +
#'     # here, we modify the plot action for the heatmap body
#'     theme(plot.background = element_rect(fill = "blue"))
#' @name layout-operator
NULL

utils::globalVariables(".Generic")

###########################################################
default_layout <- function(layout) {
    layout@theme <- default_theme() + layout@theme
    # we by default, collect all guides
    layout@controls$plot_align["guides"] <- list(
        .subset2(.subset2(layout@controls, "plot_align"), "guides") %|w|% "tlbr"
    )
    # we by default, use `default_theme()`
    layout@controls$plot_theme <- update_option(
        .subset2(layout@controls, "plot_theme"),
        new_plot_theme(default_theme())
    )
    layout
}

######################################################################
# layout params are used to align the observations
new_layout_params <- function(panel = NULL, index = NULL, nobs = NULL) {
    list(panel = panel, index = index, nobs = nobs)
}

set_layout_params <- function(params) {
    if (is.null(params)) return(NULL) # styler: off
    # if `nobs` is not initialized, it means no `Align` object exist
    # it's not necessary to initialize the `panel` and `index`
    if (is.null(nobs <- .subset2(params, "nobs"))) {
        return(params)
    }
    panel <- .subset2(params, "panel") %||% factor(rep_len(1L, nobs))
    index <- .subset2(params, "index") %||% reorder_index(panel)
    new_layout_params(panel, index, nobs)
}

reorder_index <- function(panel, index = NULL) {
    index <- index %||% seq_along(panel)
    unlist(split(index, panel[index]), recursive = FALSE, use.names = FALSE)
}

#' @keywords internal
update_layout_params <- function(layout, ..., params) {
    UseMethod("update_layout_params")
}

#' @importFrom methods slot slot<-
#' @export
update_layout_params.QuadLayout <- function(layout, direction, ..., params) {
    slot(layout, direction) <- params
    if (is_horizontal(direction)) {
        if (!is.null(left <- layout@left)) {
            layout@left <- update_layout_params(left, params = params)
        }
        if (!is.null(right <- layout@right)) {
            layout@right <- update_layout_params(right, params = params)
        }
    } else {
        if (!is.null(top <- layout@top)) {
            layout@top <- update_layout_params(top, params = params)
        }
        if (!is.null(bottom <- layout@bottom)) {
            layout@bottom <- update_layout_params(bottom, params = params)
        }
    }
    layout
}

#' @importFrom methods slot slot<-
#' @export
update_layout_params.StackLayout <- function(layout, ..., params) {
    slot(layout, "layout") <- params
    layout@plots <- lapply(layout@plots, function(plot) {
        if (is_layout(plot)) {
            update_layout_params(plot,
                direction = layout@direction,
                params = params
            )
        } else {
            plot
        }
    })
    layout
}

############################################################
#' Get the statistics from the layout
#'
#' @param x A `r rd_layout()`.
#' @inheritParams rlang::args_dots_used
#' @return The statistics
#' @export
ggalign_stat <- function(x, ...) {
    rlang::check_dots_used()
    UseMethod("ggalign_stat")
}

#' @param position A string of `r oxford_or(.TLBR)`.
#' @export
#' @rdname ggalign_stat
ggalign_stat.QuadLayout <- function(x, position, ...) {
    ggalign_stat(x = slot(x, position), ...)
}

#' @param what A single number or string of the plot elements in the stack
#' layout.
#' @export
#' @rdname ggalign_stat
ggalign_stat.StackLayout <- function(x, what, ...) {
    if (is.null(ans <- .subset2(x@plots, what))) {
        cli::cli_abort("Cannot find {what} plot in this stack layout")
    }
    ggalign_stat(x = ans, ...)
}

#' @export
ggalign_stat.align <- function(x, ...) {
    .subset2(.subset2(x, "Object"), "statistics")
}

#' @export
ggalign_stat.default <- function(x, ...) {
    cli::cli_abort("no statistics found for {.obj_type_friendly {x}}")
}

####################################################
# we keep an attribute `ggalign` across all data
# this is used to pass additional annotation informations
restore_attr_ggalign <- function(data, original) {
    if (is.null(attr(data, "ggalign")) &&
        !is.null(ggalign_params <- attr(original, "ggalign"))) {
        attr(data, "ggalign") <- ggalign_params
    }
    data
}

#' Get data from the `ggalign` attribute
#'
#' This function extracts data from the `ggalign` attribute retained in the data
#' when rendering `r rd_layout()`. The `ggalign` attribute holds supplementary
#' information for input data.
#'
#' @param x Input data for the function used to transform the layout data.
#' @param field A string specifying the particular data to retrieve from the
#' `ggalign` attribute. If `NULL`, the entire `ggalign` attribute will be
#' returned.  Commonly, this attribute list is attached by [`fortify_matrix()`]
#' or [`fortify_data_frame()`] functions (refer to the `ggalign attributes`
#' section in the documentation for details). For examples, see
#' [`fortify_matrix.MAF()`].
#'
#' @return The specified data from the `ggalign` attribute or `NULL` if it is
#' unavailable.
#'
#' @export
ggalign_attr <- function(x, field = NULL) {
    if (is.null(x <- attr(x, "ggalign")) || is.null(field)) {
        return(x)
    }
    .subset2(x, field)
}

add_ggalign_attr <- function(x, values) {
    if (is.null(suppl <- attr(x, "ggalign"))) {
        attr(x, "ggalign") <- values
    } else {
        attr(x, "ggalign") <- c(suppl, values)
    }
    x
}

#############################################################
#' Reports whether `x` is layout object
#'
#' @param x An object to test.
#' @return A single boolean value.
#' @examples
#' is_layout(ggheatmap(1:10))
#'
#' @importFrom methods is
#' @export
is_layout <- function(x) is(x, "Layout")

#' @examples
#' # for quad_layout()
#' is_quad_layout(quad_alignb(1:10))
#' is_quad_layout(quad_alignh(1:10))
#' is_quad_layout(quad_alignv(1:10))
#' is_quad_layout(quad_free(mtcars))
#'
#' @export
#' @rdname is_layout
is_quad_layout <- function(x) is(x, "QuadLayout")

#' @examples
#' # for stack_layout()
#' is_stack_layout(stack_align(1:10))
#' is_stack_layout(stack_free(1:10))
#'
#' @export
#' @rdname is_layout
is_stack_layout <- function(x) is(x, "StackLayout")

#' @examples
#' # for heatmap_layout()
#' is_heatmap_layout(ggheatmap(1:10))
#' @export
#' @rdname is_layout
is_heatmap_layout <- function(x) is(x, "HeatmapLayout")

#' @examples
#' is_ggheatmap(ggheatmap(1:10))
#' @export
#' @rdname is_layout
is_ggheatmap <- is_heatmap_layout
