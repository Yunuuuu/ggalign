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
        cli_abort("Cannot find {what} plot in this stack layout")
    }
    ggalign_stat(x = ans, ...)
}

#' @export
ggalign_stat.ggalign_align <- function(x, ...) {
    .subset2(.subset2(x, "Object"), "statistics")
}

#' @export
ggalign_stat.default <- function(x, ...) {
    cli_abort("no statistics found for {.obj_type_friendly {x}}")
}

####################################################
#' Get Data from the Attached Attribute in the Rendering Process
#'
#' @description
#' `ggalign_attr` provides access to supplementary information stored as
#' attributes during the layout rendering process in `r rd_layout()`. These
#' attributes, commonly attached during data transformation by functions like
#' [`fortify_matrix()`] or [`fortify_data_frame()`], can include essential
#' details such as filtered or supplementary data that inform downstream
#' operations.
#'
#' @details
#' Attributes attached to the data are particularly useful when input data is
#' transformed in ways that restrict access to the complete dataset. For
#' instance, [`fortify_matrix.MAF()`] might filter mutation data while adding
#' attributes that retain essential context, such as the total number of
#' observations, for detailed or aggregated analyses.
#'
#' @param x Data used, typically inherited from the layout `r rd_layout()`.
#' @param field A string specifying the particular data to retrieve from the
#' attached attribute. If `NULL`, the entire attached attribute list will be
#' returned.
#'
#' @return The specified data from the attached attribute or `NULL` if it is
#' unavailable.
#'
#' @export
ggalign_attr <- function(x, field = NULL) {
    if (is.null(x <- ggalign_attr_get(x)) || is.null(field)) {
        return(x)
    }
    .subset2(x, field)
}

#' Set or get the Attached Attribute across the Rendering Process
#'
#' @description
#' - `ggalign_attr_set`: Attaches supplementary data to the input, facilitating
#'   downstream use.
#' - `ggalign_attr_get`: Extracts previously attached supplementary data during
#'   the transformation process.
#'
#' @param x Input data for the layout.
#' @param values A list to be attached.
#' @inherit ggalign_attr details
#' @seealso [`ggalign_attr()`]
#' @export
ggalign_attr_set <- function(x, values) {
    attr(x, ".__ggalign_data__") <- values
    x
}

#' @export
#' @rdname ggalign_attr_set
ggalign_attr_get <- function(x) attr(x, ".__ggalign_data__", exact = TRUE)

ggalign_attr_remove <- function(x) ggalign_attr_set(x, NULL)

# we keep a special attribute across all data
# this is used to pass additional annotation informations
ggalign_attr_restore <- function(data, original) {
    if (is.null(ggalign_attr_get(data)) && # no attached attribute
        # the original has attached attribute
        !is.null(ggalign_data <- ggalign_attr_get(original))) {
        ggalign_attr_set(data, ggalign_data)
    } else {
        data
    }
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
