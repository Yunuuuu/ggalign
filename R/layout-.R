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
        active = "ANY",
        action = "ANY", # used to control the default plot behaviour
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

###########################################################
default_layout <- function(layout) {
    # we always remove panel border from the default theme
    layout@theme <- default_theme() +
        theme(panel.border = element_blank()) +
        layout@theme
    # we by default, collect all guides
    layout@action["guides"] <- list(
        .subset2(layout@action, "guides") %|w|% "tlbr"
    )
    layout
}

update_layout_annotation <- function(object, layout, object_name) {
    layout@annotation <- update_non_waive(
        layout@annotation, .subset2(object, "annotation")
    )
    layout@theme <- update_theme(layout@theme, .subset2(object, "theme"))
    layout
}

update_plot_annotation <- function(object, layout, object_name) {
    layout@titles <- update_non_waive(
        layout@titles, .subset(object, names(layout_title()))
    )
    layout@theme <- update_theme(layout@theme, .subset2(object, "theme"))
    layout
}

######################################################################
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
update_layout_params <- function(x, ..., params) {
    UseMethod("update_layout_params")
}

#' @importFrom methods slot slot<-
#' @export
update_layout_params.QuadLayout <- function(x, direction, ..., params) {
    slot(x, direction) <- params
    if (is_horizontal(direction)) {
        if (!is.null(left <- x@left)) {
            x@left <- update_layout_params(left, params = params)
        }
        if (!is.null(right <- x@right)) {
            x@right <- update_layout_params(right, params = params)
        }
    } else {
        if (!is.null(top <- x@top)) {
            x@top <- update_layout_params(top, params = params)
        }
        if (!is.null(bottom <- x@bottom)) {
            x@bottom <- update_layout_params(bottom, params = params)
        }
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
update_layout_params.StackLayout <- function(x, ..., params) {
    slot(x, "layout") <- params
    x@plots <- lapply(x@plots, function(plot) {
        if (is_layout(plot)) {
            update_layout_params(plot, direction = x@direction, params = params)
        } else {
            plot
        }
    })
    x
}

############################################################
#' Plot context related actions
#'
#' @param order An integer number specifying the order of the plot area in the
#' layout.
#' @param active A single boolean value; if `TRUE`, sets the active context to
#'   the current plot when added to a layout, so all subsequent `ggplot`
#'   elements are added to this plot.
#' @param name A single string specifying the plot name, used to switch active
#'   contexts in `what` argument of [`quad_anno()`]/[`stack_switch()`].
#' @export
plot_context <- function(order = waiver(), active = waiver(), name = waiver()) {
    if (!is.waive(order)) order <- check_order(order)
    if (!is.waive(active)) assert_bool(active)
    if (!is.waive(name)) {
        assert_string(name, empty_ok = FALSE, na_ok = TRUE, null_ok = FALSE)
    }
    new_context(order, active, name)
}

new_context <- function(order, active, name) {
    structure(
        list(order = order, active = active, name = name),
        class = "plot_context"
    )
}

#' @importFrom utils modifyList
update_context <- function(context, default) {
    if (is.null(context)) return(default) # styler: off
    modifyList(default,
        context[!vapply(context, is.waive, logical(1L), USE.NAMES = FALSE)],
        keep.null = TRUE
    )
}

deprecate_context <- function(context, fun,
                              set_context = deprecated(),
                              order = deprecated(), name = deprecated(),
                              call = caller_call()) {
    if (lifecycle::is_present(set_context)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(set_context)", fun),
            sprintf("%s(context)", fun)
        )
        assert_bool(set_context, call = call)
        context["active"] <- list(set_context)
    }
    if (lifecycle::is_present(order)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(order)", fun),
            sprintf("%s(context)", fun)
        )
        order <- check_order(order, call = call)
        context["order"] <- list(order)
    }
    if (lifecycle::is_present(name)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(name)", fun),
            sprintf("%s(context)", fun)
        )
        assert_string(name, empty_ok = FALSE, na_ok = TRUE, call = call)
        context["name"] <- list(name)
    }
    context
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
ggalign_stat.Align <- function(x, ...) .subset2(x, "statistics")

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
#' information for input data, making it useful for transforming parent layout
#' data within a `data` function.
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
