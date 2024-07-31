#' Add components to all stack plots
#'
#' @param e1 A [Layoutstack][layout_stack] object.
#' @param e2 An object to be added to the plot.
#' @inherit stack-add return
#' @name stack-and
#' @aliases &.Layoutstack &.ggstack
#' @seealso [layout_stack_and_add]
NULL

#' @rdname stack-and
#' @export
methods::setMethod("&", c("LayoutStack", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_stack_and_add(e2, e1, e2name)
})

# we use `layout_stack_and_add` instead of `layout_stack_and` since
# `layout_stack_and` is too similar with `layout_stack_and` in the name.
#' Add custom objects to all stack plots
#'
#' @param stack A [Layoutstack][layout_stack] object
#' @inheritParams ggplot2::ggplot_add
#' @inherit stack-add return
#' @export
layout_stack_and_add <- function(object, stack, object_name) {
    UseMethod("layout_stack_and_add")
}

#' @export
layout_stack_and_add.default <- function(object, stack, object_name) {
    cli::cli_abort(
        "Cannot add {.code {object_name}} to stack and annotations"
    )
}

#' @export
layout_stack_and_add.gg <- function(object, stack, object_name) {
    slot(stack, "plots") <- lapply(
        slot(stack, "plots"),
        function(plot) {
            if (is.ggheatmap(plot)) {
                plot <- layout_heatmap_and_add(object, plot, object_name)
            } else if (!is.null(.subset2(plot, "plot"))) {
                # if `align` has plot, we added the object
                plot <- align_add(object, plot, object_name)
            }
            plot
        }
    )
    stack
}

#' @export
layout_stack_and_add.labels <- layout_stack_and_add.gg

#' @export
layout_stack_and_add.facetted_pos_scales <- layout_stack_and_add.gg

#' @export
layout_stack_and_add.NULL <- layout_stack_add.NULL
