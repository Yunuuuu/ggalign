#' Add components to heatmap and heatmap annotations
#'
#' @inherit layout_heatmap_and_add description
#' @param e1 A [LayoutHeatmap][layout_heatmap] object.
#' @param e2 An object to be added to the plot.
#' @inherit heatmap-add return
#' @name heatmap-and
#' @aliases &.LayoutHeatmap &.ggheatmap
#' @seealso [layout_heatmap_and_add]
NULL

#' @rdname heatmap-and
#' @export
methods::setMethod("&", c("LayoutHeatmap", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_heatmap_and_add(e2, e1, e2name)
})

# we use `layout_heatmap_and_add` instead of `layout_heatmap_and` since
# `layout_heatmap_and` is too similar with `layout_heatmap_and` in the name.
#' Add custom objects to heatmap and heatmap annotations
#'
#' If the active context is the heatmap body, this will add the object for
#' heatmap and all annotations. Otherwise, it'll add the object for current
#' active context only.
#'
#' @param heatmap A [LayoutHeatmap][layout_heatmap] object
#' @inheritParams ggplot2::ggplot_add
#' @inherit heatmap-add return
#' @export
layout_heatmap_and_add <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_and_add")
}

#' @export
layout_heatmap_and_add.default <- function(object, heatmap, object_name) {
    cli::cli_abort(
        "Cannot add {.code {object_name}} to heatmap and annotations"
    )
}

#' @export
layout_heatmap_and_add.gg <- function(object, heatmap, object_name) {
    if (is.null(set <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
        set <- GGHEAT_ELEMENTS
    }
    for (position in set) {
        stack <- slot(heatmap, position)
        if (is.null(stack)) next
        slot(heatmap, position) <- layout_stack_and_add(
            object, stack, object_name
        )
    }
    heatmap
}

#' @export
layout_heatmap_and_add.labels <- layout_heatmap_and_add.gg

#' @export
layout_heatmap_and_add.facetted_pos_scales <- layout_heatmap_and_add.gg

#' @export
layout_heatmap_and_add.NULL <- layout_heatmap_add.NULL
