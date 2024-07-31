#' Add components to heatmap and heatmap annotations
#'
#' @param e1 A [ggheatmap][ggheat] object.
#' @param e2 An object to be added to the plot.
#' @return A modified `ggheatmap` object.
#' @name ggheatmap-and
#' @aliases &.ggheatmap
#' @seealso ggheatmap_and_add
NULL

#' @rdname ggheatmap-and
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

# we use `ggheatmap_and_add` instead of `ggheatmap_and` since `ggheatmap_and` is
# too similar with `ggheatmap_and` in the name.
#' Add custom objects to ggheatmap and heatmap annotations
#'
#' @param heatmap A `ggheatmap` object
#' @inheritParams ggplot2::ggplot_add
#' @inherit ggheatmap-and return
#' @noRd
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
    heatmap <- heatmap_add(object, heatmap, object_name)
    for (position in GGHEAT_ELEMENTS) {
        stack <- slot(heatmap, position)
        if (is.null(stack)) next
        slot(stack, "plots") <- lapply(
            slot(stack, "plots"),
            function(plot) {
                if (is.null(.subset2(plot, "plot"))) {
                    return(plot)
                }
                align_add(object, plot, object_name)
            }
        )
        slot(heatmap, position) <- stack
    }
    heatmap
}

#' @export
layout_heatmap_and_add.labels <- layout_heatmap_and_add.gg

#' @export
layout_heatmap_and_add.facetted_pos_scales <- layout_heatmap_and_add.gg

#' @export
layout_heatmap_and_add.NULL <- layout_heatmap_add.NULL
