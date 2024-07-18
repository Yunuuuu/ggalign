#' Add components to heatmap and heatmap annotations
#'
#' @param e1 A [ggheatmap][ggheat] object.
#' @param e2 An object to be added to the plot.
#' @return A modified `ggheatmap` object.
#' @name ggheatmap-and
#' @aliases &.ggheatmap
#' @seealso ggheatmap_and_add
NULL

#' @include ggheat.R
#' @rdname ggheatmap-and
#' @export
methods::setMethod("&", c("ggheatmap", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    ggheatmap_and_add(e2, e1, e2name)
})

# we use `ggheatmap_and_add` instead of `ggheatmap_and` since `ggheatmap_and` is
# too similar with `ggheatmap_and` in the name.
#' Add custom objects to `ggheatmap`
#'
#' @param plot A `ggheatmap` object
#' @inheritParams ggplot2::ggplot_add
#' @inherit ggheatmap-and return
#' @export
ggheatmap_and_add <- function(object, plot, object_name) {
    UseMethod("ggheatmap_and_add")
}

#' @export
ggheatmap_and_add.gg <- function(object, plot, object_name) {
    plot <- heatmap_add(object, plot, object_name)
    for (position in GGHEAT_ELEMENTS) {
        annotations <- slot(plot, position)
        if (is.null(annotations)) next
        context <- get_context(annotations)
        annotations <- lapply(annotations, function(annotation) {
            anno_and_add(annotation, object, object_name)
        })
        slot(plot, position) <- new_annotations(annotations, context)
    }
    plot
}

#' @export
ggheatmap_and_add.labels <- ggheatmap_and_add.gg

#' @export
ggheatmap_and_add.facetted_pos_scales <- ggheatmap_and_add.gg

#' @export
ggheatmap_and_add.NULL <- ggheatmap_add.NULL
