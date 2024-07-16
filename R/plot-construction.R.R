#' Add components to `ggheatmap`
#'
#' @param e1 A [ggheatmap][ggheat] object.
#' @param e2 An object to be added to the plot, including
#' [gg][ggplot2::+.gg] elements, [gganno][gganno] object, or [htanno]
#' object.
#' @return A modified `ggheatmap` object.
#' @name ggheatmap-add
#' @aliases +.ggheatmap
#' @seealso ggheatmap_add
NULL

#' @rdname ggheatmap-add
#' @export
methods::setMethod("+", c("ggheatmap", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    ggheatmap_add(e2, e1, e2name)
})

#' Add custom objects to `ggheatmap`
#'
#' @param plot A `ggheatmap` object
#' @inheritParams ggplot2::ggplot_add
#' @inherit ggheatmap-add return
#' @export
ggheatmap_add <- function(object, plot, object_name) UseMethod("ggheatmap_add")

#' @importFrom methods slot slot<-
#' @export
ggheatmap_add.default <- function(object, plot, object_name) {
    # if no active context, we directly add it into the main heatmap
    if (is.null(position <- get_context(plot))) {
        plot <- heatmap_add(object, plot, object_name)
        # we check if annotation has been initialized
    } else if (is.null(annotations <- slot(plot, position)) ||
        is.null(active_anno <- get_context(annotations))) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to {position} annotation",
            i = "Did you forget to initialize it with {.fn gganno_{position}}?"
        ))
    } else {
        anno <- .subset2(annotations, active_anno)
        annotations[[active_anno]] <- anno_add(anno, object, object_name)
        slot(plot, position) <- annotations
    }
    plot
}

#' @export
ggheatmap_add.NULL <- function(object, plot, object_name) plot

#' @export
ggheatmap_add.anno <- function(object, plot, object_name) {
    position <- slot(object, "position")
    if (is.null(position)) {
        if (is.null(position <- get_context(plot))) {
            cli::cli_abort(c(
                "No active annotation",
                i = paste(
                    "try to provide {.arg position} argument in",
                    "{.code {object_name}}"
                )
            ), call = slot(object, "call"))
        }
        slot(object, "position") <- position
    }

    # setting active position for the plot ---------------
    set_context <- slot(object, "set_context")
    if (.subset(set_context, 1L)) plot <- set_context(plot, position)

    # initialize annotation -----------------------------
    # this step the annotation will act with the heatmap
    # group heatmap into panels or reorder heatmap rows/columns
    ans <- anno_initialize(object, plot, object_name)
    plot <- .subset2(ans, 1L)
    object <- .subset2(ans, 2L)

    # add annotation -----------------------------
    annotations <- slot(plot, position) %||%
        new_annotations(list(), NULL)
    slot(plot, position) <- annotations_add(
        object, annotations, .subset(set_context, 2L),
        object_name
    )
    plot
}

#' @export
ggheatmap_add.active <- function(object, plot, object_name) {
    if (object == "heatmap") {
        set_context(plot, NULL)
    } else {
        set_context(plot, object)
    }
}

#' @export
ggheatmap_add.CoordFlip <- function(object, plot, object_name) {
    cli::cli_abort(
        "Can't add {.var {object_name}} to a {.cls ggheatmap} object"
    )
}
