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
            "Cannot add {object_name} to {position} annotation",
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
            cli::cli_abort(
                "No active annotation",
                i = "try to provide {.arg position} argument in {object_name}"
            )
        }
        slot(object, "position") <- position
    }

    # prepare data -------------------------------
    data <- anno_setup_data(
        slot(object, "data"),
        position = position,
        heatmap_matrix = slot(plot, "matrix"),
        object_name = object_name
    )
    slot(object, "data") <- data

    # add annotation -----------------------------
    add_anno(object, plot, object_name)
}

#' @export
ggheatmap_add.active <- function(object, plot, object_name) {
    if (object == "heatmap") {
        set_context(plot, NULL)
    } else {
        set_context(plot, object)
    }
}
