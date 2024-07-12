#' Add components to ggheat
#'
#' @name ggheat-add
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

#' Add custom objects to ggheatmap
#'
#' @param plot A `ggheatmap` object
#' @inheritParams ggplot2::ggplot_add
#' @return A modified `ggheatmap` object.
#' @export
ggheatmap_add <- function(object, plot, object_name) UseMethod("ggheatmap_add")

#' @export
ggheatmap_add.default <- function(object, plot, object_name) {
    cli::cli_abort(
        "Can't add {.var {object_name}} to a {.cls ggheatmap} object."
    )
}

#' @export
ggheatmap_add.NULL <- function(object, plot, object_name) plot

#' @importFrom methods slot slot<-
#' @export
ggheatmap_add.gg <- function(object, plot, object_name) {
    # if no active context, we directly add it into the main heatmap
    if (is.null(pos <- active(plot))) {
        plot@heatmap <- ggplot2::ggplot_add(
            object, plot@heatmap, object_name
        )
        # we check if annotation has been initialized
    } else if (is.null(annotations <- slot(plot, pos))) {
        cli::cli_abort(c(
            "Cannot add {object_name} to {pos} annotation",
            i = "Did you forget to initialize it with {.fn gganno_{pos}}?"
        ))
    } else {
        # add elements to the active context
        annotations[[active(annotations)]]$annotation <- ggplot2::ggplot_add(
            object,
            slot(.subset2(annotations, active(annotations)), "annotation"),
            object_name
        )
        plot[[pos]] <- annotations
    }
    plot
}

#' @export
ggheatmap_add.ggannotation <- function(object, plot, object_name) {
    position <- slot(object, "position")

    # initialize annotation data -------------------------
    anno <- slot(object, "annotation")
    anno$data <- gganno_setup_data(
        .subset2(object, "data"),
        position = position,
        heatmap_matrix = plot@matrix,
        object_name = object_name
    )

    # add annotation -------------------------------------
    annotations <- slot(plot, position) %||% list()
    anno <- list(list(
        annotation = anno,
        order = slot(object, "order"),
        size = slot(object, "size")
    ))

    # check annotation name is unique
    if (!is.null(name <- slot(object, "name"))) {
        if (any(rlang::names2(annotations) == name)) {
            cli::cli_warn(paste(
                "{name} annotation is already present",
                "in the {position} annotation of the heatmap"
            ))
            annotations[[name]] <- NULL
        }
        names(object) <- name
    }
    slot(plot, position) <- structure(
        c(annotations, anno),
        # always change the active annotation
        active = length(annotations) + 1L
    )
    # setting active position for the plot
    if (active(object)) active(plot) <- position
    plot
}
