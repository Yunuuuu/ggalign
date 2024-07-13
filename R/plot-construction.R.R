#' Add components to `ggheatmap`
#'
#' @param e1 A [ggheatmap][ggheat] object.
#' @param e2 An object to be added to the plot, including
#' [gg][ggplot2::+.gg] elements, [ggannotation][gganno] object, or [htanno]
#' object.
#' @return A modified `ggheatmap` object.
#' @name ggheatmap-add
#' @aliases +.ggheatmap
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
    } else if (is.null(annotations <- slot(plot, pos)) ||
        is.null(active_anno <- active(annotations))) {
        cli::cli_abort(c(
            "Cannot add {object_name} to {pos} annotation",
            i = "Did you forget to initialize it with {.fn gganno_{pos}}?"
        ))
    } else {
        anno <- .subset2(annotations, active_anno)
        annotations[[active_anno]] <- anno_add_gg(anno, object, object_name)
        slot(plot, pos) <- annotations
    }
    plot
}

#' @export
ggheatmap_add.ggannotation <- function(object, plot, object_name) {
    position <- slot(object, "position")
    if (is.null(position) && is.null(position <- active(plot))) {
        cli::cli_abort(
            "No active annotation",
            i = "try to provide {.arg position} argument in {object_name}"
        )
    }
    # prepare initial data -------------------------------
    data <- anno_setup_data(
        slot(object, "data"),
        position = position,
        heatmap_matrix = slot(plot, "matrix"),
        object_name = object_name
    )
    slot(object, "data") <- data
    ggheatmap_add_anno(object, plot, object_name)
}

#' @export
ggheatmap_add.htanno <- function(object, plot, object_name) {
    position <- slot(object, "position")
    if (is.null(position) && is.null(position <- active(plot))) {
        cli::cli_abort(
            "No active annotation",
            i = "try to provide {.arg position} argument in {object_name}"
        )
    }

    # prepare initial data -------------------------------
    data <- anno_setup_data(
        slot(object, "data"),
        position = position,
        heatmap_matrix = slot(plot, "matrix"),
        object_name = object_name
    )
    slot(object, "data") <- data

    # initialize annotation of htanno --------------------
    htanno <- slot(object, "htanno")

    # prepare and check parameters ----------------------
    params <- htanno$setup_params(data, position, slot(object, "params"))
    slot(object, "params") <- params

    # split parameters -----------------------------------
    htanno$split_params(params)

    # compute statistics ---------------------------------
    statistics <- rlang::inject(
        htanno$compute(data, position, !!!htanno$compute_params)
    )
    slot(object, "statistics") <- statistics

    # group row or column into slice ---------------------
    axis <- to_axis(position)
    old_slice <- slot(plot, paste0(axis, "_slice"))
    new_slice <- rlang::inject(
        htanno$make_slice(
            data, position, statistics, old_slice,
            !!!htanno$slice_params
        )
    )
    if (!is.null(new_slice)) {
        if (!is.atomic(new_slice)) {
            cli::cli_abort(paste(
                "{.fn make_slice} of {.cls {snake_class(annotation)}}",
                "must return an atomic vector"
            ))
        } else if (length(new_slice) != nrow(data)) {
            cli::cli_abort(paste(
                "{.fn make_slice} of {.cls {snake_class(annotation)}}",
                "is not compatible with heatmap {axis}"
            ))
        }
    }

    if (!is.null(old_slice) &&
        (is.null(new_order) ||
            !all(lengths(lapply(split(old_slice, new_slice), unique)) == 1L))) {
        cli::cli_abort(paste(
            "{.fn make_slice} of {.cls {snake_class(annotation)}} must",
            "respect the old group"
        ))
    }
    slot(plot, paste0(axis, "_slice")) <- new_slice

    # reorder heatmap ------------------------------------
    old_order <- slot(plot, paste0(axis, "_order"))
    new_order <- rlang::inject(
        htanno$reorder(
            data, position, statistics, new_slice,
            !!!htanno$reorder_params
        )
    )
    if (!is.null(new_order)) {
        if (!is.integer(new_order)) {
            cli::cli_abort(paste(
                "{.fn reorder} of {.cls {snake_class(annotation)}}",
                "must return an integer index"
            ))
        } else if (length(new_order) != nrow(data)) {
            cli::cli_abort(paste(
                "{.fn reorder} of {.cls {snake_class(annotation)}}",
                "is not compatible with heatmap {axis}"
            ))
        }
    }
    if (!is.null(old_order) &&
        !is.null(new_order) &&
        !all(old_order == new_order)) {
        cli::cli_abort("Cannot reorder heatmap {axis} twice")
    }
    slot(plot, paste0(axis, "_order")) <- new_order

    # add annotation -------------------------------------
    slot(object, "htanno") <- htanno
    ggheatmap_add_anno(object, plot, object_name)
}

ggheatmap_add_anno <- function(object, plot, object_name) {
    position <- slot(object, "position")

    # setting active position for the plot ---------------
    active <- rep_len(slot(object, "active"), 2L)
    if (.subset(active, 1L)) active(plot) <- position

    # add annotation -------------------------------------
    annotations <- slot(plot, position) %||% list()
    new <- list(object)

    # check annotation name is unique
    if (!is.null(name <- slot(object, "name"))) {
        if (any(rlang::names2(annotations) == name)) {
            cli::cli_warn(paste(
                "{object_name}: {name} annotation is already present",
                "in the {position} annotation of the heatmap"
            ))
            annotations[[name]] <- NULL
        }
        names(new) <- name
    }
    if (.subset(active, 2L)) {
        active <- length(annotations) + 1L
    } else {
        active <- active(annotations)
    }
    slot(plot, position) <- ggAnnotationList(c(annotations, new), active)
    plot
}

ggAnnotationList <- function(list, active = length(list)) {
    structure(list, active = active, class = c("ggAnnotationList", "ggheat"))
}
