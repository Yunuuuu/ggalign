initialize_anno <- function(object, plot, object_name) {
    UseMethod("initialize_anno")
}

#' @export
initialize_anno.default <- function(object, plot, object_name) {
    position <- slot(object, "position")

    # setting active position for the plot ---------------
    set_context <- slot(object, "set_context")
    if (.subset(set_context, 1L)) plot <- set_context(plot, position)

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
    if (.subset(set_context, 2L)) {
        anno_active <- length(annotations) + 1L
    } else {
        anno_active <- get_context(annotations)
    }
    slot(plot, position) <- structure(
        c(annotations, new),
        active = anno_active,
        class = c("ggAnnotationList", "ggheat")
    )
    plot
}

#' @export
initialize_anno.htanno <- function(object, plot, object_name) {
    data <- slot(object, "data")
    position <- slot(object, "position")
    axis <- to_axis(position)

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

    # group row or column into panels ---------------------
    panels <- htanno_split(htanno, plot, data, statistics, position)
    # the panel factor level determine the group order
    if (!is.null(panels)) panels <- factor(panels)
    slot(plot, paste0(axis, "_panels")) <- panels

    # reorder heatmap ------------------------------------
    # the index only determine the order for each group
    new_index <- htanno_reorder(htanno, plot, data, statistics, position)
    slot(plot, paste0(axis, "_index")) <- new_index

    # add annotation -------------------------------------
    slot(object, "htanno") <- htanno
    NextMethod()
}

htanno_reorder <- function(htanno, plot,
                           data, statistics, position) {
    axis <- to_axis(position)
    panels <- slot(plot, paste0(axis, "_panels"))
    old_index <- slot(plot, paste0(axis, "_index"))
    new_index <- rlang::inject(
        htanno$reorder(
            data, statistics, panels, position,
            !!!htanno$reorder_params
        )
    )
    if (!is.null(new_index)) {
        if (!is.integer(new_index)) {
            cli::cli_abort(paste(
                "{.fn reorder} of {.cls {snake_class(htanno)}}",
                "must return an integer index"
            ))
        } else if (length(new_index) != nrow(data)) {
            cli::cli_abort(paste(
                "{.fn reorder} of {.cls {snake_class(htanno)}}",
                "is not compatible with heatmap {axis}"
            ))
        }
    }
    if (!is.null(old_index) &&
        !is.null(new_index) &&
        !all(old_index == new_index)) {
        cli::cli_abort("Cannot reorder heatmap {axis} twice")
    }
    new_index
}

htanno_split <- function(htanno, plot, data, statistics, position) {
    axis <- to_axis(position)
    old_panels <- slot(plot, paste0(axis, "_panels"))
    new_panels <- rlang::inject(
        htanno$make_panels(
            data, statistics, old_panels, position,
            !!!htanno$panel_params
        )
    )
    if (!is.null(new_panels)) {
        if (!is.atomic(new_panels)) {
            cli::cli_abort(paste(
                "{.fn make_panels} of {.cls {snake_class(htanno)}}",
                "must return an atomic vector"
            ))
        } else if (length(new_panels) != nrow(data)) {
            cli::cli_abort(paste(
                "{.fn make_panels} of {.cls {snake_class(htanno)}}",
                "is not compatible with heatmap {axis}"
            ))
        }
    }

    if (!is.null(old_panels) &&
        (is.null(new_panels) ||
            !all(lengths(lapply(split(old_panels, new_panels), unique)) == 1L))) {
        cli::cli_abort(paste(
            "{.fn make_panels} of {.cls {snake_class(htanno)}} must",
            "respect the old group"
        ))
    }
    new_panels
}
