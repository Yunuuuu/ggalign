add_anno <- function(object, plot, object_name) {
    UseMethod("add_anno")
}

#' @export
add_anno.default <- function(object, plot, object_name) {
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
add_anno.htanno <- function(object, plot, object_name) {
    data <- slot(object, "data")
    position <- slot(object, "position")
    axis <- to_axis(position)
    old_panels <- slot(plot, paste0(axis, "_panels"))
    old_index <- slot(plot, paste0(axis, "_index"))

    # initialize annotation of htanno --------------------
    htanno <- slot(object, "htanno")

    # prepare and check parameters ----------------------
    params <- htanno$setup_params(data, position, slot(object, "params"))
    # slot(object, "params") <- params # Not used anymore

    # split parameters -----------------------------------
    htanno$split_params(params)

    # compute statistics ---------------------------------
    statistics <- rlang::inject(
        htanno$compute(data, position, !!!htanno$compute_params)
    )
    slot(object, "statistics") <- statistics

    # group row or column into panels ---------------------
    layout <- htanno_layout(
        htanno, old_panels, old_index,
        data, statistics, position, axis
    )
    slot(plot, paste0(axis, "_panels")) <- .subset2(layout, 1L)
    slot(plot, paste0(axis, "_index")) <- .subset2(layout, 2L)

    # add annotation -------------------------------------
    slot(object, "htanno") <- htanno
    NextMethod()
}

htanno_layout <- function(htanno, old_panels, old_index,
                          data, statistics, position, axis) {
    new <- rlang::inject(
        htanno$layout(
            data, statistics, old_panels, old_index, position,
            !!!htanno$layout_params
        )
    )
    new_panels <- .subset2(new, 1L)
    if (!is.null(new_panels)) {
        if (anyNA(new_panels)) {
            cli::cli_abort(
                "{.fn {snake_class(htanno)}}: find `NA` in layout panels"
            )
        } else if (!is.atomic(new_panels)) {
            cli::cli_abort(c(
                "{.fn {snake_class(htanno)}}: invalid layout panels",
                i = "layout panels must be an atomic vector"
            ))
        } else if (length(new_panels) != nrow(data)) {
            cli::cli_abort(paste(
                "{.fn layout} panels of {.fn {snake_class(htanno)}}",
                "is not compatible with heatmap {axis}"
            ))
        }
    }
    new_index <- .subset2(new, 2L)
    if (!is.null(new_index)) {
        if (anyNA(new_index)) {
            cli::cli_abort(
                "{.fn {snake_class(htanno)}}: find `NA` in layout index"
            )
        } else if (!is.integer(new_index)) {
            cli::cli_abort(c(
                "{.fn {snake_class(htanno)}}: invalid layout index",
                i = "layout index must be integer"
            ))
        } else if (length(new_index) != nrow(data)) {
            cli::cli_abort(paste(
                "layout index of {.fn {snake_class(htanno)}}",
                "is not compatible with heatmap {axis}"
            ))
        }
    }
    # the panel factor level determine the group order
    if (!is.null(new_panels) && !is.factor(new_panels)) {
        new_panels <- factor(new_panels)
    }
    if (!is.null(new_panels) && !is.null(new_index)) {
        new_index <- reorder_index(new_panels, new_index)
    }
    list(new_panels, new_index)
}

reorder_index <- function(panels, index) {
    unlist(split(index, panels[index]), recursive = FALSE, use.names = FALSE)
}
