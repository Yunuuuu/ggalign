anno_initialize <- function(object, plot, object_name) {
    UseMethod("anno_initialize")
}

#' @export
anno_initialize.anno <- function(object, plot, object_name) {
    position <- slot(object, "position")
    axis <- to_matrix_axis(position)

    # prepare data -------------------------------
    data <- anno_setup_data(
        slot(object, "data"),
        position = position,
        heatmap_matrix = slot(plot, "matrix"),
        object_name = object_name,
        call = slot(object, "call")
    )
    slot(object, "data") <- data

    # setup labels and labels nudge --------------
    labels <- set_labels(
        slot(object, "labels"),
        axis,
        rownames(data),
        nrow(data),
        arg = "labels",
        call = slot(object, "call")
    )
    slot(object, "labels") <- labels

    # if waiver, will inherit from the heatmap
    labels_nudge <- set_nudge(
        slot(object, "labels_nudge"),
        nrow(data), labels,
        axis
    ) %|w|% .subset2(
        slot(plot, "params"),
        paste0(to_coord_axis(position), "labels_nudge")
    )
    slot(object, "labels_nudge") <- labels_nudge
    list(plot, object)
}

#' @export
anno_initialize.htanno <- function(object, plot, object_name) {
    ans <- NextMethod()
    plot <- .subset2(ans, 1L)
    object <- .subset2(ans, 2L)

    data <- slot(object, "data")
    position <- slot(object, "position")
    axis <- to_matrix_axis(position)
    old_panels <- slot(plot, paste0(axis, "_panels"))
    old_index <- slot(plot, paste0(axis, "_index"))

    # initialize annotation of htanno --------------------
    htanno <- slot(object, "htanno")
    htanno$call <- slot(object, "call") # add call into htanno for message usage

    # prepare and check parameters ----------------------
    params <- htanno$setup_params(data, slot(object, "params"), position)
    # slot(object, "params") <- params # Not used anymore

    # prepare data --------------------------------------
    data <- htanno$setup_data(data, params, position)
    slot(object, "data") <- data

    # split parameters -----------------------------------
    htanno$split_params(params)

    # compute statistics ---------------------------------
    statistics <- rlang::inject(
        htanno$compute(
            data, old_panels, old_index, position,
            !!!htanno$compute_params
        )
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
    list(plot, object)
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
                "{.fn {snake_class(htanno)}}: find `NA` in layout panels",
                call = htanno$call
            )
        } else if (!is.atomic(new_panels)) {
            cli::cli_abort(
                c(
                    "{.fn {snake_class(htanno)}}: invalid layout panels",
                    i = "layout panels must be an atomic vector"
                ),
                call = htanno$call
            )
        } else if (length(new_panels) != nrow(data)) {
            cli::cli_abort(
                paste(
                    "{.fn layout} panels of {.fn {snake_class(htanno)}}",
                    "is not compatible with heatmap {axis}"
                ),
                call = htanno$call
            )
        }
    }
    new_index <- .subset2(new, 2L)
    if (!is.null(new_index)) {
        if (anyNA(new_index)) {
            cli::cli_abort(
                "{.fn {snake_class(htanno)}}: find `NA` in layout index",
                call = htanno$call
            )
        } else if (!is.integer(new_index)) {
            cli::cli_abort(
                c(
                    "{.fn {snake_class(htanno)}}: invalid layout index",
                    i = "layout index must be integer"
                ),
                call = htanno$call
            )
        } else if (length(new_index) != nrow(data)) {
            cli::cli_abort(
                paste(
                    "layout index of {.fn {snake_class(htanno)}}",
                    "is not compatible with heatmap {axis}"
                ),
                call = htanno$call
            )
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

reorder_index <- function(panels, index = NULL, reverse = FALSE) {
    index <- index %||% seq_along(panels)
    ans <- split(index, panels[index])
    unlist(ans, recursive = FALSE, use.names = FALSE)
}
