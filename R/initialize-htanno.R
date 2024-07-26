#' object is an environment, it won't be copied, and will modify in place
#' @noRd
initialize_htanno <- function(object, heatmap, object_name) {
    object <- initialize_htanno_data(object, heatmap, object_name)
    initialize_htanno_layout(object, heatmap, object_name)
}

initialize_htanno_data <- function(object, heatmap, object_name) {
    position <- .subset2(object, "position")
    axis <- to_matrix_axis(position)
    # prepare data -------------------------------
    data <- htanno_setup_data(
        .subset2(object, "data"),
        position = position,
        heatmap_matrix = slot(heatmap, "matrix"),
        object_name = object_name,
        call = .subset2(object, "call")
    )
    object$data <- data

    # setup labels and labels nudge --------------
    labels <- set_labels(
        .subset2(object, "labels"),
        axis,
        rownames(data),
        nrow(data),
        arg = "labels",
        call = .subset2(object, "call")
    )
    object$labels <- labels

    # if waiver, will inherit from the heatmap if labels exist
    labels_nudge <- .subset2(object, "labels_nudge")
    if (is.numeric(labels_nudge)) {
        if (!is_scalar(labels_nudge) && length(labels_nudge) != nrow(data)) {
            cli::cli_abort(paste(
                "{.arg labels_nudge} must be of length 1 or",
                "the same length of heatmap {axis}"
            ), call = .subset2(object, "call"))
        }
        labels_nudge <- rep_len(labels_nudge, nrow(data))
    } else if (is.waiver(labels_nudge)) {
        if (is.null(labels)) {
            labels_nudge <- NULL
        } else {
            labels_nudge <- .subset2(
                slot(heatmap, "params"),
                paste0(axis, "labels_nudge")
            )
        }
    } else if (!is.null(labels_nudge)) {
        cli::cli_abort(
            "{.arg labels_nudge} must be `waiver()`, `NULL` or a numeric",
            call = .subset2(object, "call")
        )
    }
    object$labels_nudge <- labels_nudge
    object
}

initialize_htanno_layout <- function(object, heatmap, object_name) {
    axis <- to_matrix_axis(.subset2(object, "position"))

    # prepare and check parameters ----------------------
    call <- .subset2(object, "call")
    params <- object$setup_params()
    object$params <- params

    # rows are observations, this data came from `htanno_setup_data`, which can
    # be used to check the axis dimention
    data <- .subset2(object, "data")

    # set up data --------------------------------------
    object$data <- object$setup_data()

    # prepare old layout --------------------------------
    old_panels <- slot(heatmap, paste0(axis, "_panels"))
    old_index <- slot(heatmap, paste0(axis, "_index"))

    # compute statistics ---------------------------------
    compute_params <- params[
        intersect(names(params), align_method_params(object$compute))
    ]
    object$statistics <- rlang::inject(
        object$compute(old_panels, old_index, !!!compute_params)
    )

    # make the new layout -------------------------------
    layout_params <- params[
        intersect(names(params), align_method_params(object$layout))
    ]
    layout <- rlang::inject(
        object$layout(old_panels, old_index, !!!layout_params)
    )

    new_panels <- .subset2(layout, 1L)
    if (!is.null(new_panels)) {
        if (anyNA(new_panels)) {
            cli::cli_abort(
                "{.fn {snake_class(object)}}: find `NA` in layout panels",
                call = call
            )
        } else if (!is.atomic(new_panels)) {
            cli::cli_abort(
                c(
                    "{.fn {snake_class(object)}}: invalid layout panels",
                    i = "layout panels must be an atomic vector"
                ),
                call = call
            )
        } else if (length(new_panels) != nrow(data)) {
            cli::cli_abort(
                paste(
                    "{.fn layout} panels of {.fn {snake_class(object)}}",
                    "is not compatible with heatmap {axis}"
                ),
                call = call
            )
        }
    }

    # the panel factor level determine the group order
    if (!is.null(new_panels) && !is.factor(new_panels)) {
        new_panels <- factor(new_panels)
    }

    new_index <- .subset2(layout, 2L)
    if (!is.null(new_index)) {
        if (anyNA(new_index)) {
            cli::cli_abort(
                "{.fn {snake_class(object)}}: find `NA` in layout index",
                call = call
            )
        } else if (!is.integer(new_index)) {
            cli::cli_abort(
                c(
                    "{.fn {snake_class(object)}}: invalid layout index",
                    i = "layout index must be integer"
                ),
                call = call
            )
        } else if (length(new_index) != nrow(data)) {
            cli::cli_abort(
                paste(
                    "layout index of {.fn {snake_class(object)}}",
                    "is not compatible with heatmap {axis}"
                ),
                call = call
            )
        }
    }

    # we always make the index following the panel
    if (!is.null(new_panels) && !is.null(new_index)) {
        new_index <- reorder_index(new_panels, new_index)
    }

    # we always prevent from reordering heatmap twice.
    # in this way, htanno even cannot make groups after creating heatmap order
    if (!is.null(old_index)) {
        if (!all(old_index == new_index)) {
            cli::cli_abort(sprintf(
                "{.fn {snake_class(object)}} disrupt the previously %s %s",
                "established order of the heatmap",
                axis
            ), call = call)
        }
    }

    # in the finally, Let us initialize the annotation plot -----
    # must return a ggplot object
    ggplot_params <- params[
        intersect(names(params), align_method_params(object$ggplot))
    ]
    p <- rlang::inject(object$ggplot(!!!ggplot_params))
    # set the default theme for all annotation
    if (ggplot2::is.ggplot(p)) {
        p <- p + theme(
            plot.background = element_blank(),
            panel.border = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
    }
    object$plot <- p

    # add annotation -------------------------------------
    list(new_panels, new_index)
}

reorder_index <- function(panels, index = NULL, reverse = FALSE) {
    index <- index %||% seq_along(panels)
    unlist(split(index, panels[index]), recursive = FALSE, use.names = FALSE)
}

####################################################################
#' @keywords internal
htanno_setup_data <- function(data, position, heatmap_matrix,
                              object_name, call) {
    UseMethod("htanno_setup_data")
}

#' @export
htanno_setup_data.data.frame <- function(data, position, heatmap_matrix,
                                         object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name), call = call)
    }
    data
}

#' @export
htanno_setup_data.matrix <- function(data, position, heatmap_matrix,
                                     object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name), call = call)
    }
    data
}

#' @export
htanno_setup_data.NULL <- function(data, position, heatmap_matrix,
                                   object_name, call) {
    anno_data_from_heatmap(position, heatmap_matrix)
}

#' @export
htanno_setup_data.numeric <- function(data, position, heatmap_matrix,
                                      object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name), call = call)
    }
    ans <- matrix(data, ncol = 1L)
    colnames(ans) <- "V1"
    if (rlang::is_named(data)) rownames(ans) <- names(data)
    ans
}

#' @export
htanno_setup_data.character <- htanno_setup_data.numeric

#' @export
htanno_setup_data.function <- function(data, position, heatmap_matrix,
                                       object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    fn <- data
    # fix R CMD check note: Found the following calls to data() loading into the
    # global environment
    data <- fn(anno_matrix)
    htanno_setup_data(data, position, heatmap_matrix, object_name, call = call)
}

anno_data_from_heatmap <- function(position, heatmap_matrix) {
    switch_position(position, heatmap_matrix, t(heatmap_matrix))
}

msg_anno_incompatible_data <- function(object_name) {
    sprintf(
        "%s from %s must have %s",
        style_arg("data"), object_name,
        "compatible observations with the heatmap"
    )
}
