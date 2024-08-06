#' object is an environment, it won't be copied, and will modify in place
#' @noRd
initialize_align <- function(object, direction,
                             layout_data,
                             layout_panels, layout_index,
                             object_name) {
    object$direction <- direction

    # prepare the data -------------------------------
    input_data <- .subset2(object, "input_data")
    data <- align_setup_data(
        input_data, layout_data,
        object_name = object_name,
        call = .subset2(object, "call")
    )
    object$data_from_layout <- is.null(input_data)
    object$labels <- rownames(data)
    initialize_align_layout(
        object, data, direction,
        layout_panels, layout_index, object_name
    )
}

initialize_align_layout <- function(object, data, direction,
                                    layout_panels, layout_index,
                                    object_name) {
    axis <- to_coord_axis(direction)

    # prepare and check parameters ----------------------
    call <- .subset2(object, "call")
    n <- nrow(data) # number of observations
    params <- .subset2(object, "input_params")
    params <- object$setup_params(data, params)
    object$params <- params

    # set up data --------------------------------------
    object$data <- object$setup_data(data, params)

    # compute statistics ---------------------------------
    compute_params <- params[
        intersect(names(params), align_method_params(object$compute))
    ]
    object$statistics <- rlang::inject(
        object$compute(layout_panels, layout_index, !!!compute_params)
    )

    # make the new layout -------------------------------
    layout_params <- params[
        intersect(names(params), align_method_params(object$layout))
    ]
    layout <- rlang::inject(
        object$layout(layout_panels, layout_index, !!!layout_params)
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
        } else if (length(new_panels) != n) {
            cli::cli_abort(
                paste(
                    "{.fn layout} panels of {.fn {snake_class(object)}}",
                    "is not compatible with {axis}-axis"
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
        } else if (length(new_index) != n) {
            cli::cli_abort(
                paste(
                    "layout index of {.fn {snake_class(object)}}",
                    "is not compatible with {axis}-axis"
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
    if (!is.null(layout_index)) {
        if (!all(layout_index == new_index)) {
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
        intersect(
            names(params),
            align_method_params(object$ggplot, character())
        )
    ]
    p <- rlang::inject(object$ggplot(!!!ggplot_params))
    if (ggplot2::is.ggplot(p)) {
        p <- p + align_theme(direction)
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
align_setup_data <- function(data, layout_data, object_name, call) {
    UseMethod("align_setup_data")
}

#' @export
align_setup_data.data.frame <- function(data, layout_data, object_name, call) {
    if (nrow(layout_data) != NROW(data)) {
        cli::cli_abort(msg_align_incompatible_data(object_name), call = call)
    }
    data
}

#' @export
align_setup_data.matrix <- align_setup_data.data.frame

#' @export
align_setup_data.NULL <- function(data, layout_data, object_name, call) {
    layout_data
}

#' @export
align_setup_data.numeric <- function(data, layout_data, object_name, call) {
    if (nrow(layout_data) != NROW(data)) {
        cli::cli_abort(msg_align_incompatible_data(object_name), call = call)
    }
    as.matrix(data)
}

#' @export
align_setup_data.character <- align_setup_data.numeric

#' @export
align_setup_data.function <- function(data, layout_data, object_name, call) {
    # fix R CMD check note: Found the following calls to data() loading into the
    # global environment
    fn <- data
    data <- fn(layout_data)
    align_setup_data(data, layout_data, object_name, call)
}

msg_align_incompatible_data <- function(object_name) {
    sprintf(
        "%s from %s must have %s",
        style_arg("data"), style_code(object_name),
        "compatible observations with the layout"
    )
}
