#' `Align` is an environment, it won't be copied, and will modify in place
#' @noRd
align_initialize <- function(object, direction, position,
                             layout_data, layout_panel, layout_index,
                             layout_nobs, object_name) {
    object$direction <- direction
    object$position <- position
    input_data <- .subset2(object, "input_data")
    input_params <- .subset2(object, "input_params")
    call <- .subset2(object, "call")

    # we must have the same observations across all plots
    # 1. if Align require data, the `nobs` should be nrow(data)
    # 2. if not, we run `nobs()` method to initialize the layout nobs
    if (!is.null(input_data)) { # this `Align` object require data
        if (is.waive(input_data)) { # inherit from the layout
            abort_no_layout_data(layout_data, object_name, call)
            data <- layout_data
        } else {
            if (is.function(input_data)) {
                abort_no_layout_data(layout_data, object_name, call)
                data <- input_data(layout_data)
            } else {
                data <- input_data
            }
            # we always regard rows as the observations
            if (is.null(layout_nobs)) {
                layout_nobs <- NROW(data)
            } else if (NROW(data) != layout_nobs) {
                cli::cli_abort(paste(
                    "{.arg data} must have compatible",
                    "observations with the layout"
                ), call = call)
            }
        }
        object$labels <- vec_names(data)
        params <- object$setup_params(layout_nobs, input_params)
        object$data <- restore_attr_ggalign(
            object$setup_data(params, data),
            layout_data
        )
    } else { # this `Align` object doesn't require any data
        # If `nobs` is `NULL`, it means we don't initialize the layout
        # observations, we initialize `nobs` with the `Align` obect
        if (is.null(layout_nobs)) layout_nobs <- object$nobs(input_params)
        params <- object$setup_params(layout_nobs, input_params)
    }
    # save the parameters into the object ------------
    object$params <- params

    # prepare the data -------------------------------
    ans <- align_initialize_layout(
        object, layout_nobs, direction,
        layout_panel, layout_index, object_name
    )
    c(vec_set_names(ans, c("panel", "index")), list(nobs = layout_nobs))
}

#' @importFrom rlang inject
align_initialize_layout <- function(object, layout_nobs, direction,
                                    layout_panel, layout_index,
                                    object_name) {
    axis <- to_coord_axis(direction)
    call <- .subset2(object, "call")
    params <- .subset2(object, "params")

    # compute statistics ---------------------------------
    compute_params <- params[
        intersect(names(params), align_method_params(object$compute))
    ]
    object$statistics <- inject(
        object$compute(layout_panel, layout_index, !!!compute_params)
    )

    # make the new layout -------------------------------
    layout_params <- params[
        intersect(names(params), align_method_params(object$layout))
    ]
    layout <- inject(
        object$layout(layout_panel, layout_index, !!!layout_params)
    )
    new_panel <- .subset2(layout, 1L)
    if (!is.null(new_panel)) {
        if (anyNA(new_panel)) {
            cli::cli_abort(
                "{.fn {snake_class(object)}}: find `NA` in layout panel",
                call = call
            )
        } else if (!is.atomic(new_panel)) {
            cli::cli_abort(c(
                "{.fn {snake_class(object)}}: invalid layout panel",
                i = "layout panel must be an atomic vector"
            ), call = call)
        } else if (is.null(layout_nobs) || length(new_panel) != layout_nobs) {
            cli::cli_abort(paste(
                "layout panel of {.fn {snake_class(object)}}",
                "is not compatible with {axis}-axis"
            ), call = call)
        } else if (!is.null(layout_panel) && !(new_panel %nest% layout_panel)) {
            cli::cli_abort(paste0(
                "{.fn {snake_class(object)}} disrupt the previously ",
                "established panel groups of the layout {axis}-axis"
            ), call = call)
        }
    }

    # the panel factor level determine the group order
    if (!is.null(new_panel) && !is.factor(new_panel)) {
        new_panel <- factor(new_panel)
    }

    new_index <- .subset2(layout, 2L)
    if (!is.null(new_index)) {
        if (anyNA(new_index)) {
            cli::cli_abort(
                "{.fn {snake_class(object)}}: find `NA` in layout index",
                call = call
            )
        } else if (!is.integer(new_index)) {
            cli::cli_abort(c(
                "{.fn {snake_class(object)}}: invalid layout index",
                i = "layout index must be integer"
            ), call = call)
        } else if (is.null(layout_nobs) || length(new_index) != layout_nobs) {
            cli::cli_abort(paste(
                "layout index of {.fn {snake_class(object)}}",
                "is not compatible with {axis}-axis"
            ), call = call)
        }
    }

    # we always make the index following the panel
    # if the `layout_index` is not `NULL` and we create a new group,
    # this will also ensure the `new_index` is not the same with `layout_index`
    # which finally cause error.
    if (!is.null(new_panel) && !is.null(new_index)) {
        new_index <- reorder_index(new_panel, new_index)
    }

    # we always prevent from reordering layout twice.
    if (!is.null(layout_index) && !all(layout_index == new_index)) {
        cli::cli_abort(paste0(
            "{.fn {snake_class(object)}} disrupt the previously ",
            "established ordered index of the layout ",
            axis, "-axis"
        ), call = call)
    }

    # in the finally, Let us initialize the annotation plot -----
    # must return a ggplot object
    ggplot_params <- params[
        intersect(
            names(params),
            align_method_params(object$ggplot, character())
        )
    ]
    p <- inject(object$ggplot(!!!ggplot_params))
    object$plot <- p

    # add annotation -------------------------------------
    list(new_panel, new_index)
}

############################################################
abort_no_layout_data <- function(data, object_name = NULL, call = NULL) {
    if (is.null(data) || is.waive(data)) {
        if (is.null(object_name)) {
            msg <- c(
                "you must provide {.arg data}",
                i = "no data was found in the layout"
            )
        } else {
            msg <- c(
                "you must provide {.arg data} in {.var {object_name}}",
                i = "no data was found in the layout"
            )
        }
        if (is.null(call)) {
            cli::cli_abort(msg)
        } else {
            cli::cli_abort(msg, call = call)
        }
    }
}
