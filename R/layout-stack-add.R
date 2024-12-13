#' @keywords internal
stack_layout_add <- function(object, stack, object_name) {
    UseMethod("stack_layout_add")
}

stack_plot_add <- function(plot, object, object_name, force) {
    # if `align` has plot, we added the object
    if (force || !is.null(plot@plot)) {
        plot <- plot_add(plot, object, object_name)
    }
    plot
}

#' @export
stack_layout_add.layout_title <- function(object, stack, object_name) {
    stack@titles <- update_layout_title(stack@titles, object)
    stack
}

#' @export
stack_layout_add.list <- function(object, stack, object_name) {
    for (o in object) stack <- stack_layout_add(o, stack, object_name)
    stack
}

##############################################################
# add elements for nested `quad_layout()`.
#' @export
stack_layout_add.ggalign_with_quad <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(stack)
            ),
            i = "Did you forget to add a {.fn quad_layout}?"
        ))
    } else {
        stack@plot_list[[active_index]] <- quad_layout_add(
            object, plot, object_name
        )
    }
    stack
}

#' @export
stack_layout_add.quad_active <- stack_layout_add.ggalign_with_quad

#' @export
stack_layout_add.quad_anno <- stack_layout_add.quad_active

#' @export
stack_layout_add.StackLayout <- stack_layout_add.quad_active

#' @export
stack_layout_add.CrossLayout <- function(object, stack, object_name) {
    # preventing from adding cross_layout with the same direction
    # in this way, `ggcross()` cannot be added to the heatmap annotation
    # parallelly with the `stack_layout()`
    if (identical(object@direction, direction <- stack@direction)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s (direction: %s)",
            object_name(stack), direction
        ))
    }
    NextMethod()
}

#' @export
stack_layout_add.ggalign_cross <- function(object, stack, object_name) {
    if (is_cross_layout(stack)) {
        NextMethod() # call `ggalign_align_plot` method
    } else {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(stack)
            ),
            i = "Did you want to use {.fn cross_align} instead?"
        ))
    }
}

###################################################################
# add elements to the nested layout or the stack layout
# Now, only one possible nested layout: `quad_layout()`
#' @export
stack_layout_add.layout_annotation <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        stack <- update_layout_annotation(object, stack, object_name)
    } else {
        stack@plot_list[[active_index]] <- quad_layout_add(
            object, plot, object_name
        )
    }
    stack
}

#' @export
stack_layout_add.default <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(stack)
            ),
            i = "No active plot component",
            i = paste(
                "Did you forget to initialize a {.cls ggplot} object",
                "with {.fn ggalign} or {.fn ggfree}?"
            )
        ))
    }
    plot <- .subset2(stack@plot_list, active_index)
    if (is_ggalign_plot(plot)) {
        plot <- stack_plot_add(plot, object, object_name, TRUE)
    } else {
        plot <- quad_layout_add(object, plot, object_name)
    }
    stack@plot_list[[active_index]] <- plot
    stack
}

#' @export
stack_layout_add.stack_switch <- function(object, stack, object_name) {
    stack <- update_stack_active(
        stack, .subset2(object, "what"),
        quote(stack_switch())
    )
    if (!is.null(sizes <- .subset2(object, "sizes"))) {
        stack@sizes <- sizes
    }
    stack
}

update_stack_active <- function(stack, what, call = caller_call()) {
    if (!is.waive(what)) {
        if (!is.null(what)) {
            what <- vec_as_location2(
                what,
                vec_size(stack@plot_list),
                vec_names(stack@plot_list),
                missing = "error",
                arg = "what", call = call
            )
        }
        stack@active <- what
    }
    stack
}

# add elements to the stack_layout()
#' @importFrom methods slot
#' @export
stack_layout_add.QuadLayout <- function(object, stack, object_name) {
    # preventing from adding cross_layout with the same direction
    # in this way, `cross_link()` cannot be added to the heatmap annotation
    # parallelly with the `stack_layout()`
    if (is_horizontal(direction <- stack@direction)) {
        if (is_cross_layout(object@left) || is_cross_layout(object@right)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(stack)
                ),
                i = paste(
                    "{.field left} or {.field right} annotation",
                    "contains {.fn cross_layout}"
                )
            ))
        }
    } else {
        if (is_cross_layout(object@top) || is_cross_layout(object@bottom)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(stack)
                ),
                i = paste(
                    "{.field top} or {.field bottom} annotation",
                    "contains {.fn cross_layout}"
                )
            ))
        }
    }
    quad_data <- object@data

    # check quad layout is compatible with stack layout
    stack_coords <- stack@layout
    quad_coords <- slot(object, direction)
    if (is.null(quad_coords)) {
        # `quad_layout()` are free from aligning obervations in this axis
        if (is.null(quad_data) || is.function(quad_data)) {
            # check if we should initialize the `quad_layout()` data
            if (is.null(stack_data <- stack@data)) {
                cli_abort(c(
                    sprintf(
                        "you must provide {.arg data} argument in %s",
                        object_name(object)
                    ),
                    i = sprintf(
                        "no data was found in %s",
                        object_name(stack)
                    )
                ))
            }
            extra_params <- slot(object, vec_set_difference(
                c("vertical", "horizontal"), direction
            ))
            if (is.matrix(data <- stack_data)) {
                data <- switch_direction(direction, data, t(data))
            }
            if (is.null(quad_data)) { # inherit from the stack layout
                if (is.null(extra_params)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli_abort(c(
                            sprintf(
                                "Cannot add %s to %s",
                                object_name(object),
                                object_name(stack)
                            ),
                            i = sprintf(
                                "{.arg data} in %s is %s, but %s need a {.cls data.frame}.",
                                object_name(stack),
                                "{.obj_type_friendly {data}}",
                                object_name(object)
                            ),
                            i = sprintf(
                                "Try provide {.arg data} in %s",
                                object_name(object)
                            )
                        ))
                    }
                } else if (!is.matrix(data)) { # we need a matrix
                    cli_abort(c(
                        sprintf(
                            "Cannot add %s to %s",
                            object_name(object), object_name(stack)
                        ),
                        i = sprintf(
                            "{.arg data} in %s is %s, but %s need a {.cls matrix}.",
                            object_name(stack),
                            "{.obj_type_friendly {data}}",
                            object_name(object)
                        ),
                        i = sprintf(
                            "Try provide {.arg data} in %s",
                            object_name(object)
                        )
                    ))
                }
            } else { # `quad_data` is a function
                data <- quad_data(data)
                # check the data format is correct
                if (is.null(extra_params)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli_abort(sprintf(
                            "{.arg data} in %s must return a {.cls data.frame}",
                            object_name(object)
                        ))
                    }
                } else if (!is.matrix(data)) { # we need a matrix
                    cli_abort(sprintf(
                        "{.arg data} in %s must return a {.cls matrix}",
                        object_name(object)
                    ))
                }
            }
            # we initialize the `nobs` of the extra_coords for the
            # `quad_layout()`
            if (is_horizontal(direction)) {
                if (!is.null(slot(object, "vertical"))) {
                    slot(object, "vertical")$nobs <- ncol(data)
                }
            } else {
                if (!is.null(slot(object, "horizontal"))) {
                    slot(object, "horizontal")$nobs <- nrow(data)
                }
            }
            # restore the ggalign attribute
            object@data <- ggalign_attr_restore(data, stack_data)
        }
        layout_coords <- NULL
    } else if (!is.null(stack_coords)) {
        # both `quad_layout()` and `stack_layout()` need align observations
        if (is.null(quad_data) || is.function(quad_data)) {
            if (is.null(stack_data <- stack@data)) {
                cli_abort(c(
                    sprintf(
                        "you must provide {.arg data} argument in %s",
                        object_name(object)
                    ),
                    i = sprintf(
                        "no data was found in %s",
                        object_name(stack)
                    )
                ))
            }
            # set `quad_layout()` data
            data <- switch_direction(direction, stack_data, t(stack_data))
            if (is.function(quad_data)) {
                data <- quad_data(data)
                if (!is.matrix(data)) {
                    cli_abort(sprintf(
                        "{.arg data} in %s must return a matrix",
                        object_name(object)
                    ))
                }
            }
            # set the `nobs` for `quad_layout()`
            if (is_horizontal(direction)) {
                quad_coords$nobs <- nrow(data)
                if (!is.null(slot(object, "vertical"))) {
                    slot(object, "vertical")$nobs <- ncol(data)
                }
            } else {
                quad_coords$nobs <- ncol(data)
                if (!is.null(slot(object, "horizontal"))) {
                    slot(object, "horizontal")$nobs <- nrow(data)
                }
            }
            # restore the ggalign attribute
            object@data <- ggalign_attr_restore(data, stack_data)
        }
        layout_coords <- check_layout_coords(
            stack_coords, quad_coords,
            old_name = object_name(stack),
            new_name = object_name
        )
    } else {
        cli_abort(c(
            sprintf(
                "Cannot add %s to %s",
                object_name(object), object_name(stack)
            ),
            i = sprintf(
                "%s cannot align observations",
                object_name(stack)
            )
        ))
    }
    stack <- stack_add_plot(stack, object, object@plot_active, object_name)
    update_layout_coords(
        stack,
        coords = layout_coords,
        object_name = object_name
    )
}

stack_add_plot <- function(stack, plot, active, object_name) {
    # set up context index ------------------------------
    plot_list <- stack@plot_list
    if (.subset2(active, "use")) {
        active_index <- length(plot_list) + 1L
    } else {
        active_index <- stack@active
    }

    # check QuadLayout name is unique ----------------------
    if (!is.na(name <- .subset2(active, "name"))) {
        if (any(names(plot_list) == name)) {
            cli_warn(
                "Adding {.var {object_name}} will replace existing {.field {name}} plot"
            )
        }
        plot_list[[name]] <- plot
    } else {
        plot_list <- c(plot_list, list(plot))
    }

    # add QuadLayout ---------------------------------------
    stack@plot_list <- plot_list
    stack@active <- active_index
    stack
}
