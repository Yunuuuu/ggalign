#' @keywords internal
stack_layout_add <- function(object, stack, object_name) {
    UseMethod("stack_layout_add")
}

stack_name <- function(stack) {
    ans <- sprintf("{.fn %s}", stack@name)
    if (!is.null(position <- stack@heatmap$position)) {
        ans <- sprintf("the %s annotation %s", position, ans)
    } else {
        ans <- sprintf("a %s", ans)
    }
    ans
}

stack_plot_add <- function(plot, object, object_name, force) {
    if (is_free(plot)) {
        plot <- free_add(object, plot, object_name)
    } else if (force || !is.null(.subset2(plot, "plot"))) {
        # if `align` has plot, we added the object
        plot <- align_add(object, plot, object_name)
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

# add elements for nested `quad_layout()`.
#' @export
stack_layout_add.ggalign_with_quad <- function(object, stack, object_name) {
    active <- stack@active
    if (!is.null(active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active))) {
        stack@plots[[active]] <- quad_layout_add(object, plot, object_name)
    } else {
        cli_abort(c(
            sprintf(
                "Cannot add {.code {object_name}} to %s",
                stack_name(stack)
            ),
            i = "Did you forget to add a {.fn quad_layout}?"
        ))
    }
    stack
}

#' @export
stack_layout_add.quad_active <- stack_layout_add.ggalign_with_quad

#' @export
stack_layout_add.quad_anno <- stack_layout_add.quad_active

#' @export
stack_layout_add.quad_init <- stack_layout_add.quad_active

###################################################################
# add elements to the nested layout or the stack layout
# Now, only one possible nested layout: `quad_layout()`
stack_active_is_layout <- is_quad_layout

#' @export
stack_layout_add.layout_annotation <- function(object, stack, object_name) {
    active <- stack@active
    if (!is.null(active) &&
        stack_active_is_layout(plot <- .subset2(stack@plots, active))) {
        stack@plots[[active]] <- quad_layout_add(object, plot, object_name)
    } else {
        stack <- update_layout_annotation(object, stack, object_name)
    }
    stack
}

#' @export
stack_layout_add.default <- function(object, stack, object_name) {
    if (is.null(active <- stack@active)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.code {object_name}} to %s",
                stack_name(stack)
            ),
            i = "No active plot component",
            i = paste(
                "Did you forget to initialize a {.cls ggplot} object",
                "with {.fn ggalign} or {.fn ggfree}?"
            )
        ))
    }
    plot <- .subset2(stack@plots, active)
    if (stack_active_is_layout(plot)) {
        plot <- quad_layout_add(object, plot, object_name)
    } else {
        plot <- stack_plot_add(plot, object, object_name, TRUE)
    }
    stack@plots[[active]] <- plot
    stack
}

# `Align` can be added for both heatmap and stack layout
#' @export
stack_layout_add.ggalign_align <- function(object, stack, object_name) {
    if (!is.null(active <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active]] <- plot
        layout_coords <- slot(plot, stack@direction)
    } else if (is.null(layout_coords <- stack@layout)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.code {object_name}} to %s",
                stack_name(stack)
            ),
            i = sprintf("%s won't align observations", stack_name(stack))
        ))
    } else {
        # internal `Align` object
        workflow <- .subset2(object, "Object")

        # make the layout panels ----------------------------
        # this step the object will act with the stack layout
        # group rows into panel or reorder rows
        layout_coords <- workflow$initialize(
            direction = stack@direction,
            position = .subset2(stack@heatmap, "position"),
            object_name = object_name,
            layout_data = stack@data,
            layout_coords = layout_coords,
            layout_name = stack_name(stack)
        )

        # in the finally, Let us initialize the annotation plot -----
        # must return a ggplot object
        object$plot <- inject(workflow$ggplot(!!!workflow$params[
            intersect(
                names(workflow$params),
                align_method_params(workflow$ggplot, character())
            )
        ]))

        stack <- stack_add_plot(
            stack, object,
            .subset2(.subset2(object, "active"), "use"),
            .subset2(.subset2(object, "active"), "name"),
            object_name
        )
    }

    # set the layout -------------------------------------
    update_layout_params(stack,
        direction = stack@direction,
        params = layout_coords
    )
}

#' @export
stack_layout_add.ggalign_free_gg <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active_index))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active_index]] <- plot
    } else {
        input_data <- .subset2(object, "data")
        layout_data <- stack@data
        if (is.waive(input_data)) { # inherit from the layout
            data <- layout_data
        } else if (is.function(input_data)) {
            if (is.null(layout_data)) {
                cli_abort(c(
                    "{.arg data} in {.var {object_name}} cannot be a function",
                    i = sprintf("no data was found in %s", stack_name(stack))
                ))
            }
            data <- input_data(layout_data)
        } else {
            data <- input_data
        }
        data <- fortify_data_frame(data)

        # convert the data into a data frame
        object$plot <- free_gg_build_plot(.subset2(object, "plot"), data)

        stack <- stack_add_plot(
            stack, object,
            .subset2(.subset2(object, "active"), "use"),
            .subset2(.subset2(object, "active"), "name"),
            object_name
        )
    }
    stack
}

#' @export
stack_layout_add.ggplot <- function(object, stack, object_name) {
    stack_layout_add(free_gg(data = object), stack, object_name)
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
                what, vec_size(stack@plots), vec_names(stack@plots),
                missing = "error", arg = "what", call = call
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
    direction <- stack@direction
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
                    paste(
                        "you must provide {.arg data} argument in",
                        "{.fn {object@name}}"
                    ),
                    i = "no data was found in {.fn {stack@name}}"
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
                            "Cannot add {.fn {object@name}} to a {.fn {stack@name}}",
                            i = paste(
                                "`data` in {.fn {object@name}} is",
                                "{.obj_type_friendly {data}},",
                                "but we need a {.cls data.frame}."
                            ),
                            i = "Try provide {.arg data} in {.fn {object@name}}"
                        ))
                    }
                } else if (!is.matrix(data)) { # we need a matrix
                    cli_abort(c(
                        "Cannot add {.fn {object@name}} to a {.fn {stack@name}}",
                        i = paste(
                            "`data` in {.fn {object@name}} is",
                            "{.obj_type_friendly {data}},",
                            "but we need a {.cls matrix}."
                        ),
                        i = "Try provide {.arg data} in {.fn {object@name}}"
                    ))
                }
            } else { # `quad_data` is a function
                data <- quad_data(data)
                # check the data format is correct
                if (is.null(extra_params)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli_abort(paste(
                            "{.arg data} in {.fn {object@name}} must return",
                            "a {.cls data.frame}"
                        ))
                    }
                } else if (!is.matrix(data)) { # we need a matrix
                    cli_abort(paste(
                        "{.arg data} in {.fn {object@name}} must return",
                        "a {.cls matrix}"
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
                    "you must provide {.arg data} argument in {.fn {object@name}}",
                    i = "no data was found in {.fn {stack@name}}"
                ))
            }
            # set `quad_layout()` data
            data <- switch_direction(direction, stack_data, t(stack_data))
            if (is.function(quad_data)) {
                data <- quad_data(data)
                if (!is.matrix(data)) {
                    cli_abort(paste(
                        "{.arg data} in {.fn {object@name}} must return",
                        "a matrix"
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
        layout_coords <- check_layout_params(
            quad_coords, stack_coords,
            old_name = stack_name(stack),
            new_name = object_name
        )
    } else {
        cli_abort(c(
            sprintf("Cannot add {.fn {object@name}} to %s", stack_name(stack)),
            i = sprintf("%s cannot align observations", stack_name(stack))
        ))
    }
    stack <- stack_add_plot(
        stack, object,
        .subset2(object@plot_active, "use"),
        .subset2(object@plot_active, "name"),
        object_name
    )
    update_layout_params(stack, params = layout_coords)
}

stack_add_plot <- function(stack, plot, use, name, object_name) {
    # set up context index ------------------------------
    plots <- stack@plots
    if (use) {
        active_index <- length(plots) + 1L
    } else {
        active_index <- stack@active
    }

    # check QuadLayout name is unique ----------------------
    if (!is.na(name)) {
        if (any(names(plots) == name)) {
            cli_warn(
                "Adding {.var {object_name}} will replace existing {.field {name}} plot"
            )
        }
        plots[[name]] <- plot
    } else {
        plots <- c(plots, list(plot))
    }

    # add QuadLayout ---------------------------------------
    stack@plots <- plots
    stack@active <- active_index
    stack
}
