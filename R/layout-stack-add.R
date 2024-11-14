#' @keywords internal
stack_layout_add <- function(object, stack, object_name) {
    UseMethod("stack_layout_add")
}

#' @export
stack_layout_add.default <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active)) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to the stack layout",
            i = "No active {.cls ggplot} object",
            i = paste(
                "Did you forget to initialize a {.cls ggplot} object",
                "with {.fn ggalign} or {.fn ggfree}?"
            )
        ))
    }
    plot <- .subset2(stack@plots, active_index)
    if (is_quad_layout(plot)) {
        plot <- quad_layout_add(object, plot, object_name)
    } else if (is_align(plot)) {
        plot <- align_add(object, plot, object_name)
    } else if (is_free(plot)) {
        plot <- free_add(object, plot, object_name)
    }
    stack@plots[[active_index]] <- plot
    stack
}

#' @export
stack_layout_add.ggalign_option <- stack_layout_add.default

#' @export
stack_layout_add.layout_annotation <- function(object, stack, object_name) {
    active_index <- stack@active
    if (!is.null(active_index) &&
        is_layout(plot <- stack@plots[[active_index]])) {
        stack@plots[[active_index]] <- update_layout_annotation(
            object, plot, object_name
        )
    } else {
        stack <- update_layout_annotation(object, stack, object_name)
    }
    stack
}

#' @export
stack_layout_add.layout_title <- function(object, stack, object_name) {
    stack@titles <- update_non_waive(stack@titles, object)
    stack
}

###################################################################
# `Align` can be added for both heatmap and stack layout
#' @export
stack_layout_add.align <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active_index))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active_index]] <- plot
        layout <- slot(plot, stack@direction)
    } else if (is.null(layout <- stack@layout)) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to a {.fn {stack@name}}",
            i = "{.fn {stack@name}} won't align observations"
        ))
    } else {
        # make layout ----------------------------------------
        # this step the object will act with the stack layout
        # group rows into panel or reorder rows
        data <- align_initialize(
            object,
            direction = stack@direction,
            position = .subset2(stack@heatmap, "position"),
            layout_data = stack@data,
            layout_panel = .subset2(layout, "panel"),
            layout_index = .subset2(layout, "index"),
            layout_nobs = .subset2(layout, "nobs"),
            object_name = object_name
        )
        # initialize the plot
        object$plot <- .subset2(data, "plot")
        stack <- stack_add_plot(
            stack, object,
            .subset2(.subset2(object, "active"), "use"),
            .subset2(.subset2(object, "active"), "name"),
            object_name
        )
        layout <- .subset2(data, "layout")
    }

    # set the layout -------------------------------------
    update_layout_params(stack, direction = stack@direction, params = layout)
}

#' @export
stack_layout_add.free_gg <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active_index))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active_index]] <- plot
    } else {
        input_data <- .subset2(object, "data")
        layout_data <- stack@data
        if (is.waive(input_data)) { # inherit from the layout
            data <- layout_data %|w|% NULL
        } else if (is.function(input_data)) {
            abort_no_layout_data(layout_data, object_name,
                call = quote(free_gg())
            )
            data <- input_data(layout_data)
        } else {
            data <- input_data
        }
        # convert the data into a data frame
        object$plot$data <- fortify_data_frame(data)

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

#' @export
stack_layout_add.quad_active <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active_index))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active_index]] <- plot
        return(stack)
    } else {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to {.fn {stack@name}}",
            i = "Did you forget to add a {.fn quad_layout}?"
        ))
    }
}

#' @export
stack_layout_add.quad_anno <- stack_layout_add.quad_active

#' @export
stack_layout_add.quad_init <- stack_layout_add.quad_active

#' @importFrom methods slot
#' @export
stack_layout_add.QuadLayout <- function(object, stack, object_name) {
    direction <- stack@direction
    quad_data <- object@data

    # check quad layout is compatible with stack layout
    stack_layout <- stack@layout
    quad_layout <- slot(object, direction)
    if (is.null(quad_layout)) {
        # `QuadLayout` are free from aligning obervations in this axis
        if (is.null(quad_data) || is.function(quad_data)) {
            # check if we should initialize the `quad-layout` data
            if (is.null(stack_data <- stack@data) || is.function(stack_data)) {
                cli::cli_abort(c(
                    paste(
                        "you must provide {.arg data} argument in",
                        "{.fn {object@name}}"
                    ),
                    i = "no data was found in {.fn {stack@name}}"
                ))
            }
            extra_layout <- slot(object, vec_set_difference(
                c("vertical", "horizontal"), direction
            ))
            if (is.matrix(data <- stack_data)) {
                data <- switch_direction(direction, data, t(data))
            }
            if (is.null(quad_data)) { # inherit from the stack layout
                if (is.null(extra_layout)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli::cli_abort(c(
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
                    cli::cli_abort(c(
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
                if (is.null(extra_layout)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli::cli_abort(paste(
                            "{.arg data} in {.fn {object@name}} must return",
                            "a {.cls data.frame}"
                        ))
                    }
                } else if (!is.matrix(data)) { # we need a matrix
                    cli::cli_abort(paste(
                        "{.arg data} in {.fn {object@name}} must return",
                        "a {.cls matrix}"
                    ))
                }
            }
            # we initialize the `nobs` for the `quad_layout()`
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
            object@data <- restore_attr_ggalign(data, stack_data)
        }
        layout <- NULL
    } else if (!is.null(stack_layout)) {
        # both `quad_layout()` and `stack_layout()` need align observations
        if (is.null(quad_data) || is.function(quad_data)) {
            if (is.null(stack_data <- stack@data) || is.function(stack_data)) {
                cli::cli_abort(c(
                    "you must provide {.arg data} argument in {.fn {object@name}}",
                    i = "no data was found in {.fn {stack@name}}"
                ))
            }
            # set QuadLayout data
            data <- switch_direction(direction, stack_data, t(stack_data))
            if (is.function(quad_data)) {
                data <- quad_data(data)
                if (!is.matrix(data)) {
                    cli::cli_abort(paste(
                        "{.arg data} in {.fn {object@name}} must return",
                        "a matrix"
                    ))
                }
            }
            # set the `nobs` for QuadLayout
            if (is_horizontal(direction)) {
                quad_layout$nobs <- nrow(data)
                if (!is.null(slot(object, "vertical"))) {
                    slot(object, "vertical")$nobs <- ncol(data)
                }
            } else {
                quad_layout$nobs <- ncol(data)
                if (!is.null(slot(object, "horizontal"))) {
                    slot(object, "horizontal")$nobs <- nrow(data)
                }
            }
            # restore the ggalign attribute
            object@data <- restore_attr_ggalign(data, stack_data)
        }

        # check the observations is compatible ----------------
        stack_nobs <- .subset2(stack_layout, "nobs")
        quad_nobs <- .subset2(quad_layout, "nobs")
        if (is.null(quad_nobs)) {
            nobs <- stack_nobs
        } else if (is.null(stack_nobs)) {
            nobs <- quad_nobs
        } else if (!identical(quad_nobs, stack_nobs)) {
            cli::cli_abort(sprintf(
                "{.fn {object@name}} (%d) is not compatible with the %s (%d)",
                quad_nobs, "{.field {direction}} {.fn {stack@name}}", stack_nobs
            ))
        } else {
            nobs <- quad_nobs
        }

        # check panel and index ------------------------------
        stack_panel <- .subset2(stack_layout, "panel")
        quad_panel <- .subset2(quad_layout, "panel")
        if (is.null(quad_panel)) {
            panel <- stack_panel
        } else if (is.null(stack_panel)) {
            panel <- quad_panel
        } else if (!(quad_panel %nest% stack_panel)) {
            cli::cli_abort(paste(
                "{.fn {object@name}} disrupt the previously",
                "established layout panel of the stack"
            ))
        } else {
            panel <- quad_panel
        }

        stack_index <- .subset2(stack_layout, "index")
        quad_index <- .subset2(quad_layout, "index")
        index <- quad_index %||% stack_index
        if (!is.null(panel) && !is.null(index)) {
            index <- reorder_index(panel, index)
        }

        # we always prevent from reordering the layout twice.
        if (!is.null(stack_index) && !all(stack_index == index)) {
            cli::cli_abort(sprintf(
                "{.fn {object@name}} disrupt the previously %s %s-axis",
                "established layout order of the stack",
                to_coord_axis(direction)
            ))
        }
        layout <- new_layout_params(panel, index, nobs)
    } else {
        cli::cli_abort(c(
            "Cannot add {.fn {object@name}} to a {.fn {stack@name}}",
            i = "{.fn {stack@name}} cannot align observations"
        ))
    }
    stack <- stack_add_plot(
        stack, object,
        .subset2(object@plot_active, "use"),
        .subset2(object@plot_active, "name"),
        object_name
    )
    # set the layout ------------------------------------
    if (!is.null(layout)) stack <- update_layout_params(stack, params = layout)
    stack
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
            cli::cli_warn(
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

#' @export
stack_layout_add.list <- function(object, stack, object_name) {
    for (o in object) stack <- stack_layout_add(o, stack, object_name)
    stack
}
