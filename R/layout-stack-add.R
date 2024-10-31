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

###################################################################
# `Align` can be added for both heatmap and stack layout
#' @export
stack_layout_add.Align <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active_index))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active_index]] <- plot
        layout <- slot(plot, stack@direction)
    } else if (is.null(layout <- stack@layout)) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to a free {.fn stack_layout}",
            i = "free {.fn stack_layout} won't align observations"
        ))
    } else {
        plots <- stack@plots

        # set up context index ------------------------------
        if (.subset2(.subset2(object, "context"), "active")) {
            active_index <- length(plots) + 1L
        } else {
            active_index <- stack@active
        }

        # check annotation name is unique --------------------
        if (!is.na(name <- .subset2(.subset2(object, "context"), "name"))) {
            if (any(names(plots) == name)) {
                cli::cli_warn("{object_name}: {name} plot is already present")
            }
            plots[[name]] <- object
        } else {
            plots <- c(plots, list(object))
        }

        # make layout ----------------------------------------
        # this step the object will act with the stack layout
        # group rows into panel or reorder rows
        layout <- align_initialize(
            object,
            direction = stack@direction,
            position = .subset2(stack@heatmap, "position"),
            layout_data = stack@data,
            layout_panel = .subset2(layout, "panel"),
            layout_index = .subset2(layout, "index"),
            layout_nobs = .subset2(layout, "nobs"),
            object_name = object_name
        )

        # add annotation -------------------------------------
        stack@plots <- plots
        stack@active <- active_index
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
        plots <- stack@plots
        call <- .subset2(object, "call")
        input_data <- .subset2(object, "data")
        layout_data <- stack@data
        if (is.waive(input_data)) { # inherit from the layout
            abort_no_layout_data(layout_data, call)
            data <- layout_data
        } else if (is.function(input_data)) {
            abort_no_layout_data(layout_data, call)
            data <- input_data(layout_data)
        } else {
            data <- input_data
        }

        # convert the data into a data frame
        object$plot$data <- fortify_data_frame(data)

        # set up context index ------------------------------
        if (.subset2(.subset2(object, "context"), "active")) {
            active_index <- length(plots) + 1L
        } else {
            active_index <- stack@active
        }

        # check annotation name is unique --------------------
        if (!is.na(name <- .subset2(.subset2(object, "context"), "name"))) {
            if (any(names(plots) == name)) {
                cli::cli_warn("{object_name}: {name} plot is already present")
            }
            plots[[name]] <- object
        } else {
            plots <- c(plots, list(object))
        }
        # add annotation -------------------------------------
        stack@plots <- plots
        stack@active <- active_index
    }
    stack
}

#' @export
stack_layout_add.ggplot <- function(object, stack, object_name) {
    stack_layout_add(free_gg(object), stack, object_name)
}

#' @export
stack_layout_add.stack_switch <- function(object, stack, object_name) {
    if (!is.waive(what <- .subset2(object, "what"))) {
        stack <- update_active(stack, what)
    }
    if (!is.null(sizes <- .subset2(object, "sizes"))) {
        stack@sizes <- sizes
    }
    if (!is.null(new_action <- .subset2(object, "action"))) {
        stack@action <- update_action(stack@action, new_action)
    }
    stack
}

#' @export
stack_layout_add.quad_switch <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_quad_layout(plot <- .subset2(stack@plots, active_index))) {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plots[[active_index]] <- plot
        return(stack)
    } else {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to the stack layout",
            i = "Did you forget to add a {.fn quad_layout}?"
        ))
    }
}

#' @importFrom rlang try_fetch
#' @importFrom methods slot
#' @export
stack_layout_add.QuadLayout <- function(object, stack, object_name) {
    direction <- stack@direction
    quad_data <- object@data

    # check quad layout is compatible with stack layout
    stack_layout <- stack@layout
    quad_layout <- slot(object, direction)
    if (is.null(stack_layout) && is.null(quad_layout)) {
        # both StackLayout and QuadLayout are free from aligning obervations
        # check if we should initialize the quad-layout data
        if (is.null(quad_data) || is.function(quad_data)) {
            if (is.null(stack_data <- stack@data)) {
                cli::cli_abort(c(
                    paste(
                        "you must provide {.arg data} argument in",
                        "{.var {object_name}}"
                    ),
                    i = "no data was found in {.fn {snake_class(stack)}}"
                ))
            }
            data <- stack_data # a data frame since `stack_layout` is free
            extra_layout <- slot(
                object, setdiff(c("vertical", "horizontal"), direction)
            )
            if (is.function(quad_data)) {
                data <- quad_data(data)
                if (is.null(extra_layout)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli::cli_abort(paste(
                            "{.arg data} in {.var {object_name}} must return",
                            "a {.cls data.frame}"
                        ))
                    }
                } else { # we need a matrix
                    if (!is.matrix(data)) {
                        cli::cli_abort(paste(
                            "{.arg data} in {.var {object_name}} must return",
                            "a {.cls matrix}"
                        ))
                    }
                }
            } else if (!is.null(extra_layout)) {
                # we require user input the matrix
                cli::cli_abort(c(
                    "you must provide {.arg data} matrix in {.var {object_name}}",
                    i = "data in {.fn {snake_class(stack)}} is a {.cls data.frame}"
                ))
            }

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
    } else if (!is.null(stack_layout) && !is.null(quad_layout)) {
        # if QuadLayout need align observations, we need a matrix
        if (is.null(quad_data) || is.function(quad_data)) {
            if (is.null(stack_data <- stack@data)) {
                cli::cli_abort(c(
                    "you must provide {.arg data} argument in {.var {object_name}}",
                    i = "no data was found in {.fn {snake_class(stack)}}"
                ))
            }
            # set QuadLayout data
            data <- switch_direction(direction, stack_data, t(stack_data))
            if (is.function(quad_data)) {
                data <- quad_data(data)
                if (!is.matrix(data)) {
                    cli::cli_abort(paste(
                        "{.arg data} in {.var {object_name}} must return",
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
                "{.var {object_name}} (%d) is not compatible with the %s (%d)",
                quad_nobs, "{.field {direction}} stack layout", stack_nobs
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
                "{.var {object_name}} disrupt the previously",
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
                "{.var {object_name}} disrupt the previously %s %s-axis",
                "established layout order of the stack",
                to_coord_axis(direction)
            ))
        }
        layout <- new_layout_params(panel, index, nobs)
    } else if (is.null(stack_layout)) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to a free {.fn stack_layout}",
            i = "free {.fn stack_layout} cannot align observations"
        ))
    } else {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to a aligned {.fn stack_layout}",
            i = "{.code {object_name}} cannot align observations in {direction} direction"
        ))
    }

    # set up context index ------------------------------
    plots <- stack@plots
    if (.subset2(object@context, "active")) {
        active_index <- length(plots) + 1L
    } else {
        active_index <- stack@active
    }

    # check QuadLayout name is unique ----------------------
    if (!is.na(name <- .subset2(object@context, "name"))) {
        if (any(names(plots) == name)) {
            cli::cli_warn(
                "{.var {object_name}}: {name} plot is already present"
            )
        }
        plots[[name]] <- object
    } else {
        plots <- c(plots, list(object))
    }

    # add QuadLayout ---------------------------------------
    stack@plots <- plots
    stack@active <- active_index

    # set the layout ------------------------------------
    if (!is.null(layout)) stack <- update_layout_params(stack, params = layout)
    stack
}

#' @export
stack_layout_add.list <- function(object, stack, object_name) {
    for (o in object) stack <- stack_layout_add(o, stack, object_name)
    stack
}
