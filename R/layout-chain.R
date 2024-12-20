# Used by both `circle_layout()` and `stack_layout()`
#' @keywords internal
#' @include layout-.R
methods::setClass(
    "ChainLayout",
    contains = "LayoutProto",
    list(plot_list = "list") # save the list of plots
)

#' @export
is_layout_discrete.ChainLayout <- function(x, ...) {
    is_discrete_design(x@design)
}

#' @export
is_layout_continuous.ChainLayout <- function(x, ...) {
    is_continuous_design(x@design)
}

#############################################################
# To-DO: Use double dispatch
#' @keywords internal
chain_layout_add <- function(object, layout, object_name) {
    UseMethod("chain_layout_add")
}

#' @export
chain_layout_add.layout_title <- function(object, layout, object_name) {
    layout@titles <- update_layout_title(layout@titles, object)
    layout
}

#' @export
chain_layout_add.list <- function(object, layout, object_name) {
    for (o in object) layout <- chain_layout_add(o, layout, object_name)
    layout
}

#' @export
chain_layout_add.NULL <- function(object, layout, object_name) {
    layout
}

#' @export
chain_layout_add.ggalign_plot <- function(object, layout, object_name) {
    align <- object@align
    # To-Do: Use S7 and double dispatch
    if (is_cross(align) && !is_cross_layout(layout)) {
        # we prevent from adding `cross_*` to normal `stack_layout()`
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "Did you want to use {.fn cross_discrete} instead?"
        ))
    }
    if (is.null(active_index <- layout@active) ||
        is_ggalign_plot(plot <- .subset2(layout@plot_list, active_index))) {
        # check plot is compatible with the layout
        if (is_continuous_design(stack_design <- layout@design) &&
            # `Align` object is special for discrete variables
            is_align(align)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(layout)
                ),
                i = sprintf(
                    "%s cannot align discrete variables",
                    object_name(layout)
                )
            ))
        }

        # unlock the object
        align$unlock()

        # initialize the necessary parameters for `AlignProto` object
        align$direction <- layout@direction
        align$position <- .subset2(layout@heatmap, "position")
        align$object_name <- object_name
        align$layout_name <- object_name(layout)

        # this step, the object will act with the stack layout
        # group rows into panel or reorder rows, we can also
        # initialize object data
        new_design <- align$layout(
            layout_data = layout@data, # must be a matrix
            layout_coords = stack_design
        )

        # initialize the plot object
        object@plot <- align$setup_plot(object@plot)

        # finally, we let the object do some changes in the layout
        layout <- align$finish_layout(layout)

        # we lock the Align object to prevent user from modifying this
        # object in `$build` method, we shouldn't do any calculations in
        # `$build` method
        align$lock()
        layout <- chain_add_plot(layout, object, object@active, object_name)
    } else { # should be a QuadLayout object
        plot <- quad_layout_add(object, plot, object_name)
        layout@plot_list[[active_index]] <- plot
        new_design <- slot(plot, layout@direction)
    }
    update_design(
        layout,
        design = new_design,
        object_name = object_name
    )
}

# Add ggplot2 elements
#' @export
chain_layout_add.default <- function(object, layout, object_name) {
    if (is.null(active_index <- layout@active)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "No active plot component",
            i = paste(
                "Did you forget to initialize a {.cls ggplot} object",
                "with {.fn ggalign} or {.fn ggfree}?"
            )
        ))
    }
    plot <- .subset2(layout@plot_list, active_index)
    if (is_ggalign_plot(plot)) {
        plot <- chain_plot_add(plot, object, object_name, TRUE)
    } else {
        plot <- quad_layout_add(object, plot, object_name)
    }
    layout@plot_list[[active_index]] <- plot
    layout
}

#' @export
chain_layout_add.layout_annotation <- function(object, layout, object_name) {
    if (is.null(active_index <- layout@active) ||
        is_ggalign_plot(plot <- .subset2(layout@plot_list, active_index))) {
        layout <- update_layout_annotation(object, layout, object_name)
    } else {
        layout@plot_list[[active_index]] <- quad_layout_add(
            object, plot, object_name
        )
    }
    layout
}

chain_plot_add <- function(plot, object, object_name, force) {
    # if `align` has plot, we added the object
    if (force || !is.null(plot@plot)) {
        plot <- plot_add(object, plot, object_name)
    }
    plot
}

chain_add_plot <- function(layout, plot, active, object_name) {
    # set up context index
    plot_list <- layout@plot_list
    if (.subset2(active, "use")) {
        active_index <- length(plot_list) + 1L
    } else {
        active_index <- layout@active
    }
    # check the name is unique
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

    # add QuadLayout
    layout@plot_list <- plot_list
    layout@active <- active_index
    layout
}

switch_chain_plot <- function(layout, what, call = caller_call()) {
    if (!is.waive(what)) {
        if (!is.null(what)) {
            what <- vec_as_location2(
                what,
                vec_size(layout@plot_list),
                vec_names(layout@plot_list),
                missing = "error",
                arg = "what", call = call
            )
        }
        layout@active <- what
    }
    layout
}

##############################################################
# for `stack_layout()` only
#' @export
chain_layout_add.ggalign_with_quad <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }
    if (is.null(active_index <- layout@active) ||
        is_ggalign_plot(plot <- .subset2(layout@plot_list, active_index))) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "Did you forget to add a {.fn quad_layout}?"
        ))
    } else {
        layout@plot_list[[active_index]] <- quad_layout_add(
            object, plot, object_name
        )
    }
    layout
}

#' @export
chain_layout_add.quad_active <- chain_layout_add.ggalign_with_quad

#' @export
chain_layout_add.quad_anno <- chain_layout_add.quad_active

#' @export
chain_layout_add.StackLayout <- chain_layout_add.quad_active

#' @export
chain_layout_add.CrossLayout <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }
    # preventing from adding `cross_layout` with the same direction in this way,
    # `cross_layout()` cannot be added to the heatmap annotation parallelly with
    # the `stack_layout()`
    if (identical(object@direction, layout@direction)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }
    NextMethod() # call StackLayout method
}

#' @export
chain_layout_add.stack_switch <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "Did you want to add a {.fn circle_switch}?"
        ))
    }
    layout <- switch_chain_plot(
        layout, .subset2(object, "what"),
        quote(stack_switch())
    )
    if (!is.null(sizes <- .subset2(object, "sizes"))) {
        layout@sizes <- sizes
    }
    layout
}

#' @importFrom methods slot
#' @export
chain_layout_add.QuadLayout <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }

    # preventing from adding `cross_layout` with the same direction
    # `cross_link()` cannot be added to the heatmap annotation
    # parallelly with the `cross_layout()`
    if (is_horizontal(direction <- layout@direction)) {
        if (is_cross_layout(object@left) || is_cross_layout(object@right)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(layout)
                ),
                i = sprintf(
                    "{.field left} or {.field right} annotation contains %s",
                    "{.fn cross_layout}"
                )
            ))
        }
    } else if (is_cross_layout(object@top) || is_cross_layout(object@bottom)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = sprintf(
                "{.field top} or {.field bottom} annotation contains %s",
                "{.fn cross_layout}"
            )
        ))
    }

    # check quad layout is compatible with stack layout
    quad_data <- object@data
    stack_design <- layout@design
    quad_design <- slot(object, direction)
    if (is_continuous_design(quad_design)) {
        # `quad_layout()` will align continuous variables
        if (is.null(quad_data) || is.function(quad_data)) {
            # check if we should initialize the `quad_layout()` data
            if (is.null(stack_data <- layout@data)) {
                cli_abort(c(
                    sprintf(
                        "you must provide {.arg data} argument in %s",
                        object_name(object)
                    ),
                    i = sprintf(
                        "no data was found in %s",
                        object_name(layout)
                    )
                ))
            }
            extra_design <- slot(object, vec_set_difference(
                c("vertical", "horizontal"), direction
            ))
            if (is.matrix(data <- stack_data)) {
                data <- switch_direction(direction, data, t(data))
            }
            if (is.null(quad_data)) { # inherit from the stack layout
                if (is_continuous_design(extra_design)) { # we need a data frame
                    if (!is.data.frame(data)) {
                        cli_abort(c(
                            sprintf(
                                "Cannot add %s to %s",
                                object_name(object),
                                object_name(layout)
                            ),
                            i = sprintf(
                                "{.arg data} in %s is %s, but %s need a {.cls data.frame}.",
                                object_name(layout),
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
                            object_name(object), object_name(layout)
                        ),
                        i = sprintf(
                            "{.arg data} in %s is %s, but %s need a {.cls matrix}.",
                            object_name(layout),
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
                if (is_continuous_design(extra_design)) { # we need a data frame
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
            # we initialize the `nobs` of the extra_design for the
            # `quad_layout()`
            if (is_horizontal(direction)) {
                if (is_discrete_design(slot(object, "vertical"))) {
                    slot(object, "vertical")$nobs <- ncol(data)
                }
            } else {
                if (is_discrete_design(slot(object, "horizontal"))) {
                    slot(object, "horizontal")$nobs <- nrow(data)
                }
            }
            # restore the ggalign attribute
            object@data <- ggalign_attr_restore(data, stack_data)
        }
        layout_coords <- quad_design
    } else if (is_discrete_design(stack_design)) {
        # both `quad_layout()` and `stack_layout()` will align discrete
        # variables
        if (is.null(quad_data) || is.function(quad_data)) {
            if (is.null(stack_data <- layout@data)) {
                cli_abort(c(
                    sprintf(
                        "you must provide {.arg data} argument in %s",
                        object_name(object)
                    ),
                    i = sprintf(
                        "no data was found in %s",
                        object_name(layout)
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
                quad_design$nobs <- nrow(data)
                if (is_discrete_design(slot(object, "vertical"))) {
                    slot(object, "vertical")$nobs <- ncol(data)
                }
            } else {
                quad_design$nobs <- ncol(data)
                if (is_discrete_design(slot(object, "horizontal"))) {
                    slot(object, "horizontal")$nobs <- nrow(data)
                }
            }
            # restore the ggalign attribute
            object@data <- ggalign_attr_restore(data, stack_data)
        }
        layout_coords <- check_discrete_design(
            stack_design, quad_design,
            old_name = object_name(layout),
            new_name = object_name
        )
    } else {
        cli_abort(c(
            sprintf(
                "Cannot add %s to %s",
                object_name(object), object_name(layout)
            ),
            i = sprintf(
                "%s cannot align discrete variable",
                object_name(layout)
            )
        ))
    }
    stack <- chain_add_plot(layout, object, object@plot_active, object_name)
    update_design(
        stack,
        design = layout_coords,
        object_name = object_name
    )
}
