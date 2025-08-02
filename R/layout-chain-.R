#' Finalize plot modifications from a ChainLayout object.
#'
#' This generic function lets a ChainLayout apply any final transformations
#' to the composed plot before returning it. It does not extract or store
#' the plot, but instead allows the layout to inject custom modifications
#' (e.g., spacing guides, annotations, alignment fixes) at the last step.
#'
#' @param layout A ChainLayout object.
#' @param plot The plot being finalized.
#' @keywords internal
chain_decorate <- function(layout, plot) UseMethod("chain_decorate")

#' @export
`chain_decorate.ggalign::ChainLayout` <- function(layout, plot) plot

#############################################################
# To-DO: Use double dispatch
#' @keywords internal
chain_layout_add <- function(object, layout, object_name) {
    UseMethod("chain_layout_add")
}

#' @export
chain_layout_add.layout_title <- function(object, layout, object_name) {
    layout@titles <- update_non_waive(layout@titles, object)
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
`chain_layout_add.ggalign::CraftBox` <- function(object, layout, object_name) {
    craftsman <- prop(object, "craftsman")
    # To-Do: Use S7 and double dispatch
    if (is.na(current <- layout@current) ||
        !is_layout(box <- .subset2(layout@box_list, current))) {
        # unlock the object
        craftsman$unlock()

        # we lock the `Craftsman` object to prevent user from modifying this
        # object in `$build_plot()` method, we shouldn't do any calculations in
        # `$build_plot()` method
        on.exit(craftsman$lock())

        # initialize the necessary parameters for `Craftsman` object
        if (is_stack_layout(layout)) {
            craftsman$direction <- layout@direction
            craftsman$position <- .subset2(layout@heatmap, "position")
        } else if (is_circle_layout(layout)) {
            # we treat circle layout as a vertical stack layout
            craftsman$direction <- "vertical"
        }
        craftsman$in_linear <- is_linear(layout)
        craftsman$layout_name <- object_name(layout)

        # firstly, we let the object do some changes in the layout
        layout <- craftsman$interact_layout(layout)

        # this step, the object will act with the stack layout
        # group rows into panel or reorder rows, we can also
        # initialize object data
        new_domain <- craftsman$setup_domain(layout@domain)

        # initialize the plot object
        # use attributes to bypass the prop setter checking
        attr(object, "plot") <- craftsman$setup_plot(object@plot)

        layout <- chain_add_box(layout, object, object@active, object_name)
    } else { # should be a QuadLayout object
        box <- quad_layout_add(object, box, object_name)
        layout@box_list[[current]] <- box
        new_domain <- prop(box, layout@direction)
    }
    layout_update_domain(layout, domain = new_domain, objectname = object_name)
}

#' @export
chain_layout_add.continuous_limits <- function(object, layout, object_name) {
    if (is_discrete_domain(layout@domain)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = sprintf(
                "%s cannot align continuous variables",
                object_name(layout)
            )
        ))
    }
    layout_update_domain(layout, domain = object, objectname = object_name)
}

#' @export
chain_layout_add.ggplot <- function(object, layout, object_name) {
    chain_layout_add(ggfree(data = object), layout, object_name)
}

# Add ggplot2 elements
#' @export
chain_layout_add.default <- function(object, layout, object_name) {
    if (is.na(current <- layout@current)) {
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
    box <- .subset2(layout@box_list, current)
    if (is_craftbox(box)) {
        box <- chain_plot_add(box, object, object_name, TRUE)
    } else {
        box <- quad_layout_add(object, box, object_name)
    }
    layout@box_list[[current]] <- box
    layout
}

#' @export
chain_layout_add.layout_theme <- function(object, layout, object_name) {
    if (is.na(current <- layout@current) ||
        !is_layout(box <- .subset2(layout@box_list, current))) {
        attr(layout, "theme") <- layout_theme_update(layout@theme, object)
    } else {
        layout@box_list[[current]] <- quad_layout_add(
            object, box, object_name
        )
    }
    layout
}

chain_plot_add <- function(plot, object, object_name, force) {
    # if `align` has plot, we added the object
    if (force || !is.null(plot@plot)) {
        plot <- craftbox_add(object, plot, object_name)
    }
    plot
}

#' @importFrom S7 prop
chain_add_box <- function(layout, box, active, object_name) {
    # set up context index
    box_list <- layout@box_list
    if (prop(active, "use")) {
        current <- length(box_list) + 1L
    } else {
        current <- layout@current
    }

    # check the name is unique
    if (!is.na(name <- prop(active, "name"))) {
        if (any(names(box_list) == name)) {
            cli_warn(
                "Adding {.var {object_name}} will replace existing {.field {name}} plot"
            )
        }
        box_list[[name]] <- box
    } else {
        box_list <- c(box_list, list(box))
    }

    # add QuadLayout
    layout@box_list <- box_list
    layout@current <- current
    layout
}

switch_chain_plot <- function(layout, what, call = caller_call()) {
    if (!is.waive(what)) {
        if (!is.na(what)) {
            what <- vec_as_location2(
                what,
                vec_size(layout@box_list),
                vec_names(layout@box_list),
                missing = "error",
                arg = "what", call = call
            )
        }
        layout@current <- what
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
    if (is.na(current <- layout@current) ||
        is_craftbox(plot <- .subset2(layout@box_list, current))) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "Did you forget to add a {.fn quad_layout}?"
        ))
    } else {
        layout@box_list[[current]] <- quad_layout_add(
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
`chain_layout_add.ggalign::StackLayout` <- chain_layout_add.quad_active

#' @export
`chain_layout_add.ggalign::StackCross` <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }

    # preventing from adding `stack_cross` with the same direction in this way,
    # `stack_cross()` cannot be added to the heatmap annotation parallelly with
    # the `stack_layout()`
    if (identical(object@direction, layout@direction)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "Cannot add {.fn stack_cross} with the same direction as {.fn stack_discrete}."
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

#' @export
`chain_layout_add.ggalign::QuadLayout` <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }

    # preventing from adding `stack_cross` with the same direction
    # `cross_link()` cannot be added to the heatmap annotation
    # parallelly with the `stack_cross()`
    if (is_horizontal(direction <- layout@direction)) {
        if (is_cross_layout(object@left) || is_cross_layout(object@right)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(layout)
                ),
                i = sprintf(
                    "{.field left} or {.field right} annotation contains %s",
                    "{.fn stack_cross}"
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
                "{.fn stack_cross}"
            )
        ))
    }

    # check quad layout is compatible with stack layout
    quad_data <- object@data
    stack_domain <- layout@domain
    quad_domain <- prop(object, direction)
    if (!is_discrete_domain(quad_domain)) {
        if (is_discrete_domain(stack_domain)) {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(object), object_name(layout)
                ),
                i = sprintf(
                    "%s cannot align continuous variable",
                    object_name(layout)
                )
            ))
        }
        # `quad_layout()` will align continuous variables,
        # `data` can be `NULL`
        extra_domain <- prop(object, vec_set_difference(
            c("vertical", "horizontal"), direction
        ))
        allow_null <- !is_discrete_domain(extra_domain)
        if (is.waive(quad_data) || is.function(quad_data)) {
            # check if we should initialize the `quad_layout()` data
            if (is.null(stack_data <- layout@data)) {
                if (allow_null) {
                    quad_data <- NULL
                } else {
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
            } else {
                data <- stack_data # should be a data frame
                if (is.waive(quad_data)) { # inherit from the stack layout
                    if (!allow_null) { # we need a matrix
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
                    if (allow_null) { # we need a data frame
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
                    } else {
                        if (NROW(data) == 0L || ncol(data) == 0L) {
                            cli_abort(sprintf(
                                "{.arg data} in %s return an empty matrix",
                                object_name(object)
                            ))
                        }
                    }
                }
                # we initialize the `nobs` of the extra_domain for the
                # `quad_layout()`
                if (is_horizontal(direction)) {
                    if (is_discrete_domain(prop(object, "vertical"))) {
                        prop(prop(object, "vertical"), "nobs") <- ncol(data)
                    }
                } else {
                    if (is_discrete_domain(prop(object, "horizontal"))) {
                        prop(prop(object, "horizontal"), "nobs") <- nrow(data)
                    }
                }
            }
            # restore the ggalign attribute
            object@data <- ggalign_data_restore(data, stack_data)
        }
        layout_domain <- quad_domain
    } else if (is_discrete_domain(stack_domain)) {
        # both `quad_layout()` and `stack_layout()` will align discrete
        # variables
        if (is.waive(quad_data) || is.function(quad_data)) {
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
                if (NROW(data) == 0L || ncol(data) == 0L) {
                    cli_abort(sprintf(
                        "{.arg data} in %s return an empty matrix",
                        object_name(object)
                    ))
                }
            } else {
                if (NROW(data) == 0L || ncol(data) == 0L) {
                    cli_abort(c(
                        sprintf(
                            "Cannot use data from %s in %s",
                            object_name(layout), object_name(object)
                        ),
                        i = sprintf(
                            "{.arg data} in %s is an empty matrix",
                            object_name(layout)
                        )
                    ))
                }
            }
            # set the `nobs` for `quad_layout()`
            if (is_horizontal(direction)) {
                prop(quad_domain, "nobs") <- nrow(data)
                if (is_discrete_domain(prop(object, "vertical"))) {
                    prop(prop(object, "vertical"), "nobs") <- ncol(data)
                }
            } else {
                prop(quad_domain, "nobs") <- ncol(data)
                if (is_discrete_domain(prop(object, "horizontal"))) {
                    prop(prop(object, "horizontal"), "nobs") <- nrow(data)
                }
            }
            # restore the ggalign attribute
            object@data <- ggalign_data_restore(data, stack_data)
        }
        layout_domain <- discrete_domain_update(
            stack_domain, quad_domain,
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
    stack <- chain_add_box(layout, object, object@plot_active, object_name)
    layout_update_domain(
        stack,
        domain = layout_domain, objectname = object_name
    )
}

##################################################
#' @export
chain_layout_add.circle_switch <- function(object, layout, object_name) {
    if (!is_circle_layout(layout)) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(layout)
            ),
            i = "Did you want to add a {.fn stack_switch}?"
        ))
    }
    if (!is.waive(radial <- .subset2(object, "radial"))) {
        layout@radial <- radial
    }
    if (!is.null(direction <- .subset2(object, "direction"))) {
        layout@direction <- direction
    }
    layout <- switch_chain_plot(
        layout, .subset2(object, "what"),
        quote(circle_switch())
    )
    layout
}
