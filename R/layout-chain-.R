#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, CraftBox)) <-
    function(layout, object, objectname) {
        craftsman <- prop(object, "craftsman")
        # To-Do: Use S7 and double dispatch
        if (is.na(current <- layout@current) ||
            is_craftbox(box <- .subset2(layout@box_list, current))) {
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

            # Initialize the plot object by retrieving it via the property
            # accessor.
            plot <- prop(object, "plot")

            # Execute all initialization hooks in sequence, passing the current
            # plot and the craftsman's direction to each hook function. Each
            # hook may modify and return a new version of the plot.
            for (hook in prop(object, "init_hooks")) {
                plot <- hook(plot, craftsman$direction)
                if (!is_ggplot(plot)) {
                    cli_abort("{.field init_hooks} must return a {.cls ggplot} object.")
                }
            }

            # Finally, initialize the plot using the craftsman-specific
            # initializer, and update the object's plot attribute directly to
            # bypass setter checks.
            # Use 'attr()' to bypass property setter validations when
            # updating the plot.
            attr(object, "plot") <- craftsman$init_plot(plot)

            layout <- chain_add_box(layout, object, object@active, objectname)
        } else { # should be a QuadLayout object
            box <- layout_add(box, object, objectname)
            layout@box_list[[current]] <- box
            new_domain <- prop(box, layout@direction)
        }
        layout_update_domain(layout,
            domain = new_domain, objectname = objectname
        )
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, ggplot2::class_ggplot)) <-
    function(layout, object, objectname) {
        layout_add(layout, ggfree(data = object), objectname)
    }

#' @include layout-.R
S7::method(layout_add, list(ChainLayout, layout_title)) <-
    function(layout, object, objectname) {
        layout@titles <- layout@titles + object
        layout
    }

#' @include layout-.R
S7::method(layout_add, list(ChainLayout, S7::class_list)) <-
    function(layout, object, objectname) {
        for (o in object) layout <- layout_add(layout, o, object_name)
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, S7::class_any)) <-
    function(layout, object, objectname) {
        if (is.null(object)) return(layout) # styler: off
        if (is.na(current <- layout@current)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
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
            box <- chain_box_add(box, object, objectname, TRUE)
        } else {
            box <- layout_add(box, object, objectname)
        }
        layout@box_list[[current]] <- box
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, S3_layout_theme)) <-
    function(layout, object, objectname) {
        if (is.na(current <- layout@current) ||
            is_craftbox(box <- .subset2(layout@box_list, current))) {
            prop(layout, "theme") <- layout_theme_update(layout@theme, object)
        } else {
            layout@box_list[[current]] <- layout_add(
                box, object, objectname
            )
        }
        layout
    }

chain_box_add <- function(box, object, object_name, force) {
    # if `align` has plot, we added the object
    if (force || !is.null(box@plot)) {
        box <- craftbox_add(object, box, object_name)
    }
    box
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
    if (!is_waiver(what)) {
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
#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_add, list(StackLayout, quad_scope)) <-
    S7::method(
        layout_add,
        list(StackLayout, S7::new_S3_class("quad_active"))
    ) <-
    S7::method(
        layout_add,
        list(StackLayout, S7::new_S3_class("quad_anno"))
    ) <-
    S7::method(layout_add, list(StackLayout, StackLayout)) <-
    function(layout, object, objectname) {
        if (is.na(current <- layout@current) ||
            is_craftbox(box <- .subset2(layout@box_list, current))) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
                    object_name(layout)
                ),
                i = "Did you forget to add a {.fn quad_layout}?"
            ))
        } else {
            layout@box_list[[current]] <- layout_add(
                box, object, objectname
            )
        }
        layout
    }

S7::method(layout_add, list(CircleLayout, quad_scope)) <-
    S7::method(layout_add, list(CircleLayout, QuadLayout)) <-
    S7::method(
        layout_add,
        list(CircleLayout, S7::new_S3_class("quad_active"))
    ) <-
    S7::method(
        layout_add,
        list(CircleLayout, S7::new_S3_class("quad_anno"))
    ) <-
    S7::method(layout_add, list(CircleLayout, StackLayout)) <-
    function(layout, object, objectname) {
        cli_abort(c(
            sprintf("Cannot add %s to a {.fn circle_layout}", objectname),
            i = "Try to use {.fn stack_layout} instead"
        ))
    }

#' @include layout-.R
#' @include layout-operator.R
#' @importFrom S7 super
S7::method(layout_add, list(StackLayout, StackCross)) <-
    function(layout, object, objectname) {
        # preventing from adding `stack_cross` with the same direction in this
        # way, `stack_cross()` cannot be added to the heatmap annotation
        # parallelly with the `stack_layout()`
        if (identical(object@direction, layout@direction)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
                    object_name(layout)
                ),
                i = "Cannot add {.fn stack_cross} with the same direction as {.fn stack_discrete}."
            ))
        }
        # call StackLayout method
        layout_add(layout, super(object, StackLayout), objectname)
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(StackLayout, S7::new_S3_class("stack_switch"))) <-
    function(layout, object, objectname) {
        layout <- switch_chain_plot(
            layout, .subset2(object, "what"),
            quote(stack_switch())
        )
        if (!is.null(sizes <- .subset2(object, "sizes"))) {
            layout@sizes <- sizes
        }
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(CircleLayout, S7::new_S3_class("stack_switch"))) <-
    function(layout, object, objectname) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {objectname}} to %s",
                object_name(layout)
            ),
            i = "Did you want to add a {.fn circle_switch}?"
        ))
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(StackLayout, QuadLayout)) <-
    function(layout, object, objectname) {
        # preventing from adding `stack_cross` with the same direction
        # `cross_link()` cannot be added to the heatmap annotation
        # parallelly with the `stack_cross()`
        if (is_horizontal(direction <- layout@direction)) {
            if (is_cross_layout(object@left) || is_cross_layout(object@right)) {
                cli_abort(c(
                    sprintf(
                        "Cannot add {.var {objectname}} to %s",
                        object_name(layout)
                    ),
                    i = sprintf(
                        "{.field left} or {.field right} annotation contains %s",
                        "{.fn stack_cross}"
                    )
                ))
            }
        } else if (is_cross_layout(object@top) ||     # styler: off
                   is_cross_layout(object@bottom)) {  # styler: off
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
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
            if (is_waiver(quad_data) || is.function(quad_data)) {
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
                    if (is_waiver(quad_data)) { # inherit from the stack layout
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
            if (is_waiver(quad_data) || is.function(quad_data)) {
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

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(CircleLayout, S7::new_S3_class("circle_switch"))) <-
    function(layout, object, objectname) {
        if (!is_waiver(radial <- .subset2(object, "radial"))) {
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

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(StackLayout, S7::new_S3_class("circle_switch"))) <-
    function(layout, object, objectname) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {objectname}} to %s",
                object_name(layout)
            ),
            i = "Did you want to add a {.fn stack_switch}?"
        ))
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_subtract, list(ChainLayout, S7::class_any)) <-
    function(layout, object, objectname) {
        if (is.na(current <- layout@current) ||
            is_craftbox(box <- .subset2(layout@box_list, current))) {
            layout@box_list <- lapply(layout@box_list, function(box) {
                if (is_craftbox(box)) {
                    chain_box_add(box, object, objectname, force = FALSE)
                } else {
                    box
                }
            })
        } else {
            layout@box_list[[current]] <- layout_subtract(
                box, object, objectname
            )
        }
        layout
    }

# for objects can inherit from layout
#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_subtract, list(ChainLayout, Scheme)) <-
    function(layout, object, objectname) {
        if (is.na(current <- layout@current) ||
            is_craftbox(box <- .subset2(layout@box_list, current))) {
            layout <- update_layout_schemes(object, layout, objectname)
        } else {
            layout@box_list[[current]] <- layout_subtract(
                box, object, objectname
            )
        }
        layout
    }

#' @importFrom S7 S7_inherits
#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_subtract, list(StackLayout, quad_scope)) <-
    function(layout, object, objectname) {
        if (is.na(current <- layout@current) ||
            is_craftbox(box <- .subset2(layout@box_list, current))) {
            inner <- prop(object, "object")

            # subtract set at layout level, if it is a Scheme
            if (S7_inherits(inner, Scheme)) {
                layout <- update_layout_schemes(inner, layout, objectname)
            }

            # otherwise, we apply the object to all plots in the stack layout
            layout@box_list <- lapply(layout@box_list, function(box) {
                if (is_craftbox(box)) {
                    box <- chain_box_add(box, inner, objectname, force = FALSE)
                } else {
                    # we respect the context setting
                    box <- layout_subtract(box, object, objectname)
                }
                box
            })
        } else {
            layout@box_list[[current]] <- layout_subtract(
                box, object, objectname
            )
        }
        layout
    }

##################################################################
#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_propagate, list(ChainLayout, quad_scope)) <-
    function(layout, object, objectname) {
        object <- prop(object, "object")
        layout_propagate(layout, object, objectname)
    }

chain_propagate <- function(layout, object, objectname) {
    layout@box_list <- lapply(layout@box_list, function(box) {
        if (is_craftbox(box)) {
            box <- chain_box_add(box, object, objectname, force = FALSE)
        } else {
            box <- layout_propagate(box, object, objectname)
        }
        box
    })
    layout
}

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_propagate, list(ChainLayout, S7::class_any)) <-
    chain_propagate

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_propagate, list(ChainLayout, S3_class_theme)) <-
    function(layout, object, objectname) {
        ans <- chain_propagate(layout, object, objectname)
        # to align with `patchwork`, we also modify the layout theme
        # when using `&` to add the theme object.
        ans@theme <- ggfun("add_theme")(ans@theme, object)
        ans
    }
