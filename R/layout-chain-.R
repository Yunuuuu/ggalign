#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(ChainLayout, CraftBox)) <-
    function(x, object, objectname) {
        if (is.na(current <- x@current) ||
            is_craftbox(box <- .subset2(x@box_list, current))) {
            craftsman <- prop(object, "craftsman")
            # initialize the necessary parameters for `Craftsman` object
            if (is_stack_layout(x)) {
                craftsman$direction <- x@direction
                craftsman$position <- .subset2(x@heatmap, "position")
            } else if (is_circle_layout(x)) {
                # we treat circle layout as a vertical stack layout
                craftsman$direction <- "vertical"
            }
            craftsman$in_linear <- is_linear(x)
            craftsman$layout_name <- object_name(x)

            # firstly, we let the object do some changes in the layout
            x <- craftsman$interact_layout(x)

            # this step, the object will act with the stack layout
            # group rows into panel or reorder rows, we can also
            # initialize object data
            new_domain <- craftsman$setup_domain(x@domain)

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
            prop(object, "plot") <- craftsman$init_plot(plot)

            x <- chain_add_box(x, object, object@active, objectname)
        } else { # should be a QuadLayout object
            box <- ggalign_update(box, object, objectname)
            x@box_list[[current]] <- box
            new_domain <- prop(box, x@direction)
        }
        layout_update_domain(x,
            domain = new_domain, objectname = objectname
        )
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(ChainLayout, ggplot2::class_ggplot)) <-
    function(x, object, objectname) {
        ggalign_update(x, ggfree(data = object), objectname)
    }

#' @include layout-.R
S7::method(ggalign_update, list(ChainLayout, layout_title)) <-
    function(x, object, objectname) {
        x@titles <- x@titles + object
        x
    }

#' @include layout-.R
S7::method(ggalign_update, list(ChainLayout, S7::class_list)) <-
    function(x, object, objectname) {
        for (o in object) x <- ggalign_update(x, o, object_name)
        x
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(ChainLayout, S7::class_any)) <-
    function(x, object, objectname) {
        if (is.null(object)) return(x) # styler: off
        if (is.na(current <- x@current)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
                    object_name(x)
                ),
                i = "No active plot component",
                i = paste(
                    "Did you forget to initialize a {.cls ggplot} object",
                    "with {.fn ggalign} or {.fn ggfree}?"
                )
            ))
        }
        box <- .subset2(x@box_list, current)
        if (is_craftbox(box)) {
            box <- chain_box_add(box, object, objectname, TRUE)
        } else {
            box <- ggalign_update(box, object, objectname)
        }
        x@box_list[[current]] <- box
        x
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(ChainLayout, layout_theme)) <-
    function(x, object, objectname) {
        if (is.na(current <- x@current) ||
            is_craftbox(box <- .subset2(x@box_list, current))) {
            prop(x, "theme") <- prop(x, "theme") + prop(object, "theme")
        } else {
            x@box_list[[current]] <- ggalign_update(box, object, objectname)
        }
        x
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
S7::method(ggalign_update, list(StackLayout, quad_scope)) <-
    S7::method(
        ggalign_update,
        list(StackLayout, S7::new_S3_class("quad_active"))
    ) <-
    S7::method(
        ggalign_update,
        list(StackLayout, S7::new_S3_class("quad_anno"))
    ) <-
    S7::method(ggalign_update, list(StackLayout, StackLayout)) <-
    function(x, object, objectname) {
        if (is.na(current <- x@current) ||
            is_craftbox(box <- .subset2(x@box_list, current))) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
                    object_name(x)
                ),
                i = "Did you forget to add a {.fn quad_layout}?"
            ))
        } else {
            x@box_list[[current]] <- ggalign_update(box, object, objectname)
        }
        x
    }

S7::method(ggalign_update, list(CircleLayout, quad_scope)) <-
    S7::method(ggalign_update, list(CircleLayout, QuadLayout)) <-
    S7::method(
        ggalign_update,
        list(CircleLayout, S7::new_S3_class("quad_active"))
    ) <-
    S7::method(
        ggalign_update,
        list(CircleLayout, S7::new_S3_class("quad_anno"))
    ) <-
    S7::method(ggalign_update, list(CircleLayout, StackLayout)) <-
    function(x, object, objectname) {
        cli_abort(c(
            sprintf("Cannot add %s to a {.fn circle_layout}", objectname),
            i = "Try to use {.fn stack_layout} instead"
        ))
    }

#' @include layout-.R
#' @include layout-operator.R
#' @importFrom S7 super
S7::method(ggalign_update, list(StackLayout, StackCross)) <-
    function(x, object, objectname) {
        # preventing from adding `stack_cross` with the same direction in this
        # way, `stack_cross()` cannot be added to the heatmap annotation
        # parallelly with the `stack_layout()`
        if (identical(object@direction, x@direction)) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(x)),
                i = "Cannot add {.fn stack_cross} with the same direction as {.fn stack_discrete}."
            ))
        }
        # call StackLayout method
        ggalign_update(x, super(object, StackLayout), objectname)
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(StackLayout, S7::new_S3_class("stack_switch"))) <-
    function(x, object, objectname) {
        x <- switch_chain_plot(
            x, .subset2(object, "what"),
            quote(stack_switch())
        )
        if (!is.null(sizes <- .subset2(object, "sizes"))) {
            x@sizes <- sizes
        }
        x
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(CircleLayout, S7::new_S3_class("stack_switch"))) <-
    function(x, object, objectname) {
        cli_abort(c(
            sprintf("Cannot add {.var {objectname}} to %s", object_name(x)),
            i = "Did you want to add a {.fn circle_switch}?"
        ))
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(StackLayout, QuadLayout)) <-
    function(x, object, objectname) {
        # preventing from adding `stack_cross` with the same direction
        # `cross_link()` cannot be added to the heatmap annotation
        # parallelly with the `stack_cross()`
        if (is_horizontal(direction <- x@direction)) {
            if (is_cross_layout(object@left) || is_cross_layout(object@right)) {
                cli_abort(c(
                    sprintf(
                        "Cannot add {.var {objectname}} to %s",
                        object_name(x)
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
                    object_name(x)
                ),
                i = sprintf(
                    "{.field top} or {.field bottom} annotation contains %s",
                    "{.fn stack_cross}"
                )
            ))
        }

        # check quad layout is compatible with stack layout
        quad_data <- object@data
        stack_domain <- x@domain
        quad_domain <- prop(object, direction)
        if (!is_discrete_domain(quad_domain)) {
            if (is_discrete_domain(stack_domain)) {
                cli_abort(c(
                    sprintf(
                        "Cannot add %s to %s",
                        object_name(object), object_name(x)
                    ),
                    i = sprintf(
                        "%s cannot align continuous variable",
                        object_name(x)
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
                if (is.null(stack_data <- x@data)) {
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
                                object_name(x)
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
                                    object_name(object), object_name(x)
                                ),
                                i = sprintf(
                                    "{.arg data} in %s is %s, but %s need a {.cls matrix}.",
                                    object_name(x),
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
                if (is.null(stack_data <- x@data)) {
                    cli_abort(c(
                        sprintf(
                            "you must provide {.arg data} argument in %s",
                            object_name(object)
                        ),
                        i = sprintf(
                            "no data was found in %s",
                            object_name(x)
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
                                object_name(x), object_name(object)
                            ),
                            i = sprintf(
                                "{.arg data} in %s is an empty matrix",
                                object_name(x)
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
                old_name = object_name(x),
                new_name = object_name
            )
        } else {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(object), object_name(x)
                ),
                i = sprintf(
                    "%s cannot align discrete variable",
                    object_name(x)
                )
            ))
        }
        stack <- chain_add_box(x, object, object@plot_active, object_name)
        layout_update_domain(
            stack,
            domain = layout_domain, objectname = object_name
        )
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(CircleLayout, S7::new_S3_class("circle_switch"))) <-
    function(x, object, objectname) {
        if (!is_waiver(radial <- .subset2(object, "radial"))) {
            x@radial <- radial
        }
        if (!is.null(direction <- .subset2(object, "direction"))) {
            x@direction <- direction
        }
        x <- switch_chain_plot(
            x, .subset2(object, "what"),
            quote(circle_switch())
        )
        x
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(ggalign_update, list(StackLayout, S7::new_S3_class("circle_switch"))) <-
    function(x, object, objectname) {
        cli_abort(c(
            sprintf("Cannot add {.var {objectname}} to %s", object_name(x)),
            i = "Did you want to add a {.fn stack_switch}?"
        ))
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_apply_selected, list(ChainLayout, S7::class_any)) <-
    function(x, object, objectname) {
        if (is.na(current <- x@current) ||
            is_craftbox(box <- .subset2(x@box_list, current))) {
            x@box_list <- lapply(x@box_list, function(box) {
                if (is_craftbox(box)) {
                    chain_box_add(box, object, objectname, force = FALSE)
                } else {
                    box
                }
            })
        } else {
            x@box_list[[current]] <- layout_apply_selected(
                box, object, objectname
            )
        }
        x
    }

# for objects can inherit from layout
#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_apply_selected, list(ChainLayout, Scheme)) <-
    function(x, object, ...) {
        if (is.na(current <- x@current) ||
            is_craftbox(box <- .subset2(x@box_list, current))) {
            prop(x, "schemes") <- ggalign_update(
                prop(x, "schemes"), object, ...
            )
        } else {
            x@box_list[[current]] <- layout_apply_selected(
                box, object, ...
            )
        }
        x
    }

#' @importFrom S7 S7_inherits
#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_apply_selected, list(StackLayout, quad_scope)) <-
    function(x, object, ...) {
        if (is.na(current <- x@current) ||
            is_craftbox(box <- .subset2(x@box_list, current))) {
            inner <- prop(object, "object")

            # subtract set at layout level, if it is a Scheme
            if (S7_inherits(inner, Scheme)) {
                prop(x, "schemes") <- ggalign_update(
                    prop(x, "schemes"), inner, ...
                )
            }

            # otherwise, we apply the object to all plots in the stack layout
            x@box_list <- lapply(x@box_list, function(box) {
                if (is_craftbox(box)) {
                    box <- chain_box_add(box, inner, ..., force = FALSE)
                } else {
                    # we respect the context setting
                    box <- layout_apply_selected(box, object, ...)
                }
                box
            })
        } else {
            x@box_list[[current]] <- layout_apply_selected(box, object, ...)
        }
        x
    }

##################################################################
#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_apply_all, list(ChainLayout, quad_scope)) <-
    function(x, object, ...) {
        layout_apply_all(x, prop(object, "object"), ...)
    }

chain_apply_all <- function(x, object, objectname) {
    x@box_list <- lapply(x@box_list, function(box) {
        if (is_craftbox(box)) {
            box <- chain_box_add(box, object, objectname, force = FALSE)
        } else {
            box <- layout_apply_all(box, object, objectname)
        }
        box
    })
    x
}

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_apply_all, list(ChainLayout, S7::class_any)) <-
    chain_apply_all

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_apply_all, list(ChainLayout, ggplot2::class_theme)) <-
    function(x, object, ...) {
        ans <- chain_apply_all(x, object, ...)
        # to align with `patchwork`, we also modify the layout theme
        # when using `&` to add the theme object.
        ans@theme <- ggfun("add_theme")(ans@theme, object)
        ans
    }
