#' Finalize plot modifications from a ChainLayout object.
#'
#' This generic function lets a ChainLayout apply any final transformations
#' to the composed plot before returning it. It does not extract or store
#' the plot, but instead allows the layout to inject custom modifications
#' (e.g., spacing guides, annotations, alignment fixes) at the last step.
#'
#' @param layout A `ChainLayout` object.
#' @param plot The plot being finalized.
#' @keywords internal
chain_decorate <- function(layout, plot) UseMethod("chain_decorate")

#' @export
chain_decorate.ChainLayout <- function(layout, plot) plot

#############################################################
#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, CraftBox)) <-
    function(layout, object, objectname) {
        if (is.null(current <- layout@current) ||
            !is_layout(box <- .subset2(layout@plot_list, current))) {
            craftsman <- prop(object, "craftsman")
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

            layout <- chain_add_plot(layout, object, object@active, objectname)
        } else { # should be a QuadLayout object
            box <- layout_add(box, object, objectname)
            # use attributes to bypass the prop setter checking
            attr(layout, "box_list")[[current]] <- box
            new_domain <- slot(box, layout@direction)
        }
        layout_update_domain(
            layout,
            domain = new_domain,
            objectname = objectname
        )
    }

#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, ContinuousDomain)) <-
    function(layout, object, objectname) {
        layout_update_domain(layout, domain = object, objectname = objectname)
    }

#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, class_S3_gg)) <-
    function(layout, object, objectname) {
        layout_add(layout, ggfree(data = object), objectname = objectname)
    }

#' @importFrom S7 S7_inherits
#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, S7::class_any)) <-
    function(layout, object, objectname) {
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
        if (S7_inherits(box, LayoutProto)) {
            box <- layout_add(box, object, objectname)
        } else {
            box <- chain_box_add(box, object, objectname, TRUE)
        }
        # use attributes to bypass the prop setter checking
        attr(layout, "box_list")[[current]] <- box
        layout
    }

#' @include layout-operator.R
S7::method(layout_add, list(ChainLayout, S3_layout_theme)) <-
    function(layout, object, objectname) {
        if (is.null(current <- layout@current) ||
            !is_layout(box <- .subset2(layout@box_list, current))) {
            layout@theme <- object
        } else {
            # use attributes to bypass the prop setter checking
            attr(layout, "box_list")[[current]] <- layout_add(
                box, object, object_name
            )
        }
        layout
    }

chain_box_add <- function(plot, object, object_name, force) {
    # if `align` has plot, we added the object
    if (force || !is.null(plot@plot)) {
        plot <- craftbox_add(object, plot, object_name)
    }
    plot
}

#' @importFrom S7 prop
chain_add_plot <- function(layout, plot, active, object_name) {
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
        box_list[[name]] <- plot
    } else {
        box_list <- c(box_list, list(plot))
    }

    # use attributes to bypass the prop setter checking
    attr(layout, "box_list") <- box_list
    attr(layout, "current") <- current
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
        layout@current <- what
    }
    layout
}

##################################################
#' @include layout-operator.R
S7::method(layout_subtract, list(LayoutProto, S7::class_any)) <-
    function(layout, object, objectname) {
        if (is.null(current <- layout@current) ||
            !is_layout(box <- .subset2(layout@box_list, current))) {
            attr(layout, "box_list") <- lapply(layout@box_list, function(box) {
                if (!is_layout(box)) {
                    chain_box_add(box, object, objectname, force = FALSE)
                } else {
                    box
                }
            })
        } else {
            attr(layout, "box_list")[[current]] <- layout_subtract(
                box, object, objectname
            )
        }
        layout
    }

# for objects can inherit from layout
#' @include layout-operator.R
S7::method(layout_subtract, list(LayoutProto, Scheme)) <-
    function(layout, object, objectname) {
        if (is.null(current <- layout@active) ||
            !is_layout(box <- .subset2(layout@box_list, current))) {
            layout <- update_layout_schemes(object, layout, objectname)
        } else {
            attr(layout, "box_list")[[current]] <- layout_subtract(
                box, object, objectname
            )
        }
        layout
    }

#' @importFrom S7 S7_inherits
#' @export
chain_layout_subtract.ggalign_with_quad <- function(object, layout,
                                                    object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }
    if (is.null(active_index <- layout@active) ||
        is_craftbox(plot <- .subset2(layout@plot_list, active_index))) {
        inner <- .subset2(object, "object")
        inner_name <- .subset2(object, "object_name")

        # subtract set at layout level, if it is a plot option
        # we only apply to current active layout
        if (S7_inherits(inner, Scheme)) {
            layout <- update_layout_schemes(inner, layout, inner_name)
            return(layout)
        }

        # otherwise, we apply the object to all plots in the stack layout
        direction <- layout@direction
        layout@plot_list <- lapply(layout@plot_list, function(plot) {
            if (is_craftbox(plot)) {
                plot <- chain_box_add(plot, inner, inner_name, force = FALSE)
            } else if (is.waive(.subset2(object, "position"))) {
                # default behaviour for object wrap with `with_quad()`
                # we add the object along the stack layout
                # if means for horizontal stack, we'll add it
                # to the left and right annotation, and the main plot
                positions <- switch_direction(
                    direction,
                    c("left", "right"),
                    c("top", "bottom")
                )
                for (position in positions) {
                    if (!is.null(slot(plot, position))) {
                        slot(plot, position) <- chain_layout_subtract(
                            inner, slot(plot, position), inner_name
                        )
                    }
                }
                if (is.null(main <- .subset2(object, "main")) || main) {
                    plot <- quad_body_add(inner, plot, inner_name)
                }
            } else {
                # we respect the context setting
                plot <- quad_layout_subtract(object, plot, object_name)
            }
            plot
        })
    } else {
        layout@plot_list[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    }
    layout
}

##################################################################
#' @keywords internal
chain_layout_and_add <- function(object, layout, object_name) {
    UseMethod("chain_layout_and_add")
}

#' @export
chain_layout_and_add.ggalign_with_quad <- function(object, layout, object_name) {
    if (!is_stack_layout(layout)) {
        cli_abort(sprintf(
            "Cannot add {.var {object_name}} to %s",
            object_name(layout)
        ))
    }
    object_name <- .subset2(object, "object_name")
    object <- .subset2(object, "object")
    chain_layout_and_add(object, layout, object_name)
}

#' @export
chain_layout_and_add.default <- function(object, layout, object_name) {
    layout@plot_list <- lapply(layout@plot_list, function(plot) {
        if (is_craftbox(plot)) {
            plot <- chain_box_add(plot, object, object_name, force = FALSE)
        } else {
            plot <- quad_layout_and_add(object, plot, object_name)
        }
        plot
    })
    layout
}

#' @export
chain_layout_and_add.theme <- function(object, layout, object_name) {
    ans <- NextMethod()
    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    ans@theme <- ggfun("add_theme")(ans@theme, object)
    ans
}
