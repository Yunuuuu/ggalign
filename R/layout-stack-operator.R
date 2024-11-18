#' @keywords internal
stack_layout_subtract <- function(object, stack, object_name) {
    UseMethod("stack_layout_subtract")
}

#' @export
stack_layout_subtract.default <- function(object, stack, object_name) {
    active <- stack@active
    if (!is.null(active) &&
        stack_active_is_layout(plot <- .subset2(stack@plots, active))) {
        stack@plots[[active]] <- quad_layout_subtract(object, plot, object_name)
    } else {
        stack@plots <- lapply(stack@plots, function(plot) {
            if (stack_active_is_layout(plot)) {
                return(plot)
            }
            stack_plot_add(plot, object, object_name, force = FALSE)
        })
    }
    stack
}

# for objects can inherit from layout
#' @export
stack_layout_subtract.ggalign_option <- function(object, stack, object_name) {
    active <- stack@active
    if (!is.null(active) &&
        stack_active_is_layout(plot <- .subset2(stack@plots, active))) {
        stack@plots[[active]] <- quad_layout_subtract(object, plot, object_name)
    } else {
        stack <- update_layout_option(object, stack, object_name)
    }
    stack
}

#' @export
stack_layout_subtract.with_quad <- function(object, stack, object_name) {
    active <- stack@active
    if (!is.null(active) &&
        stack_active_is_layout(plot <- .subset2(stack@plots, active))) {
        stack@plots[[active]] <- quad_layout_subtract(object, plot, object_name)
    } else {
        inner <- .subset2(object, "object")
        inner_name <- .subset2(object, "object_name")

        # subtract set at layout level, if it is a plot option
        # we only apply to current active layout
        if (inherits(inner, "ggalign_option")) {
            stack <- update_layout_option(inner, stack, inner_name)
            return(stack)
        }

        # otherwise, we apply the object to all plots in the stack layout
        direction <- stack@direction
        stack@plots <- lapply(stack@plots, function(plot) {
            if (stack_active_is_layout(plot)) {
                if (is.waive(.subset2(object, "position"))) {
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
                            slot(plot, position) <- stack_layout_subtract(
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
            } else {
                plot <- stack_plot_add(plot, inner, inner_name, force = FALSE)
            }
            plot
        })
    }
    stack
}

# for objects should only be added with `+`
#' @export
stack_layout_subtract.layout_title <- function(object, stack, object_name) {
    cli_abort(c(
        "Cannot use {.code -} to add {.obj_type_friendly {object}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_subtract.ggplot <- stack_layout_subtract.layout_title

#' @export
stack_layout_subtract.free_gg <- stack_layout_subtract.layout_title

#' @export
stack_layout_subtract.quad_active <- stack_layout_subtract.ggplot

#' @export
stack_layout_subtract.quad_anno <- stack_layout_subtract.ggplot

#' @export
stack_layout_subtract.quad_init <- stack_layout_subtract.ggplot

#' @export
stack_layout_subtract.Align <- function(object, stack, object_name) {
    cli_abort(c(
        "Cannot use {.code -} to add {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_subtract.layout_annotation <- stack_layout_subtract.layout_title

##################################################################
#' @keywords internal
stack_layout_and_add <- function(object, stack, object_name) {
    UseMethod("stack_layout_and_add")
}

#' @export
stack_layout_and_add.default <- function(object, stack, object_name) {
    stack@plots <- lapply(stack@plots, function(plot) {
        if (stack_active_is_layout(plot)) {
            plot <- quad_layout_and_add(object, plot, object_name)
        } else {
            plot <- stack_plot_add(plot, object, object_name, force = FALSE)
        }
        plot
    })
    stack
}

#' @export
stack_layout_and_add.with_quad <- function(object, stack, object_name) {
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    NextMethod()
}

#' @export
stack_layout_and_add.theme <- function(object, stack, object_name) {
    ans <- NextMethod()
    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    ans@theme <- ans@theme + object
    ans
}

#' @export
stack_layout_and_add.layout_title <- function(object, stack, object_name) {
    cli_abort(c(
        "Cannot use {.code &} to add {.obj_type_friendly {object}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_and_add.ggplot <- stack_layout_and_add.layout_title

#' @export
stack_layout_and_add.free_gg <- stack_layout_and_add.layout_title

#' @export
stack_layout_and_add.Align <- function(object, stack, object_name) {
    cli_abort(c(
        "Cannot use {.code &} to add {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_and_add.layout_annotation <- stack_layout_and_add.layout_title
