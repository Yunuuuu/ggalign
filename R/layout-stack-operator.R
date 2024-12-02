#' @keywords internal
stack_layout_subtract <- function(object, stack, object_name) {
    UseMethod("stack_layout_subtract")
}

#' @export
stack_layout_subtract.default <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        stack@plot_list <- lapply(stack@plot_list, function(plot) {
            if (is_ggalign_plot(plot)) {
                stack_plot_add(plot, object, object_name, force = FALSE)
            } else {
                plot
            }
        })
    } else {
        stack@plot_list[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    }
    stack
}

# for objects can inherit from layout
#' @export
stack_layout_subtract.ggalign_option <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        stack <- update_layout_option(object, stack, object_name)
    } else {
        stack@plot_list[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    }
    stack
}

#' @export
stack_layout_subtract.ggalign_with_quad <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
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
        stack@plot_list <- lapply(stack@plot_list, function(plot) {
            if (is_ggalign_plot(plot)) {
                plot <- stack_plot_add(plot, inner, inner_name, force = FALSE)
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
            plot
        })
    } else {
        stack@plot_list[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    }
    stack
}

##################################################################
#' @keywords internal
stack_layout_and_add <- function(object, stack, object_name) {
    UseMethod("stack_layout_and_add")
}

#' @export
stack_layout_and_add.default <- function(object, stack, object_name) {
    stack@plot_list <- lapply(stack@plot_list, function(plot) {
        if (is_ggalign_plot(plot)) {
            plot <- stack_plot_add(plot, object, object_name, force = FALSE)
        } else {
            plot <- quad_layout_and_add(object, plot, object_name)
        }
        plot
    })
    stack
}

#' @export
stack_layout_and_add.ggalign_with_quad <- function(object, stack, object_name) {
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
