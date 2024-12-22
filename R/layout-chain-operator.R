#' @keywords internal
chain_layout_subtract <- function(object, layout, object_name) {
    UseMethod("chain_layout_subtract")
}

#' @export
chain_layout_subtract.default <- function(object, layout, object_name) {
    if (is.null(active_index <- layout@active) ||
        is_ggalign_plot(plot <- .subset2(layout@plot_list, active_index))) {
        layout@plot_list <- lapply(layout@plot_list, function(plot) {
            if (is_ggalign_plot(plot)) {
                chain_plot_add(plot, object, object_name, force = FALSE)
            } else {
                plot
            }
        })
    } else {
        layout@plot_list[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    }
    layout
}

# for objects can inherit from layout
#' @export
chain_layout_subtract.ggalign_scheme <- function(object, layout, object_name) {
    if (is.null(active_index <- layout@active) ||
        is_ggalign_plot(plot <- .subset2(layout@plot_list, active_index))) {
        layout <- update_layout_scheme(object, layout, object_name)
    } else {
        layout@plot_list[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    }
    layout
}

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
        is_ggalign_plot(plot <- .subset2(layout@plot_list, active_index))) {
        inner <- .subset2(object, "object")
        inner_name <- .subset2(object, "object_name")

        # subtract set at layout level, if it is a plot option
        # we only apply to current active layout
        if (inherits(inner, "ggalign_scheme")) {
            layout <- update_layout_scheme(inner, layout, inner_name)
            return(layout)
        }

        # otherwise, we apply the object to all plots in the stack layout
        direction <- layout@direction
        layout@plot_list <- lapply(layout@plot_list, function(plot) {
            if (is_ggalign_plot(plot)) {
                plot <- chain_plot_add(plot, inner, inner_name, force = FALSE)
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
chain_layout_and_add.default <- function(object, layout, object_name) {
    layout@plot_list <- lapply(layout@plot_list, function(plot) {
        if (is_ggalign_plot(plot)) {
            plot <- chain_plot_add(plot, object, object_name, force = FALSE)
        } else {
            plot <- quad_layout_and_add(object, plot, object_name)
        }
        plot
    })
    layout
}

#' @export
chain_layout_and_add.ggalign_with_quad <- function(object, layout, object_name) {
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    NextMethod()
}

#' @export
chain_layout_and_add.theme <- function(object, layout, object_name) {
    ans <- NextMethod()
    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    ans@theme <- ans@theme + object
    ans
}
