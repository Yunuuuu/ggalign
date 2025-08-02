#' @keywords internal
chain_layout_subtract <- function(object, layout, object_name) {
    UseMethod("chain_layout_subtract")
}

#' @export
chain_layout_subtract.default <- function(object, layout, object_name) {
    if (is.na(current <- layout@current) ||
        !is_layout(box <- .subset2(layout@box_list, current))) {
        layout@box_list <- lapply(layout@box_list, function(box) {
            if (is_craftbox(box)) {
                chain_plot_add(box, object, object_name, force = FALSE)
            } else {
                box
            }
        })
    } else {
        layout@box_list[[current]] <- quad_layout_subtract(
            object, box, object_name
        )
    }
    layout
}

# for objects can inherit from layout
#' @export
`chain_layout_subtract.ggalign::Scheme` <- function(object, layout, object_name) {
    if (is.na(current <- layout@current) ||
        !is_layout(box <- .subset2(layout@box_list, current))) {
        layout <- update_layout_schemes(object, layout, object_name)
    } else {
        layout@box_list[[current]] <- quad_layout_subtract(
            object, box, object_name
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
    if (is.na(current <- layout@current) ||
        !is_layout(box <- .subset2(layout@box_list, current))) {
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
        layout@box_list <- lapply(layout@box_list, function(box) {
            if (is_craftbox(box)) {
                box <- chain_plot_add(box, inner, inner_name, force = FALSE)
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
                    if (!is.null(prop(box, position))) {
                        prop(box, position) <- chain_layout_subtract(
                            inner, prop(box, position), inner_name
                        )
                    }
                }
                if (is.null(main <- .subset2(object, "main")) || main) {
                    box <- quad_body_add(inner, box, inner_name)
                }
            } else {
                # we respect the context setting
                box <- quad_layout_subtract(object, box, object_name)
            }
            box
        })
    } else {
        layout@box_list[[current]] <- quad_layout_subtract(
            object, box, object_name
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
    layout@box_list <- lapply(layout@box_list, function(box) {
        if (is_craftbox(box)) {
            box <- chain_plot_add(box, object, object_name, force = FALSE)
        } else {
            box <- quad_layout_and_add(object, box, object_name)
        }
        box
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
