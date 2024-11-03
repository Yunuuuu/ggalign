#' @keywords internal
stack_layout_subtract <- function(object, stack, object_name) {
    UseMethod("stack_layout_subtract")
}

#' @export
stack_layout_subtract.default <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_layout(plot <- stack@plots[[active_index]])) {
        stack@plots[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    } else {
        stack@plots <- lapply(stack@plots, function(plot) {
            if (is_free(plot)) {
                plot <- free_add(object, plot, object_name)
            } else if (is_align(plot) && !is.null(.subset2(plot, "plot"))) {
                # if `align` has plot, we added the object
                plot <- align_add(object, plot, object_name)
            } else if (is_quad_layout(plot)) {
                # we always apply to all plots of
                plot <- quad_layout_subtract(
                    with_position(object, NULL),
                    plot, object_name
                )
            }
            plot
        })
    }
    stack
}

# for objects can inherit from layout
#' @export
stack_layout_subtract.theme <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_layout(plot <- stack@plots[[active_index]])) {
        stack@plots[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    } else {
        stack <- update_layout_option_theme(object, stack, object_name)
    }
    stack
}

#' @export
stack_layout_subtract.ggalign_controls <- function(object, stack, object_name) {
    if (!is.null(active_index <- stack@active) &&
        is_layout(plot <- stack@plots[[active_index]])) {
        stack@plots[[active_index]] <- quad_layout_subtract(
            object, plot, object_name
        )
    } else {
        stack <- update_layout_option(object, stack, object_name)
    }
    stack
}

# for objects should only be added with `+`
#' @export
stack_layout_subtract.layout_title <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code -} to add {.obj_type_friendly {object}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_subtract.ggplot <- stack_layout_subtract.layout_title

#' @export
stack_layout_subtract.free_gg <- stack_layout_subtract.layout_title

#' @export
stack_layout_subtract.Align <- function(object, stack, object_name) {
    cli::cli_abort(c(
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
        if (is_quad_layout(plot)) {
            plot <- quad_layout_and_add(object, plot, object_name)
        } else if (is_free(plot)) {
            plot <- free_add(object, plot, object_name)
        } else if (is_align(plot) && !is.null(.subset2(plot, "plot"))) {
            # if `align` has plot, we added the object
            plot <- align_add(object, plot, object_name)
        }
        plot
    })
    stack
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
    cli::cli_abort(c(
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
    cli::cli_abort(c(
        "Cannot use {.code &} to add {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_and_add.layout_annotation <- stack_layout_and_add.layout_title
