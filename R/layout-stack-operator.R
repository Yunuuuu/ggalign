#' @keywords internal
stack_layout_subtract <- function(object, stack, object_name) {
    UseMethod("stack_layout_subtract")
}

#' @export
stack_layout_subtract.default <- function(object, stack, object_name) {
    stack@plots <- lapply(stack@plots, function(plot) {
        if (is_quad_layout(plot)) {
            plot <- quad_layout_subtract(object, plot, object_name)
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
stack_layout_subtract.ggplot <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code -} to add a {.cls ggplot}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_subtract.Align <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code -} to add a {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}

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
stack_layout_and_add.ggplot <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code &} to add a {.cls ggplot}",
        i = "Try to use {.code +} instead"
    ))
}

#' @export
stack_layout_and_add.Align <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot use {.code &} to add a {.fn {snake_class(object)}}",
        i = "Try to use {.code +} instead"
    ))
}
