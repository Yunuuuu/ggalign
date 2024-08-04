#' @keywords internal
layout_stack_and_add <- function(object, stack, object_name) {
    UseMethod("layout_stack_and_add")
}

#' @export
layout_stack_and_add.default <- function(object, stack, object_name) {
    cli::cli_abort(
        "Cannot add {.code {object_name}} to stack and annotations"
    )
}

#' @export
layout_stack_and_add.gg <- function(object, stack, object_name) {
    slot(stack, "plots") <- lapply(
        slot(stack, "plots"),
        function(plot) {
            if (is.ggheatmap(plot)) {
                plot <- layout_heatmap_and_add(object, plot, object_name)
            } else if (!is.null(.subset2(plot, "plot"))) {
                # if `align` has plot, we added the object
                plot <- align_add(object, plot, object_name)
            }
            plot
        }
    )
    stack
}

#' @export
layout_stack_and_add.ggplot <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot add {.code {object_name}} into the stack layout",
        i = "try to use {.fn ggalign} to initialize a {.cls ggplot} object"
    ))
}

#' @export
layout_stack_and_add.labels <- layout_stack_and_add.gg

#' @export
layout_stack_and_add.facetted_pos_scales <- layout_stack_and_add.gg

#' @export
layout_stack_and_add.NULL <- layout_stack_add.NULL
