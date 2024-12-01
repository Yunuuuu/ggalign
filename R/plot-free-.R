new_free_plot <- function(..., class = character()) {
    new_ggalign_plot(..., class = c(class, "ggalign_free_plot"))
}

#' Initialize the plot
#'
#' Used by `ggalign_free_plot`
#' @noRd
plot_initialize <- function(object, layout, object_name) {
    UseMethod("plot_initialize")
}

#' @export
stack_layout_add.ggalign_free_plot <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        object <- plot_initialize(object, stack)
        stack <- stack_add_plot(
            stack, object,
            .subset2(.subset2(object, "active"), "use"),
            .subset2(.subset2(object, "active"), "name"),
            object_name
        )
    } else {
        plot <- quad_layout_add(object, plot, object_name)
        stack@plot_list[[active_index]] <- plot
    }
    stack
}

#' @export
stack_layout_add.ggplot <- function(object, stack, object_name) {
    stack_layout_add(free_gg(data = object), stack, object_name)
}

#' @export
plot_build.ggalign_free_plot <- function(plot, ..., controls) {
    plot_add_controls(.subset2(plot, "plot"), controls)
}
