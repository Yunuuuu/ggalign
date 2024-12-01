new_ggalign_plot <- function(..., plot = NULL, active = NULL, size = NULL,
                             controls = NULL, class = character(),
                             call = caller_call()) {
    if (!inherits(active, "ggalign_active")) {
        cli_abort("{.arg active} must be created by {.fn active}", call = call)
    }
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    structure(
        list(...,
            controls = controls %||% new_controls(),
            plot = plot,
            active = active,
            size = size
        ),
        class = c(class, "ggalign_plot")
    )
}

is_ggalign_plot <- function(x) inherits(x, "ggalign_plot")

#' @export
#' @keywords internal
plot.ggalign_plot <- function(x, ...) {
    cli_abort("You cannot plot {.obj_type_friendly {x}} object directly")
}

plot_build <- function(plot, ...) UseMethod("plot_build")

# add object to the plot
plot_add <- function(plot, object, object_name) UseMethod("plot_add")

ggalign_plot_add <- function(object, plot, object_name) {
    UseMethod("ggalign_plot_add")
}

#' @export
plot_add.ggalign_plot <- function(plot, object, object_name) {
    ggalign_plot_add(object, plot, object_name)
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggalign_plot_add.default <- function(object, plot, object_name) {
    plot$plot <- ggplot_add(object, .subset2(plot, "plot"), object_name)
    plot
}

#' @export
ggalign_plot_add.ggalign_option <- function(object, plot, object_name) {
    name <- ggalign_option_name(object)
    plot$controls[name] <- list(update_option(
        object, .subset2(.subset2(plot, "controls"), name), object_name
    ))
    plot
}
