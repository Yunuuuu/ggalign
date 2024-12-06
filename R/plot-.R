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
