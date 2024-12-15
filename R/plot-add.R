plot_add <- function(object, plot, object_name) {
    if (is.null(plot@plot)) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(plot)),
            i = sprintf("no plot found for %s", object_name(plot))
        ))
    }
    UseMethod("plot_add")
}

#' @importFrom ggplot2 ggplot_add
#' @export
plot_add.default <- function(object, plot, object_name) {
    plot@plot <- ggplot_add(object, plot@plot, object_name)
    plot
}

#' @export
plot_add.ggalign_scheme <- function(object, plot, object_name) {
    name <- ggalign_scheme_name(object)
    plot@schemes[name] <- list(update_scheme(
        object, .subset2(plot@schemes, name), object_name
    ))
    plot
}
