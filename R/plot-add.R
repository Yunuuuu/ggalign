plot_add <- function(plot, object, object_name) {
    if (is.null(plot@plot)) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(plot)),
            i = sprintf("no plot found for %s", object_name(plot))
        ))
    }
    UseMethod("plot_add")
}

#' @export
plot_add.ggalign_align_plot <- function(plot, object, object_name) {
    align_add(object, plot, object_name)
}

#' @export
plot_add.ggalign_free_plot <- function(plot, object, object_name) {
    free_add(object, plot, object_name)
}

####################################################################
# Following methods are used to add elements to `align` object
align_add <- function(object, plot, object_name) UseMethod("align_add")

#' @importFrom ggplot2 ggplot_add
#' @export
align_add.default <- function(object, plot, object_name) {
    plot@plot <- ggplot_add(object, plot@plot, object_name)
    plot
}

#' @export
align_add.Coord <- function(object, plot, object_name) {
    if (!inherits(object, "CoordCartesian")) {
        cli_warn(c(
            "only {.field Cartesian coordinate} is supported",
            i = "will discard {.fn {snake_class(object)}} directly"
        ))
        return(plot)
    }
    NextMethod() # call default method
}

#' @export
align_add.ggalign_scheme <- function(object, plot, object_name) {
    name <- ggalign_scheme_name(object)
    plot@schemes[name] <- list(update_scheme(
        object, .subset2(plot@schemes, name), object_name
    ))
    plot
}

##################################################################
free_add <- function(object, plot, object_name) UseMethod("free_add")

#' @importFrom ggplot2 ggplot_add
#' @export
free_add.default <- align_add.default

#' @export
free_add.ggalign_scheme <- align_add.ggalign_scheme
