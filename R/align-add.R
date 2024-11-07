####################################################################
# Following methods are used to add elements to `align` object
align_add <- function(object, align, object_name) UseMethod("align_add")

#' @importFrom ggplot2 ggplot_add
#' @export
align_add.default <- function(object, align, object_name) {
    if (is.null(plot <- .subset2(align, "plot"))) {
        cli::cli_abort(paste(
            "Can't add {.code {object_name}} to a",
            "{.fn {snake_class(align)}} plot"
        ), call = .subset2(align, "call"))
    }
    align$plot <- ggplot_add(object, plot, object_name)
    align
}

#' @export
align_add.Coord <- function(object, align, object_name) {
    if (!inherits(object, "CoordCartesian")) {
        cli::cli_warn(c(
            "only {.field Cartesian coordinate} is supported",
            i = "will discard {.fn {snake_class(object)}} directly"
        ))
        return(align)
    }
    NextMethod() # call default method
}

#' @export
align_add.ggalign_option <- function(object, align, object_name) {
    name <- ggalign_option_name(object)
    align$controls[name] <- list(update_option(
        object, .subset2(.subset2(align, "controls"), name), object_name
    ))
    align
}
