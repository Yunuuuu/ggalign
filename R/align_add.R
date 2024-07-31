####################################################################
# Following methods are used to add elements to `Align` object
align_add <- function(object, align, object_name) UseMethod("align_add")

#' @export
align_add.gg <- function(object, align, object_name) {
    if (is.null(plot <- .subset2(align, "plot"))) {
        cli::cli_abort(paste(
            "Can't add {.var {object_name}} to a",
            "{.fn {snake_class(align)}} plot"
        ), call = .subset2(align, "call"))
    }
    align$plot <- ggplot2::ggplot_add(object, plot, object_name)
    align
}

#' @export
align_add.labels <- align_add.gg

#' @export
align_add.facetted_pos_scales <- function(object, htanno, object_name) {
    htanno$facetted_pos_scales <- object
    htanno
}
