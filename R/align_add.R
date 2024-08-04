####################################################################
# Following methods are used to add elements to `Align` object
align_add <- function(object, align, object_name) UseMethod("align_add")

#' @export
align_add.gg <- function(object, align, object_name) {
    if (is.null(plot <- .subset2(align, "plot"))) {
        cli::cli_abort(paste(
            "Can't add {.code {object_name}} to a",
            "{.fn {snake_class(align)}} plot"
        ), call = .subset2(align, "call"))
    }
    align$plot <- ggplot2::ggplot_add(object, plot, object_name)
    align
}

#' @export
align_add.labels <- align_add.gg

#' @export
align_add.facetted_pos_scales <- function(object, align, object_name) {
    assert_facetted_scales(
        object, object_name,
        sprintf("a %s plot", style_fn(snake_class(align)))
    )
    align$facetted_pos_scales <- object
    align
}
