#' Change the layer adding order
#'
#' This function allows you to change the order in which layers are added to a
#' ggplot.
#'
#' @param layer A [layer geometry][ggplot2::layer_geoms] object to be added.
#' @param order An integer indicating the position at which the layer should be
#' added. If `<= 0`, the layer will be added at the beginning. If greater than
#' the number of plot layers, it will be added at the end.
#' @return A `layer_order` object.
#' @examples
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'     geom_raster(aes(fill = density)) +
#'     geom_point(color = "red", size = 1)
#' ggplot(faithfuld, aes(waiting, eruptions)) +
#'     geom_raster(aes(fill = density)) +
#'     layer_order(geom_point(color = "red", size = 1))
#' @export
layer_order <- function(layer, order = 0) {
    assert_s3_class(layer, "Layer")
    if (!is_scalar(order)) {
        cli::cli_abort("{.arg order} must be a single number")
    }
    if (is.na(order)) {
        cli::cli_abort("{.arg order} cannot be missing value")
    }
    if (!is.infinite(order)) order <- vec_cast(order, integer())
    layer <- add_class(layer, "layer_order")
    attr(layer, "layer_order") <- list(
        order = order,
        # used for `ggplot_add`
        object_name = deparse(substitute(layer))
    )
    layer
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.layer_order <- function(object, plot, object_name) {
    if (is.null(params <- attr(object, "layer_order"))) {
        cli::cli_abort("Invalid {.cls layer_order} object")
    }
    # ggplot2 will do something special for the layer
    # add layer_name, we re-call the method for the layer
    object_name <- .subset2(params, "object_name")
    ans <- NextMethod()
    if ((cur <- length(layers <- .subset2(ans, "layers"))) == 1L) {
        return(ans)
    }
    layer <- .subset2(layers, cur)
    order <- .subset2(params, "order")
    if (order >= length(layers)) return(ans) # styler: off
    if (order <= 0L) {
        layers <- append(vec_slice(layers, -cur), layer, 0L)
    } else {
        layers <- append(vec_slice(layers, -cur), layer, order)
    }
    ans$layers <- layers
    ans
}
