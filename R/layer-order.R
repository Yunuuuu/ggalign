#' Change the layer adding order
#'
#' @description
#' This function allows you to change the order in which layers are added to a
#' ggplot.
#'
#' @param layer A [`layer geometry`][ggplot2::layer_geoms] object to be added.
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
    assert_number_decimal(order)
    UseMethod("layer_order")
}

#' @export
layer_order.default <- function(layer, order = 0) {
    cli_abort("{.arg layer} must be a {.fn geom_*} object")
}

#' @export
layer_order.Layer <- function(layer, order = 0) {
    if (!is.infinite(order)) order <- vec_cast(order, integer())
    structure(
        list(
            object = layer,
            order = order,
            # used for `ggplot_add`
            object_name = deparse(substitute(layer))
        ),
        class = "ggalign_layer_order"
    )
}

#' @export
layer_order.ggalign_layer_order <- function(layer, order = 0) {
    if (!is.infinite(order)) order <- vec_cast(order, integer())
    layer$order <- order
    layer
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggalign_layer_order <- function(object, plot, object_name) {
    # ggplot2 will do something special for the layer
    # add layer_name, we re-call the method for the layer
    ans <- ggplot_add(
        .subset2(object, "object"),
        plot, .subset2(object, "object_name")
    )
    if ((cur <- length(layers <- .subset2(ans, "layers"))) == 1L) {
        return(ans)
    }
    order <- .subset2(object, "order")
    layer <- .subset2(layers, cur)
    if (order >= length(layers)) return(ans) # styler: off
    if (order <= 0L) {
        layers <- append(vec_slice(layers, -cur), layer, 0L)
    } else {
        layers <- append(vec_slice(layers, -cur), layer, order)
    }
    ans$layers <- layers
    ans
}
