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
#' @importFrom S7 new_object S7_object S7_inherits prop
#' @export
layer_order <- S7::new_class(
    "layer_order",
    properties = list(
        layer = ggplot2::class_layer,
        order = S7::new_property(
            S7::class_numeric,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
            }
        )
    ),
    constructor = function(layer, order = 0) {
        if (S7_inherits(layer, layer_order)) {
            prop(layer, "order") <- order
            return(layer)
        }
        new_object(S7_object(), layer = layer, order = order)
    }
)

#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(layer_order, ggplot2::class_ggplot)) <-
    function(object, plot, objectname, ...) {
        # ggplot2 will do something special for the layer
        # add layer_name, we re-call the method for the layer
        ans <- update_ggplot(prop(object, "layer"), plot, objectname, ...)
        if ((cur <- length(layers <- ans$layers)) == 1L) {
            return(ans)
        }
        order <- prop(object, "order")
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
