rasterise.QuadLayout <- function(input, ...) {
    input@plot <- ggrastr::rasterise(input = input@plot, ...)
    for (position in .TLBR) {
        stack <- slot(input, position)
        if (is.null(stack)) next
        slot(input, position) <- ggrastr::rasterise(input = stack, ...)
    }
    input
}

rasterise.ChainLayout <- function(input, ...) {
    input@plot_list <- lapply(input@plot_list, ggrastr::rasterise, ...)
    input
}

rasterise.CraftBox <- function(input, ...) {
    if (!is.null(plot <- input@plot)) {
        input@plot <- ggrastr::rasterise(input = plot, ...)
    }
    input
}

##########################################################
#' @export
raster_magick0.QuadLayout <- function(x, ...) {
    x@plot <- raster_magick0(x = x@plot, ...)
    for (position in .TLBR) {
        stack <- slot(x, position)
        if (is.null(stack)) next
        slot(x, position) <- raster_magick0(x = stack, ...)
    }
    x
}

#' @export
raster_magick0.ChainLayout <- function(x, ...) {
    x@plot_list <- lapply(x@plot_list, raster_magick0, ...)
    x
}


#' @export
raster_magick0.CraftBox <- function(x, ...) {
    if (!is.null(plot <- input@plot)) {
        input@plot <- raster_magick0(x = plot, ...)
    }
    input
}
