rasterise.QuadLayout <- function(input, ...) {
    input@plot <- ggrastr::rasterise(input = input@plot, ...)
    for (position in .TLBR) {
        stack <- slot(input, position)
        if (is.null(stack)) next
        slot(input, position) <- ggrastr::rasterise(input = stack, ...)
    }
    input
}

rasterise.StackLayout <- function(input, ...) {
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
.raster_magick.QuadLayout <- function(x, ...) {
    x@plot <- .raster_magick(x = x@plot, ...)
    for (position in .TLBR) {
        stack <- slot(x, position)
        if (is.null(stack)) next
        slot(x, position) <- .raster_magick(x = stack, ...)
    }
    x
}

#' @export
.raster_magick.StackLayout <- function(x, ...) {
    x@plot_list <- lapply(x@plot_list, .raster_magick, ...)
    x
}


#' @export
.raster_magick.CraftBox <- function(x, ...) {
    if (!is.null(plot <- input@plot)) {
        input@plot <- .raster_magick(x = plot, ...)
    }
    input
}
