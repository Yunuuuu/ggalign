`rasterise.ggalign::QuadLayout` <- function(input, ...) {
    input@plot <- ggrastr::rasterise(input = input@plot, ...)
    for (position in .TLBR) {
        stack <- prop(input, position)
        if (is.null(stack)) next
        prop(input, position) <- ggrastr::rasterise(input = stack, ...)
    }
    input
}

`rasterise.ggalign::ChainLayout` <- function(input, ...) {
    input@box_list <- lapply(input@box_list, ggrastr::rasterise, ...)
    input
}

`rasterise.ggalign::CraftBox` <- function(input, ...) {
    if (!is.null(plot <- input@plot)) {
        input@plot <- ggrastr::rasterise(input = plot, ...)
    }
    input
}

##########################################################
#' @export
`raster_magick.ggalign::QuadLayout` <- function(x, ...) {
    x@plot <- raster_magick(x = x@plot, ...)
    for (position in .TLBR) {
        stack <- prop(x, position)
        if (is.null(stack)) next
        prop(x, position) <- raster_magick(x = stack, ...)
    }
    x
}

#' @export
`raster_magick.ggalign::ChainLayout` <- function(x, ...) {
    x@box_list <- lapply(x@box_list, raster_magick, ...)
    x
}

#' @export
`raster_magick.ggalign::CraftBox` <- function(x, ...) {
    if (!is.null(plot <- input@plot)) {
        input@plot <- raster_magick(x = plot, ...)
    }
    input
}
