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
    input@plots <- lapply(input@plots, ggrastr::rasterise, ...)
    input
}

rasterise.align <- function(input, ...) {
    if (!is.null(plot <- .subset2(input, "plot"))) {
        input$plot <- ggrastr::rasterise(input = plot, ...)
    }
    input
}

rasterise.ggalign_free_gg <- function(input, ...) {
    input$plot <- .raster_magick(input = .subset2(input, "plot"), ...)
    input
}

##########################################################
#' @export
.raster_magick.QuadLayout <- function(x, magick = NULL, ...) {
    x@plot <- .raster_magick(x = x@plot, ...)
    for (position in .TLBR) {
        stack <- slot(x, position)
        if (is.null(stack)) next
        slot(x, position) <- .raster_magick(x = stack, magick = magick, ...)
    }
    x
}

#' @export
.raster_magick.StackLayout <- function(x, magick = NULL, ...) {
    x@plots <- lapply(x@plots, .raster_magick, magick = magick, ...)
    x
}


#' @export
.raster_magick.ggalign_free_gg <- function(x, magick = NULL, ...) {
    x$plot <- .raster_magick(x = .subset2(x, "plot"), magick = magick, ...)
    x
}

#' @export
.raster_magick.align <- function(x, magick = NULL, ...) {
    if (!is.null(plot <- .subset2(x, "plot"))) {
        x$plot <- .raster_magick(x = plot, magick = magick, ...)
    }
    x
}
