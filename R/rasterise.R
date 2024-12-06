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

rasterise.ggalign_align <- function(input, ...) {
    if (!is.null(plot <- .subset2(input, "plot"))) {
        input$plot <- ggrastr::rasterise(input = plot, ...)
    }
    input
}

rasterise.ggalign_plot <- function(input, ...) {
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
    x@plot_list <- lapply(x@plot_list, .raster_magick, magick = magick, ...)
    x
}


#' @export
.raster_magick.ggalign_plot <- function(x, magick = NULL, ...) {
    x$plot <- .raster_magick(x = .subset2(x, "plot"), magick = magick, ...)
    x
}

#' @export
.raster_magick.ggalign_align <- function(x, magick = NULL, ...) {
    if (!is.null(plot <- .subset2(x, "plot"))) {
        x$plot <- .raster_magick(x = plot, magick = magick, ...)
    }
    x
}
