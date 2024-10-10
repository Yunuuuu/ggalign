rasterise.HeatmapLayout <- function(input, ...) {
    input@plot <- ggrastr::rasterise(input = input@plot, ...)
    for (position in .TLBR) {
        stack <- slot(input, position)
        if (is.null(stack)) next
        slot(input, position) <- ggrastr::rasterise(input = stack, ...)
    }
    input
}

rasterise.StackLayout <- function(input, ...) {
    input@plots <- lapply(input@plots, function(plot) {
        if (is_ggheatmap(plot)) {
            plot <- ggrastr::rasterise(input = plot, ...)
        } else if (!is.null(.subset2(plot, "plot"))) {
            # if `align` has plot, we added the object
            plot$plot <- ggrastr::rasterise(input = plot$plot, ...)
        }
        plot
    })
}
