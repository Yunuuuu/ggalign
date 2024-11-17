#' @export
quad_build.HeatmapLayout <- function(quad, controls = quad@controls) {
    ans <- NextMethod()

    # add heatmap filling in the first layer --------------
    if (!is.null(filling <- quad@filling)) {
        # we always ensure the filling layer has a fill mapping
        if (is.null(.subset2(ans$plots$main$mapping, "fill"))) {
            mapping <- aes(.data$.x, .data$.y, fill = .data$value)
        } else {
            mapping <- aes(.data$.x, .data$.y)
        }
        if (is.waive(filling)) {
            if (nrow(quad@data) * ncol(quad@data) > 20000L) {
                cli_inform(c(">" = "heatmap built with {.fn geom_raster}"))
                filling <- "raster"
            } else {
                cli_inform(c(">" = "heatmap built with {.fn geom_tile}"))
                filling <- "tile"
            }
        }
        ans$plots$main <- ans$plots$main + layer_order(switch(filling,
            raster = ggplot2::geom_raster(mapping = mapping),
            tile = ggplot2::geom_tile(mapping = mapping)
        ))
    }
    # add class to set the default color mapping --------
    ans$plots$main <- add_class(ans$plots$main, "ggalign_heatmap")
    ans
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.ggalign_heatmap <- function(plot) {
    with_options(
        NextMethod(),
        ggplot2.discrete.fill = heatmap_fill("discrete"),
        ggplot2.continuous.fill = heatmap_fill("continuous")
    )
}

heatmap_fill <- function(type) {
    opt <- sprintf("%s.heatmap_%s_fill", pkg_nm(), type)
    if (is.null(ans <- getOption(opt, default = NULL))) {
        if (type == "continuous") {
            ans <- function(...) {
                ggplot2::scale_fill_gradient2(low = "blue", high = "red")
            }
        } else {
            ans <- getOption("ggplot2.discrete.fill")
        }
    }
    ans
}
