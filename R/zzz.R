#' @importFrom vctrs s3_register
.onLoad <- function(libname, pkgname) {
    theme_elements()
    # register method for 
    s3_register("ggrastr::rasterise", "HeatmapLayout")
    s3_register("ggrastr::rasterise", "StackLayout")
    invisible()
}
