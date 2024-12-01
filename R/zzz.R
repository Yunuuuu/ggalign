.onLoad <- function(libname, pkgname) {
    # register ggplot2 theme elements
    theme_elements()

    # register method for
    s3_register("ggrastr::rasterise", "ggalign_plot")
    s3_register("ggrastr::rasterise", "QuadLayout")
    s3_register("ggrastr::rasterise", "StackLayout")
    invisible()
}
