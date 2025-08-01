#' @importFrom S7 methods_register
.onLoad <- function(libname, pkgname) {
    # register ggplot2 theme elements
    theme_elements()

    # register method for
    s3_register("ggrastr::rasterise", "ggalign::CraftBox")
    s3_register("ggrastr::rasterise", "QuadLayout")
    s3_register("ggrastr::rasterise", "ChainLayout")

    methods_register()
    invisible()
}
