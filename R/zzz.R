#' @importFrom S7 methods_register
.onLoad <- function(libname, pkgname) {
    # register ggplot2 theme elements
    theme_elements()

    # register method for
    s3_register("ggrastr::rasterise", "CraftBox")
    s3_register("ggrastr::rasterise", "QuadLayout")
    s3_register("ggrastr::rasterise", "ChainLayout")
    if (getRversion() >= "4.3.0") {
        registerS3method("+", "ggalign::AlignPatches", alignpatches_add_call)
    }
    methods_register()
    invisible()
}
