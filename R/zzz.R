.onLoad <- function(libname, pkgname) {
    theme_elements()
    # register method for
    s3_register("ggrastr::rasterise", "ggalign_align")
    s3_register("ggrastr::rasterise", "ggalign_free_gg")
    s3_register("ggrastr::rasterise", "QuadLayout")
    s3_register("ggrastr::rasterise", "StackLayout")
    invisible()
}
