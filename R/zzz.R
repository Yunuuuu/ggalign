#' @importFrom S7 methods_register
.onLoad <- function(libname, pkgname) {
    # register ggplot2 theme elements
    theme_elements()

    # register method for
    s3_register("ggrastr::rasterise", "CraftBox")
    s3_register("ggrastr::rasterise", "QuadLayout")
    s3_register("ggrastr::rasterise", "ChainLayout")

    # In R >= 4.3.0, S7 methods fall back to base Ops behavior when one of the
    # arguments is not an S7 object. This ensures compatibility in such cases.
    if (getRversion() >= "4.3.0") {
        registerS3method("+", "ggalign::AlignPatches", alignpatches_add_call)
    }
    methods_register()
    invisible()
}
