.onAttach <- function(libname, pkgname) {
    version <- utils::packageDescription(pkgname, fields = "Version")
    packageStartupMessage(paste(
        "========================================",
        paste(pkgname, "version", version),
        "",
        "If you use it in published research, please cite: ",
        paste0(
            "Peng, Y.; Jiang, S.; Song, Y.; et al. ",
            "ggalign: Bridging the Grammar of Graphics and Biological Multilayered Complexity. ",
            "Advanced Science. 2025. doi:10.1002/advs.202507799"
        ),
        "========================================",
        sep = "\n"
    ))
}

#' @importFrom S7 methods_register
.onLoad <- function(libname, pkgname) {
    # register ggplot2 theme elements
    theme_elements()

    # register method for
    s3_register("ggrastr::rasterise", "ggalign::CraftBox")
    s3_register("ggrastr::rasterise", "ggalign::QuadLayout")
    s3_register("ggrastr::rasterise", "ggalign::ChainLayout")

    methods_register()
    invisible()
}
