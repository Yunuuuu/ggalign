.onLoad <- function(libname, pkgname) {
    if (packageVersion("ggplot2") >= "3.3.0") {
        theme_elements()
    }
    invisible()
}
