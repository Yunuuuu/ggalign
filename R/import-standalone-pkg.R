# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-pkg.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-pkg.R
# last-updated: 2025-02-26
# license: https://unlicense.org
# imports: [utils]
# ---

# This file contains several helper functions for package checking and
# installation.

# ## Changelog
# 2025-02-26:
# - Add `is_installed`
# - Add `install_pkgs`
# - Add `pkg_nm`
# - Add `pkg_namespace`
#
# nocov start

is_installed <- local({
    cache <- new.env(parent = emptyenv())
    function(pkg, version = NULL) {
        id <- if (is.null(version)) pkg else paste(pkg, version, sep = ":")
        out <- cache[[id]]
        if (is.null(out)) {
            if (is.null(version)) {
                out <- requireNamespace(pkg, quietly = TRUE)
            } else {
                out <- requireNamespace(pkg, quietly = TRUE) &&
                    utils::packageVersion(pkg) >= version
            }
            assign(id, out, envir = cache, inherits = FALSE)
        }
        out
    }
})

install_pkgs <- function(pkgs) {
    if (is_installed("pak")) {
        utils::getFromNamespace("pkg_install", "pak")(pkgs, ask = FALSE)
    } else {
        utils::install.packages(pkgs)
    }
}

pkg_nm <- function() utils::packageName(environment())

pkg_namespace <- function() topenv(environment())

# nocov end
