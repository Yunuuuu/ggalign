# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev
#     version of R)
#   - CI is not set (on CRAN)

if (is_installed("vdiffr")) {
    # write_svg <- function(plot, file, title = "") {
    #     svglite::svglite(file)
    #     on.exit(grDevices::dev.off())
    #     # warning: `print_plot()` is not exported by {vdiffr}
    #     vdiffr:::print_plot(plot, title)
    # }
    # function(...) {
    #     vdiffr::expect_doppelganger(..., writer = write_svg)
    # }
    expect_doppelganger <- vdiffr::expect_doppelganger
} else if (!identical(Sys.getenv("VDIFFR_RUN_TESTS"), "false")) {
    # If vdiffr is not available and visual tests are not explicitly
    # disabled, raise error.
    cli_abort("{.pkg vdiffr} is not installed")
} else {
    # Otherwise, assign a dummy function
    expect_doppelganger <- function(...) {
        testthat::skip("vdiffr is not installed.")
    }
}
