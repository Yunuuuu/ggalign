# vdiffr ignores failures when
#   - VDIFFR_RUN_TESTS is "false" (on Travis CI with older versions and dev
#     version of R)
#   - CI is not set (on CRAN)

expect_doppelganger <- tryCatch(
    vdiffr::expect_doppelganger,
    error = function(cnd) {
        # If vdiffr is not available and visual tests are not explicitly
        # disabled, raise error.
        if (!identical(Sys.getenv("VDIFFR_RUN_TESTS"), "false")) {
            cli_abort("{.pkg vdiffr} is not installed")
        }

        # Otherwise, assign a dummy function
        function(...) testthat::skip("vdiffr is not installed.")
    }
)
