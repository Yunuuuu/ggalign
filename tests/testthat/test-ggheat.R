testthat::test_that("`ggheat` works well", {
    expect_identical(ncol(ggheat(1:10)@matrix), 1L)
    expect_identical(ncol(ggheat(letters)@matrix), 1L)
    expect_error(ggheat(NULL))
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(save_png(ggheat(1:10)), "numeric.png")
    expect_snapshot_file(save_png(ggheat(letters)), "character.png")
    expect_snapshot_file(save_png(ggheat(matrix(1:9, nrow = 3L))), "matrix.png")
    expect_snapshot_file(save_png(ggheat(data.frame(1:10))), "data_rame.png")
})

testthat::test_that("`ggheat_build` works well", {
    p <- ggheat(1:10)
    expect_s3_class(ggheat_build(p), "patchwork")
})

testthat::test_that("`ggplot` method works well", {
    p <- ggheat(1:10)
    expect_no_error(ggplot2::ggplot_build(p))
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
