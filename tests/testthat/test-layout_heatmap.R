testthat::test_that("`ggheatmap` works well", {
    expect_identical(ncol(ggheatmap(1:10)@data), 1L)
    expect_identical(ncol(ggheatmap(letters)@data), 1L)
    expect_error(ggheatmap(NULL))
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(save_png(ggheatmap(1:10)), "numeric.png")
    expect_snapshot_file(save_png(ggheatmap(letters)), "character.png")
    expect_snapshot_file(save_png(ggheatmap(matrix(1:9, nrow = 3L))), "matrix.png")
    expect_snapshot_file(save_png(ggheatmap(data.frame(1:10))), "data_frame.png")
})

testthat::test_that("`build_patchwork` works well", {
    p <- ggheatmap(1:10)
    expect_s3_class(build_patchwork(p), "patchwork")
})

testthat::test_that("`ggplot` method works well", {
    p <- ggheatmap(1:10)
    expect_no_error(ggplot2::ggplot_build(p))
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
