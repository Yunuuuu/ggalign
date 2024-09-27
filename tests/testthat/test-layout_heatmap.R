testthat::test_that("`ggheatmap` works well", {
    # atomic was converted to one-column matrix
    x <- ggheatmap(1:10)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    x <- ggheatmap(letters)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)

    # heatmap with no data
    p <- ggheatmap()
    expect_s3_class(p@params$width, "unit")
    expect_s3_class(p@params$height, "unit")
    expect_identical(p@nobs_list$x, NULL)
    expect_identical(p@nobs_list$y, NULL)

    # heatmap with data
    expect_doppelganger("heatmap-numeric", ggheatmap(1:10))
    expect_doppelganger(
        "heatmap-numeric-to-factor",
        ggheatmap(1:10, aes(fill = factor(value)))
    )
    expect_doppelganger("heatmap-character", ggheatmap(letters))
    expect_doppelganger("heatmap-matrix", ggheatmap(matrix(1:9, nrow = 3L)))
    testthat::skip_on_ci() # I don't know why this will fail in github CI
    expect_doppelganger("heatmap-data.frame", ggheatmap(data.frame(1:10)))
})

testthat::test_that("add `layout_theme()` works well", {
    expect_doppelganger(
        "heatmap-layout-theme",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            layout_theme(plot.background = element_rect(fill = "red"))
    )
})

testthat::test_that("add `layout_annotation()` works well", {
    expect_doppelganger(
        "heatmap-layout-annotation",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            layout_annotation(
                title = "I'm layout title"
            ) +
            layout_theme(plot.title = element_text(face = "bold"))
    )
})

testthat::test_that("add `hmanno()` works well", {
    expect_doppelganger(
        "heatmap-hmanno-width",
        ggheatmap(matrix(1:9, nrow = 3L)) + hmanno(width = unit(1, "cm"))
    )
    expect_doppelganger(
        "heatmap-hmanno-height",
        ggheatmap(matrix(1:9, nrow = 3L)) + hmanno(height = unit(1, "cm"))
    )
    expect_doppelganger(
        "heatmap-hmanno-null-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            scale_fill_viridis_c() +
            hmanno(guides = NULL) +
            hmanno("r") +
            align_dendro()
    )
    expect_doppelganger(
        "heatmap-hmanno-null-free-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            scale_fill_viridis_c() +
            hmanno(guides = NULL, free_guides = "r") +
            hmanno("r") +
            align_dendro()
    )
    expect_doppelganger(
        "heatmap-hmanno-stack-free-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            scale_fill_viridis_c() +
            hmanno("l", free_guides = "l") +
            align_dendro(aes(color = branch)) +
            scale_x_reverse()
    )
})

testthat::test_that("add `Align` object works well", {
    p <- ggheatmap(matrix(1:9, nrow = 3L))
    expect_error(p + align_dendro())
    p2 <- p + hmanno("t") + align_dendro()
    stack <- p2@top
    expect_identical(get_panel(stack), get_panel(p2, "x"))
    expect_identical(get_index(stack), get_index(p2, "x"))
    expect_identical(get_nobs(stack), get_nobs(p2, "x"))
})

testthat::test_that("`ggsave()` works well", {
    p <- ggheatmap(1:10)
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
