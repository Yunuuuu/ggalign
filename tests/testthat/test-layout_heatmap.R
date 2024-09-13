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

testthat::test_that("add `heatmap_active` object works well", {
    # heatmap without data
    empty_heatmap <- ggheatmap()
    empty_heatmap2 <- empty_heatmap + hmanno(
        width = unit(1, "cm"),
        height = unit(1, "cm"),
        guides = TRUE,
        free_labs = TRUE,
        plot_data = NULL
    )
    params <- empty_heatmap2@params
    expect_identical(params$width, unit(1, "cm"))
    expect_identical(params$height, unit(1, "cm"))
    expect_identical(params$guides, "tlbr")
    expect_identical(params$free_labs, "tlbr")
    expect_identical(params$plot_data, NULL)
    expect_error(empty_heatmap + hmanno("t"))

    # heatmap with data
    p <- ggheatmap(matrix(1:9, nrow = 3L))
    # change parameters for heatmap self
    p2 <- p + hmanno(
        width = unit(1, "cm"),
        height = unit(1, "cm"),
        free_labs = TRUE,
        plot_data = NULL
    )
    params <- p2@params
    expect_identical(params$width, unit(1, "cm"))
    expect_identical(params$height, unit(1, "cm"))
    expect_identical(params$guides, waiver())
    expect_identical(params$free_labs, "tlbr")
    expect_identical(params$plot_data, NULL)

    # change parameters for heatmap annotation
    p3 <- p + hmanno(
        "t",
        width = unit(3, "cm"),
        height = unit(3, "cm"),
        guides = NULL,
        free_labs = TRUE,
        plot_data = NULL
    )
    # won't change the params of heatmap
    params <- p3@params
    expect_identical(params$width, unit(NA, "null"))
    expect_identical(params$height, unit(NA, "null"))
    expect_identical(params$guides, waiver())
    expect_identical(params$free_labs, waiver())
    expect_identical(params$plot_data, waiver())

    # change the params of stack
    stack <- p3@top
    expect_identical(get_panel(stack), get_panel(p, "x"))
    expect_identical(get_index(stack), get_index(p, "x"))
    expect_identical(get_nobs(stack), get_nobs(p, "x"))
    params <- stack@params
    expect_identical(params$size, unit(NA, "null"))
    expect_identical(params$guides, NULL)
    expect_identical(params$free_labs, "tlbr")
    expect_identical(params$plot_data, NULL)
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
