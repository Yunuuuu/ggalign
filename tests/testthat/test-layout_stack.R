testthat::test_that("`ggstack` works well", {
    # atomic was converted to one-column matrix
    x <- ggstack(1:10)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    expect_identical(get_nobs(x), 10L)

    x <- ggstack(letters)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    expect_identical(get_nobs(x), length(letters))

    # stack with no data
    p <- ggstack()
    expect_s3_class(p@params$sizes, "unit")
    expect_length(p@params$sizes, 3L)
    expect_identical(p@nobs, NULL)
})

testthat::test_that("add `stack_active` object works well", {
    p <- ggstack(matrix(1:9, nrow = 3L))
    # change parameters for heatmap self
    p2 <- p + active(
        sizes = unit(1, "cm"),
        guides = TRUE,
        free_labs = TRUE,
        plot_data = NULL
    )
    params <- slot(p2, "params")
    expect_identical(params$sizes, unit(rep_len(1L, 3L), "cm"))
    expect_identical(params$guides, BORDERS)
    expect_identical(params$free_labs, BORDERS)
    expect_identical(params$plot_data, NULL)
})

testthat::test_that("add `HeatmapLayout` object works well", {
    # stack without data add heatmap without data gave error
    expect_error(ggstack() + ggheatmap())

    # add heatmap with data gave right nobs for the stack layout
    p <- ggstack() + ggheatmap(matrix(1:9, nrow = 3L))
    expect_identical(get_nobs(p), 3L)

    # add heatmap without data gave right nobs for the heatmap layout
    p <- ggstack(matrix(1:10, nrow = 2L))
    p2 <- p + ggheatmap()
    expect_identical(get_nobs(p), get_nobs(p2@plots[[1L]], "y"))
    expect_identical(get_nobs(p2@plots[[1L]], "x"), 5L)

    p <- ggstack(matrix(1:10, nrow = 2L), "v")
    p2 <- p + ggheatmap()
    expect_identical(get_nobs(p), get_nobs(p2@plots[[1L]], "x"))
    expect_identical(get_nobs(p2@plots[[1L]], "y"), 5L)

    # add heatmap with incompatible data
    expect_error(ggstack(1:10) + ggheatmap(letters))
    expect_error(
        ggstack(matrix(seq_len(81), nrow = 9L)) +
            align_kmeans(3L) +
            (ggheatmap(matrix(seq_len(81), nrow = 9L)) +
                hmanno("l") +
                align_kmeans(4))
    )
    # we prevent from reordering the layout index
    expect_error(
        ggstack(matrix(seq_len(81), nrow = 9L)) +
            align_dendro() +
            (ggheatmap(matrix(seq_len(81), nrow = 9L)) +
                hmanno("l") +
                align_kmeans(3))
    )
})
