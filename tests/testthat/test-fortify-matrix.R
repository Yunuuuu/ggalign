test_that("`MAF` method calculate variant_weights", {
    skip_if_not_installed("maftools")
    laml.maf <- system.file("extdata", "tcga_laml.maf.gz", package = "maftools")
    laml.clin <- system.file("extdata", "tcga_laml_annot.tsv",
        package = "maftools"
    )
    laml <- maftools::read.maf(
        maf = laml.maf,
        clinicalData = laml.clin,
        verbose = FALSE
    )
    expect_equal(
        rowSums(ggalign_attr(fortify_matrix(laml), "variant_weights")[-1]),
        ggalign_attr(fortify_matrix(laml), "gene_summary")$AlteredSamples
    )
})

test_that("tune.list returns list_upset class and errors on shape argument", {
    data <- list(A = 1:3, B = 2:4)
    expect_s3_class(tune(data), "list_upset")

    expect_snapshot_error(tune(data, shape = "not_used"))
})

test_that("`list_upset` method works with distinct mode", {
    data <- list(A = 1:3, B = 2:4, C = 4:5)
    tuned <- tune(data)
    mat <- fortify_matrix(tuned, mode = "distinct")

    expect_true(is.matrix(mat))
    expect_named(
        ggalign_attr(mat),
        c("intersection_sizes", "set_sizes", "upset_mode")
    )
    expect_equal(ggalign_attr(mat, "upset_mode"), "distinct")
    expect_equal(
        unname(ggalign_attr(mat, "intersection_sizes")),
        c(1, 1, 2, 1)
    )
    expect_equal(ggalign_attr(mat, "set_sizes"), lengths(data))
})

test_that("`list_upset` method works with intersect mode", {
    data <- list(A = 1:5, B = 2:4, C = 3:6)
    tuned <- tune(data)
    mat <- fortify_matrix(tuned, mode = "intersect")

    expect_true(is.matrix(mat))
    expect_named(
        ggalign_attr(mat),
        c("intersection_sizes", "set_sizes", "upset_mode")
    )
    expect_equal(ggalign_attr(mat, "upset_mode"), "intersect")
    expect_equal(
        ggalign_attr(mat, "intersection_sizes"),
        unname(c(lengths(data), 3, 3, 2, 2))
    )
    expect_equal(ggalign_attr(mat, "set_sizes"), lengths(data))
})

test_that("`list_upset` method works with union mode", {
    data <- list(A = 1:2, B = 2:3)
    tuned <- tune(data)
    mat <- fortify_matrix(tuned, mode = "union")

    expect_true(is.matrix(mat))
    expect_named(
        ggalign_attr(mat),
        c("intersection_sizes", "set_sizes", "upset_mode")
    )
    expect_equal(ggalign_attr(mat, "upset_mode"), "union")
    expect_equal(ggalign_attr(mat, "intersection_sizes"), c(2, 2, 3))
    expect_equal(ggalign_attr(mat, "set_sizes"), lengths(data))
})

test_that("`list_upset` method drops empty intersections", {
    data <- list(A = integer(), B = integer())
    tuned <- tune(data)
    expect_snapshot_error(fortify_matrix(tuned, mode = "distinct"))
})

test_that("`list_upset` method handles NA and duplicate values", {
    data <- list(A = c(1, 2, NA, 2), B = c(2, 3, NA))
    tuned <- tune(data)
    mat <- fortify_matrix(tuned, mode = "intersect")

    expect_true(is.matrix(mat))
    expect_named(
        ggalign_attr(mat),
        c("intersection_sizes", "set_sizes", "upset_mode")
    )
    expect_equal(ggalign_attr(mat, "upset_mode"), "intersect")
    expect_equal(ggalign_attr(mat, "intersection_sizes"), c(2, 2, 1))
    # Missing value and duplicated value are automatically removed
    expect_equal(ggalign_attr(mat, "set_sizes"), c(A = 2, B = 2))
})
