test_that("align_hclust runs with default parameters", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    expect_doppelganger(
        "align_hclust default",
        ggheatmap(mat) +
            anno_top() +
            align_hclust(data = mat)
    )
})

test_that("align_hclust supports Pearson distance", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    expect_doppelganger(
        "align_hclust with Pearson distance",
        ggheatmap(mat) +
            anno_top() +
            align_hclust(data = mat, distance = "pearson")
    )
})

test_that("align_hclust errors on invalid reorder_dendrogram argument", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    expect_snapshot_error(
        align_hclust(data = mat, reorder_dendrogram = 123)
    )
})

test_that("align_hclust errors if k is not a whole number or NULL", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    expect_snapshot_error(
        align_hclust(data = mat, k = "invalid")
    )
})

test_that("align_hclust errors if h is not a numeric scalar or NULL", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    expect_snapshot_error(
        align_hclust(data = mat, h = "invalid")
    )
})

test_that("align_hclust accepts reorder_dendrogram as a function", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    reorder_fun <- function(tree, data) stats::as.dendrogram(tree)
    expect_doppelganger(
        "align_hclust with reorder function",
        ggheatmap(mat) +
            anno_top() +
            align_hclust(data = mat, reorder_dendrogram = reorder_fun)
    )
})

test_that("align_hclust accepts reorder_dendrogram = TRUE", {
    mat <- matrix(rnorm(100), nrow = 10)
    dimnames(mat) <- list(paste0("row", 1:10), paste0("col", 1:10))
    expect_doppelganger(
        "align_hclust with reorder_dendrogram TRUE",
        ggheatmap(mat) +
            anno_top() +
            align_hclust(data = mat, reorder_dendrogram = TRUE)
    )
})
