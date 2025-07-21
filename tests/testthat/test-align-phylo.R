skip_if_not_installed("ape")

set.seed(123)
mat <- matrix(rnorm(25), nrow = 5)
dimnames(mat) <- list(paste0("tip", 1:5), paste0("tip", 1:5))
tree <- getExportedValue("ape", "rtree")(5)
tree$tip.label <- paste0("tip", 1:5)

test_that("align_phylo runs silently with default parameters", {
    expect_doppelganger(
        "align_phylo top",
        ggheatmap(mat) +
            anno_top() +
            align_phylo(tree)
    )
    expect_doppelganger(
        "align_phylo left",
        ggheatmap(mat) +
            anno_left() +
            align_phylo(tree)
    )
    expect_doppelganger(
        "align_phylo bottom",
        ggheatmap(mat) +
            anno_bottom() +
            align_phylo(tree)
    )
    expect_doppelganger(
        "align_phylo right",
        ggheatmap(mat) +
            anno_right() +
            align_phylo(tree)
    )
})

test_that("align_phylo runs with ladderize = left", {
    expect_doppelganger(
        "align_phylo ladderize left",
        ggheatmap(mat) +
            anno_top() +
            align_phylo(tree, ladderize = "left")
    )
})

test_that("align_phylo runs with ladderize = right", {
    expect_doppelganger(
        "align_phylo ladderize right",
        ggheatmap(mat) +
            anno_top() +
            align_phylo(tree, ladderize = "right")
    )
})

test_that("align_phylo runs with tree_type = cladogram", {
    expect_doppelganger(
        "align_phylo tree_type cladogram",
        ggheatmap(mat) +
            anno_top() +
            align_phylo(tree, tree_type = "cladogram")
    )
})

test_that("align_phylo runs with center = TRUE", {
    expect_doppelganger(
        "align_phylo center TRUE",
        ggheatmap(mat) +
            anno_top() +
            align_phylo(tree, center = TRUE)
    )
})

test_that("align_phylo errors if input is not phylo", {
    expect_snapshot_error(align_phylo(mtcars))
})
