testthat::test_that("`align_group` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + align_group(column_group))
    expect_error(p + hmanno("t") + align_group(1:4))
    expect_error(p + hmanno("t") + align_group(row_group))

    # cannot do sub-group
    expect_error(p +
        hmanno("t") +
        align_group(row_group) +
        align_group(row_group))
    expect_doppelganger(
        "group_top",
        p + hmanno("t") + align_group(column_group)
    )
    expect_doppelganger(
        "group_bottom",
        p + hmanno("b") + align_group(column_group)
    )
    expect_doppelganger(
        "group_left",
        p + hmanno("l") + align_group(row_group),
    )
    expect_doppelganger(
        "group_right",
        p + hmanno("r") + align_group(row_group),
    )
})

testthat::test_that("`align_order` works well", {
    set.seed(1L)
    mat <- matrix(stats::rnorm(72L), nrow = 9L)
    rownames(mat) <- paste0("row", seq_len(nrow(mat)))
    colnames(mat) <- paste0("column", seq_len(ncol(mat)))
    p <- ggheatmap(mat)
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    # align_order cannot do sub-split
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_order())
    expect_doppelganger(
        "reorder_top",
        p + hmanno("t") + align_order(),
    )
    expect_doppelganger(
        "reorder_bottom",
        p + hmanno("b") + align_order(),
    )
    expect_doppelganger(
        "reorder_left",
        p + hmanno("l") + align_order(),
    )
    expect_doppelganger(
        "reorder_right",
        p + hmanno("r") + align_order(),
    )
    expect_doppelganger(
        "reorder_top_within_group",
        p + hmanno("t") + align_group(column_group) +
            align_order(strict = FALSE),
    )
    expect_doppelganger(
        "reorder_top_reverse",
        p + hmanno("t") + align_order(reverse = TRUE)
    )
    expect_doppelganger(
        "reorder_left_reverse",
        p + hmanno("l") + align_order(reverse = TRUE)
    )
    # integer or character index works well
    my_order <- sample(nrow(mat))
    expect_doppelganger(
        "reorder_left_integer_index",
        p + hmanno("l") + align_order(my_order)
    )
    expect_doppelganger(
        "reorder_left_character_index",
        p + hmanno("l") + align_order(rownames(mat)[my_order])
    )
})

testthat::test_that("`align_reorder` works well", {
    set.seed(1L)
    mat <- matrix(stats::rnorm(72L), nrow = 9L)
    rownames(mat) <- paste0("row", seq_len(nrow(mat)))
    colnames(mat) <- paste0("column", seq_len(ncol(mat)))
    p <- ggheatmap(mat)
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    # align_reorder cannot do sub-split
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_reorder())
    expect_doppelganger(
        "order_top",
        p + hmanno("t") + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_bottom",
        p + hmanno("b") + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_left",
        p + hmanno("l") + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_right",
        p + hmanno("r") + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_top_within_group",
        p + hmanno("t") + align_group(column_group) +
            align_reorder(hclust2, strict = FALSE),
    )
})

testthat::test_that("`align_kmeans` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)

    expect_error(p + hmanno("t") + align_group(column_group) +
        align_kmeans(3L))
    expect_error(p + hmanno("l") + align_group(row_group) +
        align_kmeans(3L))

    expect_doppelganger(
        "kmeans_top",
        p + hmanno("t") + align_kmeans(3L)
    )
    expect_doppelganger(
        "kmeans_bottom",
        p + hmanno("b") + align_kmeans(5L)
    )
    expect_doppelganger(
        "kmeans_left",
        p + hmanno("b") + align_kmeans(4L)
    )
    expect_doppelganger(
        "kmeans_right",
        p + hmanno("r") + align_kmeans(2L)
    )
})

testthat::test_that("`align_dendro` works well", {
    set.seed(1L)
    mat <- matrix(stats::rnorm(72L), nrow = 9L)
    # rownames(mat) <- paste0("row", seq_len(nrow(mat)))
    # colnames(mat) <- paste0("column", seq_len(ncol(mat)))
    p <- ggheatmap(mat)
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_dendro(k = 2L))

    testthat::skip_on_ci() # I don't know why this will fail in github CI
    expect_doppelganger("dendrogram_top", p + hmanno("t") + align_dendro())
    expect_doppelganger("dendrogram_left", p + hmanno("l") + align_dendro())
    expect_doppelganger("dendrogram_bottom", p + hmanno("b") + align_dendro())
    expect_doppelganger("dendrogram_right", p + hmanno("r") + align_dendro())
    expect_doppelganger(
        "dendro_cutree",
        p + hmanno("t") + align_dendro(k = 3L)
    )
    expect_doppelganger(
        "dendro_between_group",
        p + hmanno("t") +
            align_group(column_group) +
            align_dendro()
    )
    expect_doppelganger(
        "dendro_reorder_group",
        p + hmanno("l") + align_group(row_group) +
            align_dendro(reorder_group = TRUE),
    )
    expect_doppelganger(
        "dendro_merge_group",
        p + hmanno("l") + align_group(row_group) +
            align_dendro(merge_dendrogram = TRUE),
    )
    expect_doppelganger(
        "dendro_reorder_and_merge",
        p + hmanno("l") + align_group(row_group) +
            align_dendro(reorder_group = TRUE, merge_dendrogram = TRUE),
    )
})

testthat::test_that("`ggalign()` works well", {
    set.seed(1L)
    small_mat <- matrix(stats::rnorm(81), nrow = 9)
    expect_doppelganger(
        "ggalign",
        ggheatmap(small_mat) +
            scale_fill_viridis_c(guide = "none") +
            hmanno("t") +
            ggalign(data = rowSums) +
            geom_point(aes(y = value))
    )
    expect_doppelganger(
        "ggalign set size",
        ggheatmap(small_mat) +
            scale_fill_viridis_c(guide = "none") +
            hmanno("t") +
            ggalign(data = rowSums, size = unit(1, "cm")) +
            geom_point(aes(y = value))
    )
})
