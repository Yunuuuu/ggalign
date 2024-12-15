testthat::test_that("`align_group` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_snapshot_error(p + align_group(column_group))
    expect_snapshot_error(p + anno_top() + align_group(1:4))
    expect_snapshot_error(p + anno_top() + align_group(row_group))

    # adding plot gave error
    expect_snapshot_error(p + anno_top() +
        align_group(1:4, active = active(use = TRUE)) +
        geom_point())

    # cannot do sub-group
    expect_snapshot_error(p +
        anno_top() +
        align_group(row_group) +
        align_group(row_group))
    expect_doppelganger(
        "group_top",
        p + anno_top() + align_group(column_group)
    )
    expect_doppelganger(
        "group_bottom",
        p + anno_bottom() + align_group(column_group)
    )
    expect_doppelganger(
        "group_left",
        p + anno_left() + align_group(row_group),
    )
    expect_doppelganger(
        "group_right",
        p + anno_right() + align_group(row_group),
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
    expect_snapshot_error(p + anno_top() + align_group(column_group) +
        align_order())

    # adding plot gave error
    expect_snapshot_error(p + anno_top() +
        align_order(active = active(use = TRUE)) +
        geom_point())

    expect_doppelganger(
        "reorder_top",
        p + anno_top() + align_order()
    )
    expect_doppelganger(
        "reorder_bottom",
        p + anno_bottom() + align_order()
    )
    expect_doppelganger(
        "reorder_left",
        p + anno_left() + align_order()
    )
    expect_doppelganger(
        "reorder_right",
        p + anno_right() + align_order()
    )
    expect_doppelganger(
        "reorder_top_within_group",
        p + anno_top() + align_group(column_group) +
            align_order(strict = FALSE),
    )
    expect_doppelganger(
        "reorder_top_reverse",
        p + anno_top() + align_order(reverse = TRUE)
    )
    expect_doppelganger(
        "reorder_left_reverse",
        p + anno_left() + align_order(reverse = TRUE)
    )
    # integer or character index works well
    set.seed(1L)
    my_order <- sample(nrow(mat))
    expect_doppelganger(
        "reorder_left_integer_index",
        p + anno_left() + align_order(my_order)
    )
    expect_doppelganger(
        "reorder_left_character_index",
        p + anno_left() + align_order(rownames(mat)[my_order])
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
    expect_snapshot_error(p + anno_top() + align_group(column_group) +
        align_reorder(hclust2))

    # adding plot gave error
    expect_snapshot_error(p + anno_top() +
        align_reorder(hclust2, active = active(use = TRUE)) +
        geom_point())

    # reorder plot
    expect_doppelganger(
        "order_top",
        p + anno_top() + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_bottom",
        p + anno_bottom() + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_left",
        p + anno_left() + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_right",
        p + anno_right() + align_reorder(hclust2),
    )
    expect_doppelganger(
        "order_top_within_group",
        p + anno_top() + align_group(column_group) +
            align_reorder(hclust2, strict = FALSE),
    )
})

testthat::test_that("`align_kmeans` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)

    # reorder twice
    expect_snapshot_error(p + anno_top() + align_group(column_group) +
        align_kmeans(3L))
    expect_snapshot_error(p + anno_left() + align_group(row_group) +
        align_kmeans(3L))

    # adding plot gave error
    expect_snapshot_error(p + anno_top() +
        align_kmeans(3L, active = active(use = TRUE)) +
        geom_point())

    set.seed(1L)
    expect_doppelganger(
        "kmeans_top",
        p + anno_top() + align_kmeans(3L)
    )
    expect_doppelganger(
        "kmeans_bottom",
        p + anno_bottom() + align_kmeans(5L)
    )
    expect_doppelganger(
        "kmeans_left",
        p + anno_bottom() + align_kmeans(4L)
    )
    expect_doppelganger(
        "kmeans_right",
        p + anno_right() + align_kmeans(2L)
    )
})

testthat::test_that("`align_dendro` works well", {
    set.seed(1L)
    mat <- matrix(stats::rnorm(120L), nrow = 10L)
    # rownames(mat) <- paste0("row", seq_len(nrow(mat)))
    # colnames(mat) <- paste0("column", seq_len(ncol(mat)))
    p <- ggheatmap(mat)
    row_group <- sample(letters[1:3], nrow(mat), replace = TRUE)
    column_group <- sample(letters[1:3], ncol(mat), replace = TRUE)
    expect_snapshot_error(p + anno_top() + align_group(column_group) +
        align_dendro(k = 2L))

    testthat::skip_on_ci() # I don't know why this will fail in github CI
    expect_doppelganger("dendrogram_top", p + anno_top() + align_dendro())
    expect_doppelganger("dendrogram_left", p + anno_left() + align_dendro())
    expect_doppelganger("dendrogram_bottom", p + anno_bottom() + align_dendro())
    expect_doppelganger("dendrogram_right", p + anno_right() + align_dendro())
    expect_doppelganger("stack_no_data_hclust_input", {
        stack_alignh() + align_dendro(method = hclust2(mat))
    })
    expect_doppelganger("stack_no_data_dendrogram_input", {
        stack_alignh() +
            align_dendro(method = stats::as.dendrogram(hclust2(mat)))
    })
    expect_doppelganger("heatmap_no_data_hclust_input", {
        stack_alignh() + align_dendro(method = hclust2(mat))
    })
    expect_doppelganger("heatmap_no_data_dendrogram_input", {
        stack_alignh() +
            align_dendro(method = stats::as.dendrogram(hclust2(mat)))
    })
    expect_doppelganger(
        "dendro_cutree",
        p + anno_top() + align_dendro(k = 3L)
    )
    expect_doppelganger(
        "dendro_reorder_dendro",
        p + anno_top() + align_dendro(reorder_dendrogram = TRUE)
    )
    expect_doppelganger(
        "dendro_reorder_dendro_and_cutree",
        p + anno_top() +
            align_dendro(aes(color = branch), k = 3L, reorder_dendrogram = TRUE)
    )
    expect_doppelganger(
        "dendro_reorder_dendro_in_and_between_group",
        p +
            anno_top() +
            align_group(column_group) +
            align_dendro(aes(color = branch), reorder_dendrogram = TRUE)
    )
    expect_doppelganger(
        "dendro_between_group",
        p + anno_top() +
            align_group(column_group) +
            align_dendro()
    )
    expect_doppelganger(
        "dendro_reorder_group",
        p + anno_left() + align_group(row_group) +
            align_dendro(reorder_group = TRUE),
    )
    expect_doppelganger(
        "dendro_merge_group",
        p + anno_left() + align_group(row_group) +
            align_dendro(merge_dendrogram = TRUE),
    )
    expect_doppelganger(
        "dendro_reorder_and_merge_group",
        p + anno_left() + align_group(row_group) +
            align_dendro(reorder_group = TRUE, merge_dendrogram = TRUE),
    )
    expect_doppelganger(
        "dendro_reorder_dendro_in_group_and_merge_group",
        p +
            anno_top() +
            align_group(column_group) +
            align_dendro(aes(color = branch),
                reorder_dendrogram = TRUE,
                merge_dendrogram = TRUE
            )
    )
    expect_doppelganger(
        "dendro_reorder_dendro_in_and_between_group_and_merge",
        p +
            anno_top() +
            align_group(column_group) +
            align_dendro(aes(color = branch),
                reorder_dendrogram = TRUE,
                reorder_group = TRUE,
                merge_dendrogram = TRUE
            )
    )
})

testthat::test_that("`ggalign()` works well", {
    set.seed(1L)
    small_mat <- matrix(stats::rnorm(81), nrow = 9)
    # adding non-cartesian coord
    expect_snapshot_warning({
        pdf(NULL)
        print(ggheatmap(small_mat) +
            anno_top() +
            ggalign(data = rowSums, size = unit(1, "cm")) +
            geom_point(aes(y = value)) +
            coord_polar())
        dev.off()
    })

    expect_doppelganger(
        "ggalign",
        ggheatmap(small_mat) +
            scale_fill_viridis_c(guide = "none") +
            anno_top() +
            ggalign(data = rowSums) +
            geom_point(aes(y = value))
    )
    expect_doppelganger(
        "ggalign set size",
        ggheatmap(small_mat) +
            scale_fill_viridis_c(guide = "none") +
            anno_top() +
            ggalign(data = rowSums, size = unit(1, "cm")) +
            geom_point(aes(y = value))
    )
})
