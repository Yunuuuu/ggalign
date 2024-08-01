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

    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_group(column_group)),
        "group_top.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("b") + align_group(column_group)),
        "group_bottom.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("l") + align_group(row_group)),
        "group_left.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("r") + align_group(row_group)),
        "group_right.png"
    )
})

testthat::test_that("`align_reorder` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_reorder())
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_reorder()), "reorder_top.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("b") + align_reorder()), "reorder_bottom.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("l") + align_reorder()), "reorder_left.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("r") + align_reorder()), "reorder_right.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_group(column_group) +
            align_reorder(strict = FALSE)),
        "reorder_top_within_group.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_reorder(decreasing = TRUE)),
        "reorder_top_decreasing.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("l") + align_reorder(decreasing = TRUE)),
        "reorder_left_decreasing.png"
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

    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_kmeans(3L)), "kmeans_top.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("b") + align_kmeans(5L)), "kmeans_bottom.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("b") + align_kmeans(4L)), "kmeans_left.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("r") + align_kmeans(2L)), "kmeans_right.png"
    )
})

testthat::test_that("`align_dendro` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_dendro(k = 3L))
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_dendro(h = 3L))

    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_dendro()), "dendro.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("t") + align_dendro(k = 3L)), "dendro_cutree.png"
    )
    expect_snapshot_file(
        save_png(
            p + hmanno("t") + align_group(column_group) +
                align_dendro()
        ),
        "dendro_top_between_group.png"
    )
    expect_snapshot_file(
        save_png(p + hmanno("l") + align_group(row_group) +
            align_dendro(reorder_group = TRUE) +
            scale_x_reverse()),
        "dendro_left_between_group_reorder.png"
    )
})
