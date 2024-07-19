testthat::test_that("`htanno_group` works well", {
    set.seed(1L)
    p <- ggheat(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + htanno_group(column_group))
    expect_error(p + htanno_group(1:4, "t"))
    expect_error(p + htanno_group(row_group, "t"))

    # cannot do sub-group
    expect_error(p + htanno_group(row_group, "t") + htanno_group(row_group))

    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + htanno_group(column_group, "t")), "group_top.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_group(column_group, "b")), "group_bottom.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_group(row_group, "l")), "group_left.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_group(row_group, "r")), "group_right.png"
    )
})

testthat::test_that("`htanno_reorder` works well", {
    set.seed(1L)
    p <- ggheat(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + htanno_group(column_group, "t") + htanno_reorder())
    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + htanno_reorder(position = "t")), "reorder_top.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_reorder(position = "b")), "reorder_bottom.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_reorder(position = "l")), "reorder_left.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_reorder(position = "r")), "reorder_right.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_group(column_group, "t") +
            htanno_reorder(strict = FALSE)),
        "reorder_top_within_group.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_reorder(position = "t", decreasing = TRUE)),
        "reorder_top_decreasing.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_reorder(position = "l", decreasing = TRUE)),
        "reorder_left_decreasing.png"
    )
})

testthat::test_that("`htanno_kmeans` works well", {
    set.seed(1L)
    p <- ggheat(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)

    expect_error(p + htanno_group(column_group, "t") + htanno_kmeans(3L))
    expect_error(p + htanno_group(row_group, "l") + htanno_kmeans(3L))

    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + htanno_kmeans(3L, position = "t")), "kmeans_top.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_kmeans(5L, position = "b")), "kmeans_bottom.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_kmeans(4L, position = "l")), "kmeans_left.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_kmeans(2L, position = "r")), "kmeans_right.png"
    )
})

testthat::test_that("`htanno_dendro` works well", {
    set.seed(1L)
    p <- ggheat(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + htanno_group(column_group, "t") + htanno_dendro(k = 3L))
    expect_error(p + htanno_group(column_group, "t") + htanno_dendro(h = 3L))

    testthat::skip_on_cran()
    testthat::skip_on_ci()
    expect_snapshot_file(
        save_png(p + htanno_dendro(position = "t")), "dendro.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_dendro(k = 3L, position = "t")), "dendro_cutree.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_group(column_group, "t") + htanno_dendro()),
        "dendro_top_between_group.png"
    )
    expect_snapshot_file(
        save_png(p + htanno_group(row_group, "l") +
            htanno_dendro(reorder_group = TRUE)),
        "dendro_left_between_group_reorder.png"
    )
})
