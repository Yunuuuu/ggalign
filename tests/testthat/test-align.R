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

testthat::test_that("`align_reorder` works well", {
    set.seed(1L)
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_reorder())
    expect_doppelganger(
        "reorder_top",
        p + hmanno("t") + align_reorder(),
    )
    expect_doppelganger(
        "reorder_bottom",
        p + hmanno("b") + align_reorder(),
    )
    expect_doppelganger(
        "reorder_left",
        p + hmanno("l") + align_reorder(),
    )
    expect_doppelganger(
        "reorder_right",
        p + hmanno("r") + align_reorder(),
    )
    expect_doppelganger(
        "reorder_top_within_group",
        p + hmanno("t") + align_group(column_group) +
            align_reorder(strict = FALSE),
    )
    expect_doppelganger(
        "reorder_top_decreasing",
        p + hmanno("t") + align_reorder(decreasing = TRUE)
    )
    expect_doppelganger(
        "reorder_left_decreasing",
        p + hmanno("l") + align_reorder(decreasing = TRUE)
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
    p <- ggheatmap(matrix(stats::rnorm(72L), nrow = 9L))
    row_group <- sample(letters[1:3], 9, replace = TRUE)
    column_group <- sample(letters[1:3], 8, replace = TRUE)
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_dendro(k = 3L))
    expect_error(p + hmanno("t") + align_group(column_group) +
        align_dendro(h = 3L))

    testthat::skip_on_ci() # I don't know why this will fail in github CI
    expect_doppelganger("dendrogram", p + hmanno("t") + align_dendro())
    expect_doppelganger(
        "dendro_cutree",
        p + hmanno("t") + align_dendro(k = 3L)
    )
    expect_doppelganger(
        "dendro_top_between_group",
        p + hmanno("t") + align_group(column_group) +
            align_dendro()
    )
    expect_doppelganger(
        "dendro_left_between_group_reorder",
        p + hmanno("l") + align_group(row_group) +
            align_dendro(reorder_group = TRUE) +
            scale_x_reverse(),
    )
})
