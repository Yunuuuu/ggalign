testthat::test_that("add `layout_annotation()` works well", {
    expect_doppelganger(
        "heatmap-layout-theme",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            layout_annotation(
                theme = theme(plot.background = element_rect(fill = "red"))
            )
    )
})

testthat::test_that("add `layout_title()` works well", {
    expect_doppelganger(
        "heatmap-layout-annotation",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            layout_title(title = "I'm layout title") +
            layout_annotation(
                theme = theme(plot.title = element_text(face = "bold"))
            )
    )
})

testthat::test_that("add `quad_anno()` works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    # successful conditions
    expect_no_error(quad_free(mtcars) +
        anno_top())
    expect_no_error(quad_free(mtcars) +
        anno_left())
    expect_no_error(quad_alignh(small_mat) +
        anno_left())
    expect_no_error(quad_alignv(small_mat) +
        anno_top())
    # error for incompatible data type
    expect_error(quad_alignh(small_mat) +
        anno_top())
    expect_error(quad_alignh(small_mat) +
        anno_bottom())
    expect_error(quad_alignv(small_mat) +
        anno_left())
    expect_error(quad_alignv(small_mat) +
        anno_right())
})

testthat::test_that("add `anno_init()` works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    # quad_free
    # for waiver()
    expect_error(quad_free() + anno_init("t"))
    expect_no_error(quad_free(mtcars) + anno_init("t"))
    # for function()
    expect_error(quad_free() + anno_init("t"))
    expect_error(quad_free(mtcars) + anno_init("t", ~small_mat))
    expect_no_error(quad_free(mtcars) + anno_init("t", ~mtcars))
    # for `NULL`
    expect_no_error(quad_free(mtcars) + anno_init("t", NULL))
    # for others
    expect_no_error(quad_free(mtcars) + anno_init("t", mtcars[1:10]))
    expect_no_error(quad_free(mtcars) + anno_init("t", small_mat))

    # quad_alignh
    # for waiver()
    expect_error(quad_alignh() + anno_init("l"))
    # cannot inherit a matrix, the top and bottom need a data frame
    expect_error(quad_alignh(small_mat) + anno_init("t"))
    expect_no_error(quad_alignh(small_mat) + anno_init("l"))
    # for function()
    expect_error(quad_alignh() + anno_init("l", ~small_mat))
    expect_error(quad_alignh(small_mat) + anno_init("l", ~mtcars))
    expect_no_error(quad_alignh(small_mat) + anno_init("t", ~mtcars))
    expect_error(quad_alignh(small_mat) + anno_init("l", ~ small_mat[1:5, ]))
    expect_no_error(quad_alignh(small_mat) + anno_init("l", ~small_mat))
    # for `NULL`
    expect_no_error(quad_alignh(mtcars) + anno_init("l", NULL))
    expect_no_error(quad_alignh(mtcars) + anno_init("t", NULL))
    # for others
    expect_error(quad_alignh(small_mat) + anno_init("l", small_mat[1:5, ]))
    expect_no_error(quad_alignh(small_mat) + anno_init("l", small_mat))
    expect_no_error(quad_alignh(small_mat) + anno_init("t", mtcars))

    # quad_alignv
    # for waiver()
    expect_error(quad_alignv() + anno_init("t"))
    # cannot inherit a matrix, the left and right need a data frame
    expect_error(quad_alignv(small_mat) + anno_init("l"))
    expect_no_error(quad_alignv(small_mat) + anno_init("t"))
    # for function()
    expect_error(quad_alignv() + anno_init("t", ~small_mat))
    expect_error(quad_alignv(small_mat) + anno_init("t", ~mtcars))
    expect_no_error(quad_alignv(small_mat) + anno_init("l", ~mtcars))
    expect_error(quad_alignv(small_mat) + anno_init("t", ~ small_mat[1:5, ]))
    expect_error(quad_alignv(small_mat) + anno_init("t", ~small_mat))
    expect_no_error(quad_alignv(small_mat) + anno_init("t", ~ t(small_mat)))
    # for `NULL`
    expect_no_error(quad_alignv(mtcars) + anno_init("t", NULL))
    expect_no_error(quad_alignv(mtcars) + anno_init("l", NULL))
    # for others
    expect_error(quad_alignv(small_mat) + anno_init("t", small_mat[1:5, ]))
    # should be transposed, so the nobs is not compatible
    expect_error(quad_alignv(small_mat) + anno_init("t", small_mat))
    expect_no_error(quad_alignv(small_mat) + anno_init("t", t(small_mat)))
    expect_no_error(quad_alignv(small_mat) + anno_init("l", mtcars))

    # quad_alignb
    # for waiver()
    expect_error(quad_alignb() + anno_init("t"))
    expect_no_error(quad_alignb(small_mat) + anno_init("l"))
    expect_no_error(quad_alignb(small_mat) + anno_init("t"))
    # for function()
    expect_error(quad_alignb() + anno_init("t", ~small_mat))
    expect_error(quad_alignb(small_mat) + anno_init("t", ~mtcars))
    expect_error(quad_alignb(small_mat) + anno_init("l", ~mtcars))
    expect_error(quad_alignb(small_mat) + anno_init("t", ~small_mat))
    expect_no_error(quad_alignb(small_mat) + anno_init("l", ~small_mat))
    expect_no_error(quad_alignb(small_mat) + anno_init("t", ~ t(small_mat)))
    # for `NULL`
    expect_no_error(quad_alignb(mtcars) + anno_init("t", NULL))
    expect_no_error(quad_alignb(mtcars) + anno_init("l", NULL))
    # for others
    expect_error(quad_alignb(small_mat) + anno_init("t", small_mat[1:5, ]))
    # should be transposed, so the nobs is not compatible
    expect_error(quad_alignb(small_mat) + anno_init("t", small_mat))
    expect_no_error(quad_alignb(small_mat) + anno_init("t", t(small_mat)))
    expect_no_error(quad_alignb(small_mat) + anno_init("l", small_mat))
})

testthat::test_that("add `align` object works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # quad_free()
    # cannot add align objects in `quad_free()`
    expect_error(quad_free(small_mat) +
        quad_anno("t") +
        align_dendro())
    expect_error(quad_free(small_mat) +
        quad_anno("t") +
        ggalign())
    expect_error({
        set.seed(1L)
        quad_free(small_mat) +
            quad_anno("l") +
            align_kmeans(3L)
    })

    # quad_alignh()
    # for vertical direction, we must manually provide the data frame
    # we cannot add `align` object in top and bottom
    expect_error(quad_alignh(small_mat) +
        anno_init("t", mtcars) +
        align_dendro())
    expect_doppelganger(
        "alignh-layout-annotation",
        quad_alignh(small_mat) +
            geom_boxplot(aes(value, .row_names)) +
            quad_anno("l") +
            align_dendro(k = 3L) +
            ggalign(data = rowSums) +
            geom_bar(aes(value, y = .y, fill = .panel),
                stat = "identity", orientation = "y"
            )
    )

    # quad_alignv()
    # for horizontal direction, we must manually provide the data frame
    # we cannot add `align` object in left and right
    expect_error(quad_alignv(small_mat) +
        anno_init("l", mtcars) +
        align_dendro())
    expect_doppelganger(
        "alignv-layout-annotation",
        quad_alignv(small_mat) +
            geom_boxplot(aes(.column_names, value)) +
            quad_anno("t") +
            align_dendro(k = 3L) +
            ggalign(data = rowSums) +
            geom_bar(aes(.x, value, fill = .panel), stat = "identity")
    )
})

testthat::test_that("`ggsave()` works well", {
    p <- ggheatmap(1:10)
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
