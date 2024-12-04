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

    # warning for incompatible data type
    expect_warning(quad_alignh(small_mat) +
        anno_top())
    expect_no_warning(quad_alignh(small_mat) +
        anno_top(initialize = FALSE))
    expect_no_warning(quad_alignh(small_mat) +
        anno_top(initialize = TRUE))

    expect_warning(quad_alignh(small_mat) +
        anno_bottom())
    expect_no_warning(quad_alignh(small_mat) +
        anno_bottom(initialize = FALSE))
    expect_no_warning(quad_alignh(small_mat) +
        anno_bottom(initialize = TRUE))

    # warning for incompatible data type
    expect_warning(quad_alignv(small_mat) +
        anno_left())
    expect_no_warning(quad_alignv(small_mat) +
        anno_left(initialize = FALSE))
    expect_no_warning(quad_alignv(small_mat) +
        anno_left(initialize = TRUE))

    expect_warning(quad_alignv(small_mat) +
        anno_right())
    expect_no_warning(quad_alignv(small_mat) +
        anno_right(initialize = FALSE))
    expect_no_warning(quad_alignv(small_mat) +
        anno_right(initialize = TRUE))
})

testthat::test_that("add `align` object works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # quad_free()
    # cannot add align objects in `quad_free()`
    expect_snapshot_error(quad_free(small_mat) +
        quad_anno("t") +
        align_dendro())
    expect_snapshot_error(quad_free(small_mat) +
        quad_anno("t") +
        ggalign())
    expect_snapshot_error({
        set.seed(1L)
        quad_free(small_mat) +
            quad_anno("l") +
            align_kmeans(3L)
    })

    # quad_alignh()
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

testthat::test_that("add `align` object builds well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # quad_alignh()
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


testthat::test_that("add `with_quad()` works as expected", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    expect_doppelganger(
        "add_with_quad_default",
        ggheatmap(small_mat) +
            anno_left(size = 0.2) +
            align_dendro() +
            with_quad(theme(plot.background = element_rect(fill = "red")))
    )
    expect_doppelganger(
        "add_with_quad_set_position_null",
        ggheatmap(small_mat) +
            anno_left(size = 0.2) +
            align_dendro() +
            with_quad(
                theme(plot.background = element_rect(fill = "red")), NULL
            )
    )
    expect_doppelganger(
        "subtract_with_quad_default",
        ggheatmap(small_mat) +
            anno_left(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) +
            anno_top(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) +
            anno_bottom(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) -
            with_quad(
                scale_color_brewer(palette = "Dark2", name = "Top and bottom")
            )
    )
    expect_doppelganger(
        "subtract_with_quad_set_position",
        ggheatmap(small_mat) +
            anno_left(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) +
            anno_top(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) +
            anno_bottom(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) -
            with_quad(theme(plot.background = element_rect(fill = "red")), "tl")
    )
    expect_doppelganger(
        "subtract_with_quad_set_position_null",
        ggheatmap(small_mat) +
            anno_left(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) +
            anno_top(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) +
            anno_bottom(size = 0.2) +
            align_dendro(aes(color = branch), k = 3L) -
            with_quad(
                theme(plot.background = element_rect(fill = "red")), NULL
            )
    )
})

testthat::test_that("add `stack_layout()` builds well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # quad_free() ------------------------------------------
    expect_snapshot_error(quad_free(mpg) + stack_freev())
    # annotaion has been initialized
    expect_snapshot_error(quad_free(mpg) + anno_top() + stack_freev())
    # add nested layout
    expect_snapshot_error(quad_free(mpg) + anno_top(initialize = FALSE) +
        (stack_freev() + quad_free(mpg) + quad_free(mpg)))
    # incompatible direction
    expect_snapshot_error(quad_free(mpg) + anno_top(initialize = FALSE) +
        stack_freeh())
    # incompatible aligning type
    expect_snapshot_error(quad_free(mpg) + anno_top(initialize = FALSE) +
        stack_alignv())

    # quad_alignh() ---------------------------------------
    expect_snapshot_error(quad_alignh(small_mat) + stack_alignh())
    expect_snapshot_error(quad_alignh(small_mat) + stack_freev())

    # annotaion has been initialized
    expect_snapshot_error(quad_alignh(small_mat) +
        anno_top(initialize = TRUE) +
        stack_freev())
    expect_snapshot_error(
        quad_alignh(mpsmall_matg) + anno_left() + stack_alignh()
    )

    # add nested layout
    expect_snapshot_error(
        quad_alignh(small_mat) + anno_top(initialize = FALSE) +
            (stack_freev() + quad_free(mpg) + quad_free(mpg))
    )
    expect_snapshot_error(
        quad_alignh(small_mat) + anno_left(initialize = FALSE) +
            (stack_alignh() + ggheatmap(small_mat) + ggheatmap(small_mat))
    )

    # incompatible direction
    expect_snapshot_error(
        quad_alignh(small_mat) + anno_top(initialize = FALSE) +
            stack_freeh()
    )

    # incompatible aligning type
    expect_snapshot_error(
        quad_alignh(small_mat) + anno_top(initialize = FALSE) +
            stack_alignv()
    )
    # update coords correctly
    quad <- quad_alignh(small_mat) +
        anno_right() +
        anno_left(size = 0.2, initialize = FALSE) +
        (stack_alignh(small_mat) + align_dendro(k = 4))
    expect_identical(quad@horizontal, quad@left@layout)
    expect_identical(quad@horizontal, quad@right@layout)

    quad <- quad_alignh(small_mat) +
        anno_left() +
        anno_right(size = 0.2, initialize = FALSE) +
        (stack_alignh(small_mat) + align_dendro(k = 4))
    expect_identical(quad@horizontal, quad@right@layout)
    expect_identical(quad@horizontal, quad@left@layout)

    # quad_alignv() ---------------------------------------
    expect_snapshot_error(quad_alignv(small_mat) + stack_alignv())
    expect_snapshot_error(quad_alignv(small_mat) + stack_freeh())

    # annotaion has been initialized
    expect_snapshot_error(quad_alignv(small_mat) + anno_top() + stack_freeh())
    expect_snapshot_error(
        quad_alignv(small_mat) + anno_left(initialize = TRUE) +
            stack_alignv()
    )

    # add nested layout
    expect_snapshot_error(
        quad_alignv(small_mat) + anno_top(initialize = FALSE) +
            (stack_freeh() + quad_free(mpg) + quad_free(mpg))
    )
    expect_snapshot_error(
        quad_alignv(small_mat) + anno_left(initialize = FALSE) +
            (stack_alignv() + ggheatmap(small_mat) + ggheatmap(small_mat))
    )

    # incompatible direction
    expect_snapshot_error(
        quad_alignv(small_mat) + anno_top(initialize = FALSE) +
            stack_freeh()
    )

    # incompatible aligning type
    expect_snapshot_error(
        quad_alignv(small_mat) + anno_top(initialize = FALSE) +
            stack_alignh()
    )

    # update coords correctly
    quad <- quad_alignv(small_mat) +
        anno_bottom() +
        anno_top(size = 0.2, initialize = FALSE) +
        (stack_alignv(t(small_mat)) + align_dendro(k = 4))
    expect_identical(quad@vertical, quad@top@layout)
    expect_identical(quad@vertical, quad@bottom@layout)

    quad <- quad_alignv(small_mat) +
        anno_top() +
        anno_bottom(size = 0.2, initialize = FALSE) +
        (stack_alignv(t(small_mat)) + align_dendro(k = 4))
    expect_identical(quad@vertical, quad@bottom@layout)
    expect_identical(quad@vertical, quad@top@layout)
})

testthat::test_that("add `stack_layout()` builds well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # quad_alignh() ------------------------------------------
    expect_doppelganger(
        "quad_alignh, add stack_alignh() in the top",
        quad_alignh(small_mat) +
            anno_left(size = 0.2, initialize = FALSE) +
            (stack_alignh(small_mat) + align_dendro(k = 4))
    )

    # quad_alignv() ---------------------------------------
    expect_doppelganger(
        "quad_alignv, add stack_alignv() in the top",
        quad_alignv(small_mat) +
            anno_top(size = 0.2, initialize = FALSE) +
            (stack_alignv(t(small_mat)) + align_dendro(k = 4))
    )
})

testthat::test_that("add `cross_layout()` builds well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # quad_alignh() ------------------------------------------
    expect_doppelganger(
        "quad_alignh, add stack_alignh() in the top",
        quad_alignh(small_mat) +
            anno_left(size = 0.2, initialize = FALSE) +
            (stack_alignh(small_mat) + align_dendro(k = 4))
    )

    # quad_alignv() ---------------------------------------
    expect_doppelganger(
        "quad_alignv, add stack_alignv() in the top",
        quad_alignv(small_mat) +
            anno_top(size = 0.2, initialize = FALSE) +
            (stack_alignv(t(small_mat)) + align_dendro(k = 4))
    )
})

testthat::test_that("`ggsave()` works well", {
    p <- ggheatmap(1:10)
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
