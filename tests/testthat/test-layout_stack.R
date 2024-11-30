testthat::test_that("`stack_align` works well", {
    # atomic was converted to one-column matrix
    x <- stack_alignh(1:10)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    expect_identical(x@layout, new_layout_coords(nobs = 10L))

    x <- stack_alignh(letters)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    expect_identical(x@layout, new_layout_coords(nobs = length(letters)))

    # stack with no data
    x <- stack_alignh()
    expect_s3_class(x@sizes, "unit")
    expect_identical(x@layout, new_layout_coords())
})

testthat::test_that("`stack_free` works well", {
    # atomic was converted to one-column data frame
    x <- stack_freeh(1:10)
    expect_true(is.data.frame(x@data))
    expect_identical(names(x@data), "value")
    expect_identical(x@layout, NULL)

    x <- stack_freeh(letters)
    expect_true(is.data.frame(x@data))
    expect_identical(names(x@data), "value")
    expect_identical(x@layout, NULL)

    # stack with no data
    x <- stack_freeh()
    expect_s3_class(x@sizes, "unit")
    expect_identical(x@layout, NULL)
})

testthat::test_that("`stack_align` add `align` object", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    expect_doppelganger("add-align-dendro", {
        stack_alignh(small_mat) +
            align_dendro()
    })
    expect_doppelganger("add-align-dendro-split", {
        stack_alignh(small_mat) +
            align_dendro(k = 3L)
    })
    expect_doppelganger("add-align-gg", {
        stack_alignh(small_mat) +
            ggalign() +
            geom_boxplot()
    })
    expect_doppelganger("add-align-kmeans", {
        set.seed(1L)
        stack_alignh(small_mat) +
            ggalign() +
            geom_boxplot(aes(value, .names, fill = .panel)) +
            align_kmeans(3L)
    })
})

testthat::test_that("`stack_align` add `quad_layout()` object works well", {
    # successful cases
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # error cases
    # stack without data add heatmap without data gave error
    expect_error(stack_alignh() + quad_alignb()) # we must provide data

    # sucessful cases
    # stack_alignh -------------------------------------
    expect_doppelganger("stack_alignh-add-quad_alignh", {
        stack_alignh(small_mat) +
            quad_alignh() +
            geom_boxplot(aes(value, factor(.y)))
    })
    expect_doppelganger("stack_alignh-add-quad_alignv", {
        stack_alignh(small_mat) +
            quad_alignv() +
            geom_boxplot(aes(factor(.x), value))
    })
    expect_doppelganger("stack_alignh-add-quad_alignb", {
        stack_alignh(small_mat) +
            quad_alignb(NULL, aes(.x, .y)) +
            geom_tile(aes(fill = value))
    })
    expect_doppelganger("stack_alignh-add-ggheatmap", {
        stack_alignh(small_mat) + ggheatmap()
    })
    expect_doppelganger("stack_alignh-add-ggheatmap_with_name_as_mapping", {
        stack_alignh(small_mat) +
            ggheatmap(filling = NULL) +
            geom_tile(aes(.column_names, .row_names, fill = value))
    })

    # stack_alignv -------------------------------------
    expect_doppelganger("stack_alignv-add-quad_alignh", {
        stack_alignv(small_mat) +
            quad_alignh() +
            geom_boxplot(aes(value, factor(.y)))
    })
    expect_doppelganger("stack_alignv-add-quad_alignv", {
        stack_alignv(small_mat) +
            quad_alignv() +
            geom_boxplot(aes(factor(.x), value))
    })
    expect_doppelganger("stack_alignv-add-quad_alignb", {
        stack_alignv(small_mat) +
            quad_alignb(NULL, aes(.x, .y)) +
            geom_tile(aes(fill = value))
    })
    expect_doppelganger("stack_alignv-add-ggheatmap", {
        stack_alignv(small_mat) + ggheatmap()
    })
    expect_doppelganger("stack_alignv-add-ggheatmap_with_name_as_mapping", {
        stack_alignv(small_mat) +
            ggheatmap(filling = NULL) +
            geom_tile(aes(.column_names, .row_names, fill = value))
    })
})

testthat::test_that("`stack_align` add `quad_switch()` works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)

    set.seed(1L)
    expect_doppelganger(
        "add-quad_switch",
        stack_alignh(small_mat) +
            align_kmeans(3L) +
            ggheatmap() +
            quad_switch("l") +
            align_dendro()
    )

    # absolute size works well
    expect_doppelganger(
        "stack with absolute size 1",
        stack_alignh(matrix(seq_len(81), nrow = 9L)) +
            ggheatmap() +
            quad_switch("t", size = unit(6, "cm")) +
            align_dendro()
    )
    expect_doppelganger(
        "stack with absolute size 2",
        stack_alignh(matrix(seq_len(81), nrow = 9L)) +
            ggheatmap() +
            quad_switch("t", size = unit(6, "cm")) +
            align_dendro(size = unit(10, "cm"))
    )
    expect_doppelganger(
        "stack with mix absolute and null size 1",
        stack_alignh(matrix(seq_len(81), nrow = 9L)) +
            ggheatmap() +
            quad_switch("t", size = unit(6, "cm")) +
            align_dendro(size = unit(10, "cm")) +
            align_dendro()
    )
    expect_doppelganger(
        "stack with mix absolute and null size 2",
        stack_alignh(matrix(seq_len(81), nrow = 9L)) +
            ggheatmap() +
            quad_switch("t", size = unit(6, "cm")) +
            align_dendro() +
            align_dendro()
    )
})

testthat::test_that("add `stack_active` object works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    # change parameters for stack self
    p <- stack_alignh(small_mat)
    p2 <- p + stack_active(sizes = unit(1, "cm")) -
        plot_align(guides = "tlbr", free_labs = "tlbr")
    expect_identical(p2@sizes, unit(1, "cm"))
    controls <- p2@controls
    expect_identical(controls$plot_align$guides, "tlbr")
    expect_identical(controls$plot_align$free_labs, "tlbr")
    expect_identical(controls$plot_data, new_plot_data(NULL))
})

testthat::test_that("add `with_quad()` works as expected", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    expect_doppelganger(
        "subtract_with_quad_default",
        stack_alignv(small_mat) +
            align_dendro() +
            ggtitle("I'm from the parent stack") +
            ggheatmap() +
            anno_top() +
            align_dendro() +
            ggtitle("I'm from the nested heatmap") +
            anno_left() +
            align_dendro() +
            ggtitle("I'm from the nested heatmap") +
            stack_active() +
            align_dendro() +
            ggtitle("I'm from the parent stack") -
            with_quad(theme(plot.background = element_rect(fill = "red")))
    )
    expect_doppelganger(
        "subtract_with_quad_set_position_null",
        stack_alignv(small_mat) +
            align_dendro() +
            ggtitle("I'm from the parent stack") +
            ggheatmap() +
            anno_top() +
            align_dendro() +
            ggtitle("I'm from the nested heatmap") +
            anno_left() +
            align_dendro() +
            ggtitle("I'm from the nested heatmap") +
            stack_active() +
            align_dendro() +
            ggtitle("I'm from the parent stack") -
            with_quad(
                theme(plot.background = element_rect(fill = "red")), NULL
            )
    )
    expect_doppelganger(
        "subtract_with_quad_set_position",
        stack_alignv(small_mat) +
            align_dendro() +
            ggtitle("I'm from the parent stack") +
            ggheatmap() +
            anno_top() +
            align_dendro() +
            ggtitle("I'm from the nested heatmap") +
            anno_left() +
            align_dendro() +
            ggtitle("I'm from the nested heatmap") +
            stack_active() +
            align_dendro() +
            ggtitle("I'm from the parent stack") -
            with_quad(
                theme(plot.background = element_rect(fill = "red")), "l"
            )
    )
})

testthat::test_that("`ggsave()` works well", {
    p <- ggstack(matrix(seq_len(81), nrow = 9L), "h") +
        ggheatmap() +
        anno_top(size = unit(6, "cm")) +
        align_dendro() +
        align_dendro()
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
