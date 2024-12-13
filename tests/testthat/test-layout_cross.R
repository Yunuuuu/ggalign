testthat::test_that("`cross_align` add `align-` object works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # cross_alignh() ----------------------------------------
    # layout `index` is updated correctly
    stack <- cross_alignh(small_mat) +
        align_dendro() +
        ggcross() +
        align_dendro(method = "ward.D2")
    expect_identical(
        stack@layout$index,
        order2(ggalign_stat(stack, 3L))
    )
    expect_identical(
        stack@index_list,
        list(order2(ggalign_stat(stack, 1L)))
    )

    # cross_alignv() ----------------------------------------
    # layout `index` is updated correctly
    stack <- cross_alignv(small_mat) +
        align_dendro() +
        ggcross() +
        align_dendro(method = "ward.D2")
    expect_identical(
        stack@layout$index,
        order2(ggalign_stat(stack, 3L))
    )
    expect_identical(
        stack@index_list,
        list(order2(ggalign_stat(stack, 1L)))
    )
})
