testthat::test_that("`cross_align` add `align-` object works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # stack_crossh() ----------------------------------------
    # layout `index` is updated correctly
    stack <- stack_crossh(small_mat) +
        align_dendro() +
        ggcross() +
        align_dendro(method = "ward.D2")
    expect_identical(
        stack@design$index,
        order2(ggalign_stat(stack, 3L))
    )
    expect_identical(
        .subset2(.subset2(stack@odesign, 1L), "index"),
        order2(ggalign_stat(stack, 1L))
    )

    # cross_alignv() ----------------------------------------
    # layout `index` is updated correctly
    stack <- stack_cross("v", small_mat) +
        align_dendro() +
        ggcross() +
        align_dendro(method = "ward.D2")
    expect_identical(
        stack@design$index,
        order2(ggalign_stat(stack, 3L))
    )
    expect_identical(
        .subset2(.subset2(stack@odesign, 1L), "index"),
        order2(ggalign_stat(stack, 1L))
    )
})
