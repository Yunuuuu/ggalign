test_that("`ggmark()` works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    expect_doppelganger(
        "ggmark, with mark_line()",
        ggheatmap(small_mat) +
            theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
            anno_right() +
            align_kmeans(3L) +
            ggmark(mark_line()) +
            geom_boxplot(aes(.names, value))
    )
    expect_doppelganger(
        "ggmark, with mark_tetragon()",
        ggheatmap(small_mat) +
            theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
            anno_right() +
            align_kmeans(3L) +
            ggmark(mark_tetragon()) +
            geom_boxplot(aes(.names, value))
    )
})
