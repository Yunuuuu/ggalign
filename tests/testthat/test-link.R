test_that("`cross_link()` horizontal works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    expect_doppelganger(
        "cross_link horizontal, with link_line() default",
        stack_crossh(small_mat) +
            align_dendro() +
            scale_x_reverse() +
            ggheatmap() +
            stack_active() +
            cross_link(link_line()) +
            ggheatmap() &
            theme(axis.text.x = element_text(angle = -60, hjust = 0))
    )

    expect_doppelganger(
        "cross_link horizontal, with link_line() manual",
        stack_crossh(small_mat) +
            align_dendro() +
            scale_x_reverse() +
            ggheatmap() +
            stack_active() +
            cross_link(link_line(1 ~ 1:2)) +
            ggheatmap() &
            theme(axis.text.x = element_text(angle = -60, hjust = 0))
    )

    expect_doppelganger(
        "cross_link horizontal, with link_tetragon() manual",
        stack_crossh(small_mat) +
            align_dendro() +
            scale_x_reverse() +
            ggheatmap() +
            stack_active() +
            cross_link(link_tetragon(1 ~ 1:2)) +
            ggheatmap() &
            theme(axis.text.x = element_text(angle = -60, hjust = 0))
    )
})

test_that("`ggmark()` vertical works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    expect_doppelganger(
        "cross_link vertical, with link_line() default",
        stack_crossv(small_mat) +
            align_dendro() +
            scale_x_reverse() +
            ggheatmap() +
            stack_active() +
            cross_link(link_line()) +
            ggheatmap() &
            theme(axis.text.x = element_text(angle = -60, hjust = 0))
    )

    expect_doppelganger(
        "cross_link vertical, with link_line() manual",
        stack_crossv(small_mat) +
            align_dendro() +
            scale_x_reverse() +
            ggheatmap() +
            stack_active() +
            cross_link(link_line(1 ~ 1:2)) +
            ggheatmap() &
            theme(axis.text.x = element_text(angle = -60, hjust = 0))
    )

    expect_doppelganger(
        "cross_link vertical, with link_tetragon() manual",
        stack_crossv(small_mat) +
            align_dendro() +
            scale_x_reverse() +
            ggheatmap() +
            stack_active() +
            cross_link(link_tetragon(1 ~ 1:2)) +
            ggheatmap() &
            theme(axis.text.x = element_text(angle = -60, hjust = 0))
    )
})
