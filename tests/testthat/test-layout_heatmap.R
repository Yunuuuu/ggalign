testthat::test_that("`ggheatmap` works well", {
    # atomic was converted to one-column matrix
    x <- ggheatmap(1:10)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    x <- ggheatmap(letters)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)

    # heatmap with no data
    p <- ggheatmap()
    expect_identical(p@horizontal, new_layout_coords())
    expect_identical(p@vertical, new_layout_coords())

    # heatmap with data
    expect_doppelganger("heatmap-numeric", ggheatmap(1:10))
    expect_doppelganger(
        "heatmap-numeric-to-factor",
        ggheatmap(1:10, aes(fill = factor(value)))
    )
    expect_doppelganger("heatmap-character", ggheatmap(letters))
    expect_doppelganger("heatmap-matrix", ggheatmap(matrix(1:9, nrow = 3L)))
    testthat::skip_on_ci() # I don't know why this will fail in github CI
    expect_doppelganger("heatmap-data.frame", ggheatmap(data.frame(1:10)))
})

testthat::test_that("`ggheatmap` add `quad_switch()` works well", {
    expect_doppelganger(
        "heatmap-hmanno-width",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            quad_active(width = unit(1, "cm"))
    )
    expect_doppelganger(
        "heatmap-hmanno-height",
        ggheatmap(matrix(1:9, nrow = 3L)) + quad_active(height = unit(1, "cm"))
    )
    expect_doppelganger(
        "heatmap-hmanno-null-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) -
            scheme_align(guides = NULL) +
            quad_anno("r") +
            align_dendro()
    )
    expect_doppelganger(
        "heatmap-hmanno-stack-action-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            quad_anno("l") -
            scheme_align(guides = "l") +
            align_dendro(aes(color = branch))
    )
    expect_doppelganger(
        "heatmap-hmanno-align-action-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) -
            scheme_align(guides = NULL) +
            quad_anno("r") +
            align_dendro() +
            scheme_align(guides = "l")
    )
})

testthat::test_that("`ggoncoplot` works well", {
    mat <- read.table(textConnection(
        "s1,s2,s3
         g1,snv; indel,snv,indel
         g2,,snv;indel,snv
         g3,snv,,indel;snv"
    ), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)

    expect_doppelganger(
        "single-oncoplot",
        ggoncoplot(mat, map_width = c(snv = 0.5), map_height = c(indel = 0.9))
    )
    expect_doppelganger(
        "oncoplot-with-annotation",
        ggoncoplot(mat, map_width = c(snv = 0.5), map_height = c(indel = 0.9)) +
            # Note that guide legends from `geom_tile` and `geom_bar` are
            # different. Although they appear similar, the internal mechanisms
            # won't collapse the guide legends. Therefore, we remove the guide
            # legends from `geom_tile`.
            guides(fill = "none") +
            anno_top(size = 0.5) +
            ggalign() +
            geom_bar(aes(fill = value), data = function(x) {
                subset(x, !is.na(value))
            }) +
            anno_right(size = 0.5) +
            ggalign() +
            geom_bar(aes(fill = value), orientation = "y", data = function(x) {
                subset(x, !is.na(value))
            }) &
            scale_fill_brewer(palette = "Dark2", na.translate = FALSE)
    )
})

testthat::test_that("`breaks` of `scale_*_()` works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    # continuous scales
    expect_doppelganger(
        "no_names_no_breaks_and_labels",
        {
            no_names <- small_mat
            colnames(no_names) <- NULL
            ggheatmap(no_names) + scale_x_continuous()
        }
    )
    expect_doppelganger(
        "continuous_no_breaks",
        ggheatmap(small_mat) + scale_x_continuous(breaks = NULL)
    )
    expect_doppelganger(
        "continuous_character_breaks",
        ggheatmap(small_mat) +
            scale_x_continuous(breaks = c("column3", "column5")) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "continuous_integer_breaks",
        ggheatmap(small_mat) +
            scale_x_continuous(breaks = c(3, 5)) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_error(
        ggplot2::ggsave(tempfile(fileext = ".png"),
            plot = ggheatmap(small_mat) + scale_x_continuous(breaks = c(3.5, 5))
        )
    )
    expect_doppelganger(
        "continuous_integer_as_is_breaks",
        ggheatmap(small_mat) +
            scale_x_continuous(breaks = I(3:4)) +
            anno_top() +
            align_dendro(k = 3L)
    )

    # discrete scales
    expect_doppelganger(
        "discrete_no_breaks",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(breaks = NULL)
    )
    expect_doppelganger(
        "discrete_character_breaks",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(breaks = c("column3", "column5")) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "discrete_integer_breaks",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(breaks = c(3, 5)) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "discrete_integer_as_is_breaks",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(breaks = I(3:4)) +
            anno_top() +
            align_dendro(k = 3L)
    )
})

testthat::test_that("`labels` of `scale_*_()` works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    # continuous scales
    expect_doppelganger(
        "continuous_no_labels",
        ggheatmap(small_mat) +
            scale_x_continuous(labels = NULL) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "continuous_labels",
        ggheatmap(small_mat) +
            scale_x_continuous(labels = letters[seq_len(ncol(small_mat))]) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "continuous_labels_as_is",
        ggheatmap(small_mat) +
            scale_x_continuous(labels = I(letters[seq_len(ncol(small_mat))])) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "continuous_labels_match_breaks",
        ggheatmap(small_mat) +
            scale_x_continuous(breaks = c(5, 3), labels = c("a", "b"))
    )
    expect_doppelganger(
        "continuous_labels_as_is_match_breaks",
        ggheatmap(small_mat) +
            scale_x_continuous(breaks = c(5, 3), labels = I(c("a", "b")))
    )

    # discrete scales
    expect_doppelganger(
        "discrete_no_labels",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(labels = NULL) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "discrete_labels",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(labels = letters[seq_len(ncol(small_mat))]) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "discrete_labels_as_is",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(labels = I(letters[seq_len(ncol(small_mat))])) +
            anno_top() +
            align_dendro(k = 3L)
    )
    expect_doppelganger(
        "discrete_labels_match_breaks",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(breaks = c(5, 3), labels = c("a", "b"))
    )
    expect_doppelganger(
        "discrete_labels_as_is_match_breaks",
        ggheatmap(small_mat, filling = FALSE) +
            geom_tile(aes(.column_names, .row_names, fill = value)) +
            scale_x_discrete(breaks = c(5, 3), labels = I(c("a", "b")))
    )
})
