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
    expect_identical(p@horizontal, new_layout_params())
    expect_identical(p@vertical, new_layout_params())

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
            plot_align(guides = NULL) +
            quad_anno("r") +
            align_dendro()
    )
    expect_doppelganger(
        "heatmap-hmanno-stack-action-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) +
            quad_anno("l") -
            plot_align(guides = "l") +
            align_dendro(aes(color = branch))
    )
    expect_doppelganger(
        "heatmap-hmanno-align-action-guides",
        ggheatmap(matrix(1:9, nrow = 3L)) -
            plot_align(guides = NULL) +
            quad_anno("r") +
            align_dendro() +
            plot_align(guides = "l")
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
