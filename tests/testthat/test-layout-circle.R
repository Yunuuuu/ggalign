testthat::test_that("`circle_discrete()` works well", {
    # atomic was converted to one-column matrix
    x <- circle_discrete(1:10)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    expect_identical(x@domain, DiscreteDomain(nobs = 10L))

    x <- circle_discrete(letters)
    expect_true(is.matrix(x@data))
    expect_identical(ncol(x@data), 1L)
    expect_identical(x@domain, DiscreteDomain(nobs = length(letters)))

    # circle with no data
    x <- circle_discrete()
    expect_identical(x@domain, DiscreteDomain())
})

testthat::test_that("`circle_continuous()` works well", {
    # atomic was converted to one-column data frame
    x <- circle_continuous(1:10)
    expect_true(is.data.frame(x@data))
    expect_identical(names(x@data), "value")
    expect_identical(x@domain, NULL)

    x <- circle_continuous(letters)
    expect_true(is.data.frame(x@data))
    expect_identical(names(x@data), "value")
    expect_identical(x@domain, NULL)

    # circle with no data
    x <- circle_continuous()
    expect_identical(x@domain, NULL)
})

testthat::test_that("`circle_discrete()` add `align-` object builds well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    # visual test
    expect_doppelganger("circle_discrete, add align_dendro()", {
        circle_discrete(small_mat) +
            align_dendro()
    })

    expect_doppelganger("circle_discrete, add align_dendro(k = 3)", {
        circle_discrete(small_mat) +
            align_dendro(k = 3L)
    })

    expect_doppelganger("circle_discrete, add ggalign()", {
        circle_discrete(small_mat) +
            ggalign() +
            geom_boxplot(aes(.discrete_x, y = value))
    })

    expect_doppelganger("circle_discrete, add align_kmeans()", {
        set.seed(1L)
        circle_discrete(small_mat) +
            ggalign() +
            geom_boxplot(aes(.discrete_x, value, fill = .panel)) +
            align_kmeans(3L)
    })
})

testthat::test_that("`circle_discrete()` error adding", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    expect_snapshot_error(circle_discrete(small_mat) + quad_alignh())
    expect_snapshot_error(
        circle_discrete(matrix(seq_len(81), nrow = 9L)) +
            ggcross()
    )
})

testthat::test_that("`ggsave()` works well", {
    set.seed(1L)
    small_mat <- matrix(rnorm(72), nrow = 8)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
    p <- circle_discrete(small_mat) +
        ggalign() +
        geom_boxplot(aes(.discrete_x, value, fill = .panel)) +
        align_kmeans(3L)
    expect_no_error(ggplot2::ggsave(tempfile(fileext = ".png"), plot = p))
})
