testthat::test_that("`activate work well`", {
    p <- ggheatmap(1:10)
    expect_identical(get_context(p), NULL)
    for (position in GGHEAT_ELEMENTS) {
        expect_no_error(p2 <- activate(p, position))
        expect_identical(get_context(p2), position)
    }
    expect_identical(get_context(activate(p2, NULL)), NULL)
})

testthat::test_that("`hmanno` adding works well", {
    p <- ggheatmap(1:10)
    for (position in GGHEAT_ELEMENTS) {
        expect_no_error(p2 <- p + hmanno(position))
        expect_identical(unclass(get_context(p2)), position)
    }
    expect_identical(get_context(p2 + hmanno(NULL)), NULL)
})
