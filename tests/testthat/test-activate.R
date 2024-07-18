testthat::test_that("`activate and deactivate work well`", {
    p <- ggheat(1:10)
    expect_identical(get_context(p), NULL)
    for (position in GGHEAT_ELEMENTS) {
        expect_no_error(p2 <- activate(p, position))
        expect_identical(get_context(p2), position)
    }
    expect_error(activate(p, NULL))
    expect_identical(deactivate(p2), p)
})

testthat::test_that("`active` adding works well", {
    p <- ggheat(1:10)
    for (position in GGHEAT_ELEMENTS) {
        expect_no_error(p2 <- p + active(position))
        expect_identical(unclass(get_context(p2)), position)
    }
    expect_identical(p2 + active(), p)
})
