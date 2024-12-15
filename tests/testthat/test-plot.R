test_that("add ggplot elements works well", {
    expect_no_error(ggalign() + geom_point())
    expect_no_error(ggfree() + geom_point())
    expect_no_error(ggcross() + geom_point())
})
