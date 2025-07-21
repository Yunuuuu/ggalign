test_that("free_lab() adds correct class and attribute for ggplot", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p2 <- free_lab(p, "tb")
    expect_s3_class(p2, "free_lab")
    expect_equal(attr(p2, "free_labs"), "tb")
})

test_that("free_lab() merges labs for free_lab input", {
    p <- ggplot(mtcars, aes(mpg, wt))
    p2 <- free_lab(p, "t")
    p3 <- free_lab(p2, "b")
    expect_equal(attr(p3, "free_labs"), "tb")
})

test_that("free_lab() respects setdiff in free_align", {
    obj <- structure(list(), class = c("free_align", "ggplot"))
    attr(obj, "free_axes") <- "t"
    res <- free_lab(obj, "t")
    expect_identical(res, obj) # no-op
})

test_that("free_lab() respects setdiff in free_borders", {
    obj <- structure(list(), class = c("free_borders", "ggplot"))
    attr(obj, "free_borders") <- "l"
    res <- free_lab(obj, "l")
    expect_identical(res, obj) # no-op
})

test_that("free_lab.default() errors on unsupported objects", {
    expect_error(free_lab(1), "Cannot use with")
})

test_that("alignpatch.free_lab() returns a ggproto object with free_labs", {
    p <- structure(list(), class = c("free_lab", "ggplot"))
    attr(p, "free_labs") <- "b"
    proto <- alignpatch(p)
    expect_s3_class(proto, "PatchFreeLab")
    expect_equal(proto$free_labs, "bottom")
})
