test_that("free_lab() adds correct class and attribute for ggplot", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p2 <- free_lab(p, "tb")
    expect_s3_class(p2, "ggalign_free_lab")
    expect_equal(attr(p2, "ggalign_free_labs"), "tb")
})

test_that("free_lab() merges labs for free_lab input", {
    p <- ggplot(mtcars, aes(mpg, wt))
    p2 <- free_lab(p, "t")
    p3 <- free_lab(p2, "b")
    expect_equal(attr(p3, "ggalign_free_labs"), "tb")
})

test_that("free_lab() respects setdiff in free_align", {
    obj <- structure(list(), class = c("ggalign_free_align", "ggplot"))
    attr(obj, "ggalign_free_axes") <- "t"
    res <- free_lab(obj, "t")
    expect_identical(res, obj) # no-op
})

test_that("free_lab() respects setdiff in free_border", {
    obj <- structure(list(), class = c("ggalign_free_border", "ggplot"))
    attr(obj, "ggalign_free_borders") <- "l"
    res <- free_lab(obj, "l")
    expect_identical(res, obj) # no-op
})

test_that("alignpatch.free_lab() returns a ggproto object with labs", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p <- free_lab(p, "b")
    proto <- alignpatch(p)
    expect_s3_class(proto, "PatchFreeLab")
    expect_equal(proto$labs, "bottom")
})
