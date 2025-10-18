test_that("free_space() sets attribute on ggplot", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p2 <- free_space(p, "tb")
    expect_s3_class(p2, "ggalign_free_space")
    expect_equal(attr(p2, "ggalign_free_spaces"), "tb")
})

test_that("free_space() on free_space appends positions", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p <- free_space(p, "t")
    p2 <- free_space(p, "b")
    expect_equal(attr(p2, "ggalign_free_spaces"), union_position("t", "b"))
})

test_that("free_space() on free_align excludes free_axes", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    class(p) <- c("ggalign_free_align", class(p))
    attr(p, "ggalign_free_axes") <- "t"
    out <- free_space(p, "tb")
    expect_equal(attr(out, "ggalign_free_spaces"), "b")
})

test_that("free_space() works with alignpatch objects", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    class(p) <- c("alignpatches", class(p))
    out <- free_space(p, "r")
    expect_s3_class(out, "ggalign_free_space")
    expect_equal(attr(out, "ggalign_free_spaces"), "r")
})

test_that("patch.free_space() returns a ggproto object with spaces", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p <- free_space(p, "lr")
    proto <- patch(p)
    expect_s3_class(proto, "PatchFreeSpace")
    expect_equal(proto$spaces, setup_position("lr"))
})
