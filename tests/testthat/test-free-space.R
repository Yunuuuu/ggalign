test_that("free_space() sets attribute on ggplot", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p2 <- free_space(p, "tb")
    expect_s3_class(p2, "free_space")
    expect_equal(attr(p2, "free_spaces"), "tb")
})

test_that("free_space() validates input", {
    expect_error(free_space(1), "Cannot use with")
})

test_that("free_space() on free_space appends positions", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p <- free_space(p, "t")
    p2 <- free_space(p, "b")
    expect_equal(attr(p2, "free_spaces"), union_position("t", "b"))
})

test_that("free_space() on free_align excludes free_axes", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    class(p) <- c("free_align", class(p))
    attr(p, "free_axes") <- "t"
    out <- free_space(p, "tb")
    expect_equal(attr(out, "free_spaces"), "b")
})

test_that("free_space() works with alignpatch objects", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    class(p) <- c("alignpatches", class(p))
    out <- free_space(p, "r")
    expect_s3_class(out, "free_space")
    expect_equal(attr(out, "free_spaces"), "r")
})

test_that("alignpatch.free_space() merges free_spaces", {
    p <- ggplot(mtcars, aes(mpg, wt)) +
        geom_point()
    p <- free_space(p, "lr")
    obj <- alignpatch(p)
    expect_true(inherits(obj, "PatchFreeSpace"))
    expect_equal(obj$free_spaces, split_position("lr"))
})
