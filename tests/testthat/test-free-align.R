test_that("free_align assigns class and axes attribute", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))

    # Apply free_align
    fa <- free_align(p)

    # Should have class "free_align"
    expect_s3_class(fa, "free_align")

    # Should retain ggplot class
    expect_s3_class(fa, "ggplot")

    # Should have the correct "free_axes" attribute
    expect_identical(attr(fa, "free_axes"), "tlbr")
})

test_that("free_align merges with existing free_align", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))

    fa1 <- free_align(p, axes = "l")
    fa2 <- free_align(fa1, axes = "b")

    expect_s3_class(fa2, "free_align")
    expect_true(all(c("l", "b") %in% strsplit(attr(fa2, "free_axes"), "")[[1]]))
})

test_that("free_align removes overlapping axes from free_lab", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    class(p) <- c("free_lab", class(p))
    attr(p, "free_labs") <- "l"

    fa <- free_align(p, axes = "l")

    # free_lab should be removed when all overlapping axes are freed
    expect_false(inherits(fa, "free_lab"))
    expect_null(attr(fa, "free_labs"))
})

test_that("free_align errors on unsupported types", {
    expect_snapshot_error(free_align(list(a = 1)), )
})
