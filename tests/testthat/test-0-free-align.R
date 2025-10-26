test_that("free_align assigns class and axes attribute", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))

    # Apply free_align
    fa <- free_align(p)

    # Should have class "free_align"
    expect_s3_class(fa, "ggalign_free_align")

    # Should retain ggplot class
    expect_s3_class(fa, "ggplot")

    # Should have the correct "free_axes" attribute
    expect_identical(attr(fa, "ggalign_free_axes"), "tlbr")
})

test_that("free_align merges with existing free_align", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))

    fa1 <- free_align(p, axes = "l")
    fa2 <- free_align(fa1, axes = "b")

    expect_s3_class(fa2, "ggalign_free_align")
    expect_true(all(c("l", "b") %in% strsplit(attr(fa2, "ggalign_free_axes"), "")[[1]]))
})

test_that("free_align removes overlapping axes from free_lab", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    class(p) <- c("ggalign_free_lab", class(p))
    attr(p, "ggalign_free_labs") <- "l"

    fa <- free_align(p, axes = "l")

    # free_lab should be removed when all overlapping axes are freed
    expect_false(inherits(fa, "ggalign_free_lab"))
    expect_null(attr(fa, "ggalign_free_labs"))
})

test_that("free_align removes overlapping axes free_space", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    attr(p, "ggalign_free_spaces") <- "tlbr"
    class(p) <- c("ggalign_free_space", class(p))

    p_fa <- free_align(p, axes = "lr")

    expect_equal(attr(p_fa, "ggalign_free_spaces"), "tb")
    expect_true(inherits(p_fa, "ggalign_free_space"))
})

test_that("free_align removes free_space class if no free spaces remain", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    attr(p, "ggalign_free_spaces") <- "lr"
    class(p) <- c("ggalign_free_space", class(p))

    p_fa <- free_align(p, axes = "tlbr")

    expect_null(attr(p_fa, "ggalign_free_spaces"))
    expect_false(inherits(p_fa, "ggalign_free_space"))
})

test_that("free_align removes overlapping axes free_border", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    attr(p, "ggalign_free_borders") <- "tlbr"
    class(p) <- c("ggalign_free_border", class(p))

    p_fa <- free_align(p, axes = "tb")

    expect_equal(attr(p_fa, "ggalign_free_borders"), "lr")
    expect_true(inherits(p_fa, "ggalign_free_border"))
})

test_that("free_align removes free_border class if no free borders remain", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    attr(p, "ggalign_free_borders") <- "tb"
    class(p) <- c("ggalign_free_border", class(p))

    p_fa <- free_align(p, axes = "tlbr")

    expect_null(attr(p_fa, "ggalign_free_borders"))
    expect_false(inherits(p_fa, "ggalign_free_border"))
})
