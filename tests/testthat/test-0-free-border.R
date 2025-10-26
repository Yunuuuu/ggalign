test_that("free_border() assigns class and border attribute", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_fb <- free_border(p)

    expect_s3_class(p_fb, "ggalign_free_border")
    expect_s3_class(p_fb, "ggplot")
    expect_identical(attr(p_fb, "ggalign_free_borders"), "tlbr")
})

test_that("free_border() respects custom borders argument", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_fb <- free_border(p, borders = "l")

    expect_identical(attr(p_fb, "ggalign_free_borders"), "l")
})

test_that("free_border() merges with existing free_border", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p1 <- free_border(p, borders = "t")
    p2 <- free_border(p1, borders = "r")

    expect_s3_class(p2, "ggalign_free_border")
    expect_true(all(c("t", "r") %in% strsplit(attr(p2, "ggalign_free_borders"), "")[[1]]))
})

test_that("free_border() removes overlapping from free_lab", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    class(p) <- c("ggalign_free_lab", class(p))
    attr(p, "ggalign_free_labs") <- "b"

    p_fb <- free_border(p, borders = "b")

    expect_false(inherits(p_fb, "ggalign_free_lab"))
    expect_null(attr(p_fb, "ggalign_free_labs"))
})

test_that("free_border() is no-op if all borders are freed in free_align", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_fa <- free_align(p, axes = "tlbr")
    p_fb <- free_border(p_fa, borders = "tlbr")

    # Since all borders are already "freed" through axes, this should return the original
    expect_identical(p_fb, p_fa)
})
