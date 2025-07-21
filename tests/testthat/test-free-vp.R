test_that("free_vp() assigns class and viewport attribute", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    p_vp <- free_vp(p, x = 0.3, y = 0.7, width = 0.5, height = 0.4)

    expect_s3_class(p_vp, "free_vp")
    expect_s3_class(p_vp, "ggplot")
    vp <- attr(p_vp, "vp")
    expect_true(inherits(vp, "viewport"))
    expect_equal(as.numeric(vp$x), 0.3)
    expect_equal(as.numeric(vp$y), 0.7)
    expect_equal(as.numeric(vp$width), 0.5)
    expect_equal(as.numeric(vp$height), 0.4)
})

test_that("free_space() works with alignpatch objects", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    class(p) <- c("alignpatches", class(p))
    p_vp <- free_vp(p, x = 0.1, y = 0.9)

    expect_s3_class(p_vp, "free_vp")
    expect_equal(as.numeric(attr(p_vp, "vp")$x), 0.1)
    expect_equal(as.numeric(attr(p_vp, "vp")$y), 0.9)
})

test_that("free_vp() uses defaults when width/height missing", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    p_vp <- free_vp(p)

    vp <- attr(p_vp, "vp")
    expect_true(inherits(vp, "viewport"))
    expect_equal(as.numeric(vp$x), 0.5)
    expect_equal(as.numeric(vp$y), 0.5)
    expect_true(is.na(as.numeric(vp$width)))
    expect_true(is.na(as.numeric(vp$height)))
})

test_that("free_vp.default errors for invalid input", {
    expect_snapshot_error(free_vp(123))
})

test_that("alignpatch.free_vp ggproto object overrides align_border", {
    p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    p <- free_vp(p)
    obj <- alignpatch(p)

    expect_s3_class(obj, "PatchFreeViewport")
    expect_true(is.function(obj$align_border))
    expect_identical(attr(p, "vp"), obj$vp)
})
