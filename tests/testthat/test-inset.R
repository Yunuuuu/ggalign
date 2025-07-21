test_that("inset() creates a patch_inset object with default parameters", {
    p1 <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(factor(gear), disp))

    pi <- inset(p2)

    expect_s3_class(pi, "patch_inset")
    expect_true(is.grob(pi$grob))
    expect_equal(pi$align, "panel")
    expect_equal(pi$clip, "on")
    expect_true(pi$on_top)
})

test_that("inset() respects vp argument and sets viewport on grob", {
    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(factor(gear), disp))
    vp <- viewport(x = 0.7, y = 0.7, width = 0.3, height = 0.3)

    pi <- inset(p2, vp = vp)

    expect_s3_class(pi, "patch_inset")
    expect_identical(pi$grob$vp, vp)
})

test_that("grid.draw.patch_inset calls next method on grob", {
    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(factor(gear), disp))
    pi <- inset(p2, vp = viewport(width = 0.5, height = 0.5))
    expect_doppelganger(
        "patch_inset grid.draw matches inner grob",
        grid.draw(pi)
    )
})

test_that("ggplot_add.patch_inset calls make_wrap", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p2 <- ggplot(mtcars) +
        geom_boxplot(aes(factor(gear), disp))
    pi <- inset(p2, vp = viewport(width = 0.5, height = 0.5))
    expect_doppelganger(
        "ggplot_add.patch_inset produces correct plot output",
        ggplot_add(pi, p)
    )
})
