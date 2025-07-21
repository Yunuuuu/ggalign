test_that("patch returns expected grob types", {
    library(grid)

    # grob input returns itself
    g <- rectGrob()
    expect_identical(patch(g), g)

    # gList input is wrapped in gTree
    gl <- gList(rectGrob())
    result <- patch(gl)
    expect_s3_class(result, "gTree")

    # ggplot input returns ggplotGrob
    p <- ggplot(mtcars, aes(mpg, disp)) +
        geom_point()
    expect_s3_class(patch(p), "gtable")

    # patchwork input (if installed)
    skip_if_not_installed("patchwork")
    pw <- patchwork::wrap_plots(p, p)
    expect_s3_class(patch(pw), "gtable")

    # default method throws error
    expect_snapshot_error(patch(list()))
})

test_that("patch.formula produces valid grob", {
    skip_if_not_installed("gridGraphics")
    f <- ~ plot(mtcars$mpg, mtcars$disp)
    expect_s3_class(patch(f), "grob")
})

test_that("patch.function produces valid grob", {
    skip_if_not_installed("gridGraphics")
    fun <- function() plot(mtcars$mpg, mtcars$disp)
    expect_s3_class(patch(fun), "grob")
})

test_that("patch.recordedplot works with base plot", {
    skip_if_not_installed("gridGraphics")
    plot(mtcars$mpg, mtcars$disp)
    rec <- recordPlot()
    expect_s3_class(patch(rec), "grob")
})

test_that("patch.trellis works for lattice plots", {
    skip_if_not_installed("lattice")
    p <- getFromNamespace("xyplot", "lattice")(disp ~ mpg, data = mtcars)
    expect_s3_class(patch(p), "grob")
})

test_that("patch.Heatmap works with ComplexHeatmap", {
    skip_if_not_installed("ComplexHeatmap")
    m <- matrix(rnorm(100), 10)
    ht <- getFromNamespace("Heatmap", "ComplexHeatmap")(m)
    expect_s3_class(patch(ht), "grob")
})

test_that("patch.pheatmap works with pheatmap", {
    skip_if_not_installed("pheatmap")
    ht <- getFromNamespace("pheatmap", "pheatmap")(matrix(rnorm(100), 10))
    expect_s3_class(patch(ht), "gtable")
})
