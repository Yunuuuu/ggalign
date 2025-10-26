test_that("`as_grob()` returns expected grob types", {
    library(grid)

    # grob input returns itself
    g <- rectGrob()
    expect_identical(as_grob(g), g)

    # gList input is wrapped in gTree
    gl <- gList(rectGrob())
    result <- as_grob(gl)
    expect_s3_class(result, "gTree")

    # ggplot input returns ggplotGrob
    p <- ggplot(mtcars, aes(mpg, disp)) +
        geom_point()
    expect_s3_class(as_grob(p), "gtable")

    # patchwork input (if installed)
    skip_if_not_installed("patchwork")
    pw <- patchwork::wrap_plots(p, p)
    expect_s3_class(as_grob(pw), "gtable")

    # default method throws error
    expect_snapshot_error(as_grob(list()))
})

test_that("`as_grob.formula()` produces valid grob", {
    skip_if_not_installed("gridGraphics")
    f <- ~ plot(mtcars$mpg, mtcars$disp)
    expect_s3_class(as_grob(f), "grob")
})

test_that("`as_grob.function()` produces valid grob", {
    skip_if_not_installed("gridGraphics")
    fun <- function() plot(mtcars$mpg, mtcars$disp)
    expect_s3_class(as_grob(fun), "grob")
})

test_that("`as_grob.recordedplot()` works with base plot", {
    skip_if_not_installed("gridGraphics")
    plot(mtcars$mpg, mtcars$disp)
    rec <- recordPlot()
    expect_s3_class(as_grob(rec), "grob")
})

test_that("`as_grob.trellis()` works for lattice plots", {
    skip_if_not_installed("lattice")
    p <- getExportedValue("lattice", "xyplot")(disp ~ mpg, data = mtcars)
    expect_s3_class(as_grob(p), "grob")
})

test_that("`as_grob.Heatmap()` works with ComplexHeatmap", {
    skip_if_not_installed("ComplexHeatmap")
    m <- matrix(rnorm(100), 10)
    ht <- getExportedValue("ComplexHeatmap", "Heatmap")(m)
    expect_s3_class(as_grob(ht), "grob")
})

test_that("`as_grob.pheatmap()` works with pheatmap", {
    skip_if_not_installed("pheatmap")
    ht <- getExportedValue("pheatmap", "pheatmap")(matrix(rnorm(100), 10))
    expect_s3_class(as_grob(ht), "gtable")
})
