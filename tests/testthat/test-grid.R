test_that("GridUnit works well", {
    x <- GridUnit(c(1, 2), c("null", "mm"))
    expect_equal(unit(c(1, 2), c("null", "mm")), prop(x, "inner"))
    expect_equal(convert(x, S3_unit), prop(x, "inner"))
    expect_equal(as.double(x), as.double(prop(x, "inner")))
    expect_snapshot_error(str(unit(NA, "mm")))
    expect_snapshot_output(str(GridUnit(NA, "mm")))
})

test_that("`magickGrob()` returns a magickGrob object", {
    grob <- grid::rectGrob()
    g <- magickGrob(grob)
    expect_s3_class(g, "magickGrob")
    expect_s3_class(g$grob, "rect")
})

test_that("`magickGrob()` validates arguments correctly", {
    grob <- grid::rectGrob()

    expect_snapshot_error(magickGrob(grob, magick = 123))

    expect_snapshot_error(magickGrob(grob, res = -1))

    expect_snapshot_error(magickGrob(grob, interpolate = "yes"))
})

test_that("`magickGrob.grob()` produces a valid magickGrob", {
    grob <- circleGrob()
    m <- magickGrob(grob)
    expect_s3_class(m, "magickGrob")
    expect_equal(m$grob, grob)
})

test_that("`magickGrob.gList()` wraps into gTree", {
    gl <- gList(grid::rectGrob(), grid::textGrob("hello"))
    m <- magickGrob(gl)
    expect_s3_class(m, "magickGrob")
    expect_s3_class(m$grob, "gTree")
})

test_that("`magickGrob.magickGrob()` edits fields", {
    grob <- magickGrob(grid::rectGrob(), interpolate = FALSE, res = 50)
    edited <- magickGrob(grob, interpolate = TRUE)
    expect_true(edited$interpolate)
    expect_equal(edited$res, 50) # unchanged
})

test_that("`magickGrob.default()` errors", {
    expect_snapshot_error(magickGrob(42))
})

test_that("`makeContent.magickGrob()` creates rasterGrob", {
    skip_if_not_installed("magick")

    grob <- magickGrob(grid::circleGrob(), res = 10)

    out <- makeContent(grob)
    expect_s3_class(out, "magickGrob")
    expect_true(inherits(out$children[[1]], "rastergrob"))
})

test_that("magick transformation function is applied", {
    skip_if_not_installed("magick")

    grob <- magickGrob(
        grid::circleGrob(),
        magick = ~ magick::image_flip(.),
        res = 10
    )

    out <- makeContent(grob)
    expect_s3_class(out, "magickGrob")
    expect_true(inherits(out$children[[1]], "rastergrob"))
})
