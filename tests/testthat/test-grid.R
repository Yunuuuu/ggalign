test_that("`channelGrob()` works well", {
    channel <- channelGrob(function(locations) {
        # you can also use `tag` to identify the locations
        loc1 <- .subset2(locations, 1L)
        loc2 <- .subset2(locations, 2L)
        grid::segmentsGrob(loc1$x, loc1$y, loc2$x, loc2$y)
    })

    gt <- gtable::gtable(unit(1:2, c("cm")), unit(5, "cm"))
    gt <- gtable::gtable_add_grob(
        gt,
        list(
            grid::rectGrob(gp = gpar(color = "black", fill = NA)),
            channel$signal(0.5, 0.5, "npc")
        ),
        t = 1, l = 1, name = c("rect1", "signal1")
    )
    gt <- gtable::gtable_add_grob(
        gt,
        list(
            grid::rectGrob(gp = gpar(color = "red", fill = NA)),
            channel$signal(0.5, 0.5, "npc")
        ),
        t = 1, l = 2, name = c("rect2", "signal2")
    )
    expect_doppelganger("channelGrob", gt)
})

test_that("`str()` method for NA unit works well", {
    expect_snapshot_error(str(unit(NA, "mm")))
    expect_snapshot_output(str(GridUnit(unit(NA, "mm"))))
})


test_that("magickGrob() returns a magickGrob object", {
    grob <- grid::rectGrob()
    g <- magickGrob(grob)
    expect_s3_class(g, "magickGrob")
    expect_s3_class(g$grob, "rect")
})

test_that("magickGrob() validates arguments correctly", {
    grob <- grid::rectGrob()

    expect_snapshot_error(magickGrob(grob, magick = 123))

    expect_snapshot_error(magickGrob(grob, res = -1))

    expect_snapshot_error(magickGrob(grob, interpolate = "yes"))
})

test_that("magickGrob0.grob() produces a valid magickGrob", {
    grob <- circleGrob()
    m <- magickGrob0(grob)
    expect_s3_class(m, "magickGrob")
    expect_equal(m$grob, grob)
})

test_that("magickGrob0.gList() wraps into gTree", {
    gl <- gList(grid::rectGrob(), grid::textGrob("hello"))
    m <- magickGrob0(gl)
    expect_s3_class(m, "magickGrob")
    expect_s3_class(m$grob, "gTree")
})

test_that("magickGrob0.magickGrob() edits fields", {
    grob <- magickGrob(grid::rectGrob(), interpolate = FALSE, res = 50)
    edited <- magickGrob0(grob, interpolate = TRUE)
    expect_true(edited$interpolate)
    expect_equal(edited$res, 50) # unchanged
})

test_that("magickGrob0.default() errors", {
    expect_snapshot_error(magickGrob0(42))
})

test_that("makeContent.magickGrob() creates rasterGrob", {
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
