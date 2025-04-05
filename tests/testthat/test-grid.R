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
