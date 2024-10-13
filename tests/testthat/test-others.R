test_that("layer_order() works well", {
    expect_error(layer_order(scale_color_brewer()))
    expect_error(layer_order(geom_blank(), order = "a"))
    expect_error(layer_order(geom_blank(), order = NA))
    expect_error(layer_order(geom_blank(), order = 1.5))
    expect_doppelganger(
        "layer_order_add_in_the_beginning",
        ggplot(faithfuld, aes(waiting, eruptions)) +
            geom_raster(aes(fill = density)) +
            layer_order(geom_point(color = "red", size = 1))
    )
    expect_doppelganger(
        "layer_order_add_in_the_end",
        ggplot(faithfuld, aes(waiting, eruptions)) +
            geom_raster(aes(fill = density)) +
            layer_order(geom_point(color = "red", size = 1), order = Inf)
    )
})
