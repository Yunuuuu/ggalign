test_that("`layer_order()` works well", {
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

test_that("`geom_subtile()` works well", {
    expect_doppelganger(
        "geom_subtile_by_row",
        ggplot(data.frame(value = letters[seq_len(5)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value))
    )
    expect_doppelganger(
        "geom_subtile_by_column",
        ggplot(data.frame(value = letters[seq_len(9)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value))
    )
    expect_doppelganger(
        "geom_subtile_horizontal",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value), direction = "h")
    )
    expect_doppelganger(
        "geom_subtile_vertical",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value), direction = "v")
    )
})
