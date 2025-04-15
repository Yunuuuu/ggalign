test_that("`layer_order()` works well", {
    expect_snapshot_error(layer_order(scale_color_brewer()))
    expect_snapshot_error(layer_order(geom_blank(), order = "a"))
    expect_snapshot_error(layer_order(geom_blank(), order = NA))
    expect_snapshot_error(layer_order(geom_blank(), order = 1.5))
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

test_that("`geom_subrect()` works well", {
    expect_doppelganger(
        "geom_subrect_by_row",
        ggplot(data.frame(value = letters[seq_len(5)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value)
            )
    )
    expect_doppelganger(
        "geom_subrect_by_column",
        ggplot(data.frame(value = letters[seq_len(9)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value)
            )
    )
    expect_doppelganger(
        "geom_subrect_horizontal",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value),
                direction = "h"
            )
    )
    expect_doppelganger(
        "geom_subrect_vertical",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value),
                direction = "v"
            )
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

test_that("geom_pie works well", {
    expect_doppelganger(
        "geom_pie_clockwise",
        ggplot(data.frame(x = 1:10, y = 1:10, value = 1:10 / sum(1:10))) +
            geom_pie(aes(x, y, angle = value * 360))
    )
    expect_doppelganger(
        "geom_pie_no_clockwise",
        ggplot(data.frame(x = 1:10, y = 1:10, value = 1:10 / sum(1:10))) +
            geom_pie(aes(x, y, angle = value * 360), clockwise = FALSE)
    )
    expect_doppelganger(
        "geom_pie_angle0",
        ggplot(data.frame(x = 1:10, y = 1:10, value = 1:10 / sum(1:10))) +
            geom_pie(aes(x, y, angle = value * 360), angle0 = 30)
    )
})

test_that("`geom_gshape()` works well", {
    library(grid)
    expect_snapshot_error(ggplot2::ggsave(
        tempfile(fileext = ".png"),
        plot = ggplot(data.frame(value = letters[seq_len(5)], y = seq_len(5))) +
            geom_gshape(aes(x = 1, y = y, gshape = value, fill = value))
    ))
    gshape_mapping <- list(
        function(x, y, width, height, fill) {
            rectGrob(x, y,
                width = width, height = height,
                gp = gpar(fill = fill),
                default.units = "native"
            )
        },
        function(x, y, width, height, fill) {
            rectGrob(x, y,
                width = width, height = height,
                gp = gpar(fill = fill),
                default.units = "native"
            )
        },
        function(x, y, width, height, fill) {
            rectGrob(x, y,
                width = width, height = height,
                gp = gpar(fill = fill),
                default.units = "native"
            )
        },
        function(x, y, width, height, shape) {
            gList(
                pointsGrob(x, y, pch = shape),
                # To ensure the rectangle color is shown in the legends, you
                # must explicitly provide a color argument and include it in
                # the `gpar()` of the graphical object
                rectGrob(x, y, width, height,
                    gp = gpar(col = "black", fill = NA)
                )
            )
        },
        function(xmin, xmax, ymin, ymax) {
            segmentsGrob(
                xmin, ymin,
                xmax, ymax,
                gp = gpar(lwd = 2)
            )
        }
    )
    expect_doppelganger(
        "geom_gshape",
        ggplot(data.frame(value = letters[seq_len(5)], y = seq_len(5))) +
            geom_gshape(aes(x = 1, y = y, gshape = value, fill = value)) +
            scale_gshape_manual(values = gshape_mapping) +
            scale_fill_brewer(palette = "Dark2")
    )
    set.seed(1L)
    value <- sample(letters, 5L)
    expect_doppelganger(
        "geom_gshape_order",
        ggplot(data.frame(
            value = c(value, value[5L]),
            y = c(1, 2, 3, 1, 2, 3)
        )) +
            geom_gshape(aes(x = 1, y = y, gshape = value, fill = value)) +
            scale_gshape_manual(values = gshape_mapping) +
            scale_fill_brewer(palette = "Dark2")
    )
})

test_that("`coord_circle()` works well", {
    expect_doppelganger(
        "coord_circle() default",
        ggplot(mtcars, aes(disp, mpg)) +
            geom_point() +
            coord_circle()
    )
    expect_doppelganger(
        "coord_circle() start and end",
        ggplot(mtcars, aes(disp, mpg)) +
            geom_point() +
            coord_circle(start = -0.4 * pi, end = 0.4 * pi)
    )
    expect_doppelganger(
        "coord_circle() inner.radius and outer.radius",
        ggplot(mtcars, aes(disp, mpg)) +
            geom_point() +
            coord_circle(inner.radius = 0.3, outer.radius = 0.5)
    )
})

test_that("`facet_sector()` works well", {
    expect_doppelganger(
        "facet_sector() default",
        ggplot(mtcars, aes(disp, mpg)) +
            geom_point() +
            facet_sector(vars(cyl))
    )
    expect_doppelganger(
        "facet_sector() sector_spacing rel()",
        ggplot(mtcars, aes(disp, mpg)) +
            geom_point() +
            facet_sector(vars(cyl), sector_spacing = rel(0.01))
    )
})
