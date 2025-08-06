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
        "geom_subrect() `byrow = TRUE`",
        ggplot(data.frame(value = letters[seq_len(5)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value),
                byrow = TRUE
            )
    )
    expect_doppelganger(
        "geom_subrect() `byrow = FALSE`",
        ggplot(data.frame(value = letters[seq_len(9)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value)
            )
    )
    expect_doppelganger(
        "geom_subrect() set nrow",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value),
                nrow = 1
            )
    )
    expect_doppelganger(
        "geom_subrect() set ncol",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subrect(
                aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value),
                ncol = 1
            )
    )
    expect_doppelganger(
        "geom_subrect() set both nrow and ncol",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subrect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = value), ncol = 1)
    )
})

test_that("`geom_subtile()` works well", {
    expect_doppelganger(
        "geom_subtile() `byrow = TRUE`",
        ggplot(data.frame(value = letters[seq_len(5)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value), byrow = TRUE)
    )
    expect_doppelganger(
        "geom_subtile() `byrow = FALSE`",
        ggplot(data.frame(value = letters[seq_len(9)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value))
    )
    expect_doppelganger(
        "geom_subtile() set nrow",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value), nrow = 1)
    )
    expect_doppelganger(
        "geom_subtile() set ncol",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value), ncol = 1)
    )
    expect_doppelganger(
        "geom_subtile() set both nrow and ncol",
        ggplot(data.frame(value = letters[seq_len(4)])) +
            geom_subtile(aes(x = 1, y = 1, fill = value), ncol = 1)
    )
})

test_that("`geom_pie()` works well", {
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

test_that("`geom_tile3d()` works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    expect_doppelganger(
        "geom_tile3d",
        ggplot2::ggplot(fortify_data_frame(small_mat)) +
            geom_tile3d(aes(
                .column_index, .row_index,
                fill = value, z = value,
                width = 0.8, height = 0.8
            )) +
            scale_z_continuous()
    )
})

test_that("`geom_rect3d()` works well", {
    set.seed(123)
    small_mat <- matrix(rnorm(72), nrow = 9)
    rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
    colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))

    expect_doppelganger(
        "geom_rect3d",
        ggplot2::ggplot(fortify_data_frame(small_mat)) +
            geom_rect3d(aes(
                .column_index, .row_index,
                fill = value, z = value,
                width = 0.8, height = 0.8
            )) +
            scale_z_continuous()
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

test_that("`no_expansion()` works well", {
    expect_doppelganger(
        "no_expansion()",
        ggplot(mtcars, aes(disp, mpg)) +
            geom_point() +
            no_expansion()
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
