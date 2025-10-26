test_that("area() creates valid ggalign_area objects", {
    a <- area(1, 1, 2, 2)
    expect_s3_class(a, "ggalign_area")
    expect_equal(length(field(a, "t")), 1)
    expect_equal(field(a, "t"), 1L)
    expect_equal(field(a, "l"), 1L)
    expect_equal(field(a, "b"), 2L)
    expect_equal(field(a, "r"), 2L)
})

test_that("area() with missing arguments returns empty area", {
    a <- area()
    expect_s3_class(a, "ggalign_area")
    expect_equal(length(field(a, "t")), 0)
})

test_that("area() throws error if t > b or l > r", {
    expect_snapshot_error(area(3, 1, 2, 2))
    expect_snapshot_error(area(1, 3, 2, 2))
})

test_that("as_areas.character converts layout string correctly", {
    layout_str <- "A##\nA#B\n##B"
    areas <- as_areas(layout_str)
    expect_s3_class(areas, "ggalign_area")
    expect_equal(length(areas), 2)
    # Check one of the areas matches expected coordinates
    expect_equal(field(areas, "t"), c(1L, 2L))
    expect_equal(field(areas, "l"), c(1L, 3L))
    expect_equal(field(areas, "b"), c(2L, 3L))
    expect_equal(field(areas, "r"), c(1L, 3L))
})

test_that("print methods work for zero-length and normal areas", {
    a_empty <- area()
    expect_output(print(a_empty), "Spanning 0 columns and 0 rows")

    a <- area(1, 1, 2, 2)
    expect_output(print(a), "Spanning 2 columns and 2 rows")
})

test_that("trim_area adjusts coordinates correctly", {
    a <- area(2, 3, 4, 5)
    trimmed <- trim_area(a)
    expect_equal(field(trimmed, "t"), field(a, "t") - 2 + 1)
    expect_equal(field(trimmed, "l"), field(a, "l") - 3 + 1)
})

test_that("plot.ggalign_area returns a ggplot object", {
    a <- area(1, 1, 2, 2)
    p <- plot(a)
    expect_s3_class(p, "ggplot")
})
