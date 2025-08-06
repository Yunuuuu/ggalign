test_that("range_link creates valid range object", {
    rl <- range_link(1, 3)
    expect_s3_class(rl, "ggalign_range_link")
    expect_equal(rl$point1, 1L)
    expect_equal(rl$point2, 3L)

    rl2 <- range_link("A", "Z")
    expect_equal(rl2$point1, "A")
    expect_equal(rl2$point2, "Z")
})

test_that("range_link throws error on invalid inputs", {
    expect_error(range_link(1:2, 3), "point1")
    expect_error(range_link(1, list()), "point2")
    expect_error(range_link(NULL, 3), "point1")
})

test_that("pair_links can create multiple valid link pairs", {
    x <- pair_links(
        1:2,
        I(1:2),
        ~ 3:4,
        range_link(1, 5),
        range_link("a", "c"),
        range_link(1, 5) ~ waiver(),
        waiver() ~ 1:2,
        ~NULL
    )
    expect_s3_class(x, "ggalign_pair_links")
    expect_length(x, 8)
    expect_s3_class(x[[1]], "ggalign_pair_link")
    expect_snapshot_output(print(x))
})

test_that("pair_links respects modification methods", {
    x <- pair_links(1:2, 3:4)
    x[[1]] <- NULL
    expect_length(x, 1)

    x$a <- ~ LETTERS[1:3]
    expect_named(x, c("", "a"))

    x[1:2] <- list(~ 1:2, ~ 3:4)
    expect_equal(length(x), 2)
})

test_that("deparse_link works for various types", {
    expect_match(deparse_link(1:3), "1:3")
    expect_match(deparse_link(c("a", "b", "c")), "c\\(\"a\", \"b\", \"c\"\\)")
    expect_match(deparse_link(waiver()), "waiver\\(\\)")
    expect_equal(deparse_link(NULL), "")
    expect_match(deparse_link(range_link(1, 3)), "range_link\\(1, 3\\)")
})
