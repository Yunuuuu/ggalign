test_that("free_guide() assigns class and guide attribute", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_fg <- free_guide(p)

    expect_s3_class(p_fg, "ggalign_free_guide")
    expect_s3_class(p_fg, "ggplot")
    expect_identical(attr(p_fg, "ggalign_free_guides"), "tlbr")
})

test_that("free_guide() allows setting custom guide sides", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_fg <- free_guide(p, guides = "l")

    expect_identical(attr(p_fg, "ggalign_free_guides"), "l")
})

test_that("free_guide() allows setting to NULL", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p_fg <- free_guide(p, guides = NULL)

    expect_s3_class(p_fg, "ggalign_free_guide")
    expect_null(attr(p_fg, "ggalign_free_guides"))
})

test_that("free_guide() merges guide sides if already free_guide", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p1 <- free_guide(p, guides = "l")
    p2 <- free_guide(p1, guides = "b")

    expect_s3_class(p2, "ggalign_free_guide")
    guides <- strsplit(attr(p2, "ggalign_free_guides"), "")[[1]]
    expect_true(all(c("l", "b") %in% guides))
})

test_that("free_guide() clears guides if NULL passed again", {
    p <- ggplot(mtcars) +
        geom_point(aes(mpg, disp))
    p1 <- free_guide(p, guides = "tb")
    p2 <- free_guide(p1, guides = NULL)

    expect_s3_class(p2, "ggalign_free_guide")
    expect_null(attr(p2, "ggalign_free_guides"))
})

test_that("free_guide() errors with invalid input", {
    expect_snapshot_error(free_guide(ggplot(), guides = "x"))
})
