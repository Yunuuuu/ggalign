test_that("active class instantiation and defaults work", {
    obj <- active()
    expect_s7_class(obj, active)
    expect_true(is_active(obj))

    expect_true(is.na(obj@order))
    expect_true(is.na(obj@use))
    expect_true(is.na(obj@name))
})

test_that("active property assignment and validation", {
    obj <- active()

    obj@order <- 1L
    expect_equal(obj@order, 1L)

    obj@use <- TRUE
    expect_true(obj@use)

    obj@name <- "plot1"
    expect_equal(obj@name, "plot1")

    expect_error(obj@order <- c(1L, 2L), "must be a single integer value")
    expect_error(obj@use <- c(TRUE, FALSE), "must be a single boolean value")
    expect_error(obj@name <- c("a", "b"), "must be a single character string")
})

test_that("active + active merges non-NA properties", {
    a1 <- active(order = 1L, use = NA, name = "plot1")
    a2 <- active(order = NA_integer_, use = TRUE, name = NA_character_)

    result <- a1 + a2

    expect_equal(result@order, 1L)
    expect_equal(result@use, TRUE)
    expect_equal(result@name, "plot1")
})

test_that("active + NULL returns self", {
    a <- active(order = 2L)
    expect_identical(a + NULL, a)
    expect_identical(NULL + a, a)
})

test_that("active + incompatible throws error", {
    a <- active()
    expect_error(a + 1, "is not permitted")
    expect_error("text" + a, "is not permitted")
})
