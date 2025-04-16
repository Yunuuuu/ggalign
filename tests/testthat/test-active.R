test_that("`new_active()` creates an object with correct structure and class", {
    x <- new_active(use = TRUE, order = 1, name = "main")
    expect_s3_class(x, "ggalign_active")
    expect_named(x, c("order", "use", "name"))
    expect_equal(x$order, 1)
    expect_true(x$use)
    expect_equal(x$name, "main")
})

test_that("`active()` works well", {
    expect_identical(active(NULL)$order, NA_integer_)
    expect_snapshot_error(active(1:2))
    expect_snapshot_error(active("a"))
    expect_snapshot_error(active(1.2))
    expect_identical(active(1)$order, 1L)

    expect_snapshot_error(active(use = NA)$use)
    expect_identical(active(use = TRUE)$use, TRUE)
    expect_identical(active(use = FALSE)$use, FALSE)

    expect_identical(active(name = NA)$name, NA)
    expect_identical(active(name = "my_name")$name, "my_name")
    expect_snapshot_error(active(name = FALSE))
})

test_that("`update_active()` works well", {
    default <- new_active(order = 1, use = TRUE, name = "main")
    expect_equal(update_active(NULL, default), default)
})
