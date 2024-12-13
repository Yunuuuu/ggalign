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
