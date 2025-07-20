test_that("basic scheme subclass implements all required methods", {
    scheme_sub <- S7::new_class("foo", parent = scheme)

    S7::method(update_scheme, list(scheme_sub, scheme_sub)) <- function(e1, e2, ...) {
        structure(list(updated = TRUE), class = c("foo", "scheme"))
    }

    S7::method(inherit_scheme, list(scheme_sub, scheme_sub)) <- function(e1, e2, ...) {
        structure(list(inherited = TRUE), class = c("foo", "scheme"))
    }

    S7::method(update_ggplot, list(scheme_sub, ggplot2::class_ggplot)) <- function(object, plot, ...) {
        plot + ggplot2::ggtitle("Updated by scheme")
    }

    x <- structure(list(), class = c("foo", "scheme"))
    y <- structure(list(), class = c("foo", "scheme"))

    result <- update_scheme(x, y)
    expect_true(inherits(result, "scheme"))
    expect_true(result$updated)

    result2 <- inherit_scheme(x, y)
    expect_true(inherits(result2, "scheme"))
    expect_true(result2$inherited)

    plt <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
        ggplot2::geom_point()
    updated <- update_ggplot(x, plt)
    expect_s3_class(updated, "gg")
})

test_that("inherit_scheme fallback errors with informative message", {
    dummy <- structure(list(), class = c("bar", "scheme"))
    expect_snapshot_error(inherit_scheme(dummy, dummy))
})

test_that("update_scheme errors when method returns invalid object", {
    scheme_sub <- S7::new_class("fail_scheme", parent = scheme)

    S7::method(update_scheme, list(scheme_sub, scheme_sub)) <- function(e1, e2, ...) {
        structure(list(), class = "wrong_class")
    }

    s1 <- structure(list(), class = c("fail_scheme", "scheme"))
    s2 <- structure(list(), class = c("fail_scheme", "scheme"))

    schs <- structure(list(entries = list(fail_scheme = s1)), class = "schemes")
    attr(schs, "entries") <- schs$entries

    expect_snapshot_error(update_scheme(schs, s2))
})
