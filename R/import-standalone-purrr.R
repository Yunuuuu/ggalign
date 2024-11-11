# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-purrr.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-purrr.R
# last-updated: 2024-11-12
# license: https://unlicense.org
# ---

# This file provides a minimal shim to provide a purrr-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# Note: these functions won't support lambda syntax.

# ## Changelog
# 2024-11-12:
# First release
#
# nocov start

map <- function(.x, .f, ...) lapply(.x, .f, ...)

imap <- function(.x, .f, ...) {
    nms <- names(.x)
    if (is.null(nms)) {
        data <- list(.x, seq_along(.x))
    } else {
        data <- list(.x, nms)
    }
    out <- .mapply(.f, data, list(...))
    if (!is.null(nms)) names(out) <- nms
    out
}

walk <- function(.x, .f, ...) {
    lapply(.x, .f, ...)
    invisible(.x)
}

map_lgl <- function(.x, .f, ...) {
    vapply(X = .x, FUN = .f, logical(1L), ...)
}

map_int <- function(.x, .f, ...) {
    vapply(X = .x, FUN = .f, integer(1L), ...)
}

map_dbl <- function(.x, .f, ...) {
    vapply(X = .x, FUN = .f, double(1L), ...)
}

map_chr <- function(.x, .f, ...) {
    vapply(X = .x, FUN = .f, character(1L), ...)
}


map2 <- function(.x, .y, .f, ...) {
    mapply(.f, .x, .y, MoreArgs = list(...), SIMPLIFY = FALSE)
}

map2_lgl <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "logical")
}

map2_int <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "integer")
}

map2_dbl <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "double")
}

map2_chr <- function(.x, .y, .f, ...) {
    as.vector(map2(.x, .y, .f, ...), "character")
}

pmap <- function(.l, .f, ...) {
    lens <- lengths(.l)
    n <- max(lens)
    stopifnot(all(lens == 1L | lens == n))
    to_recycle <- lens != n
    .l[to_recycle] <- lapply(.l[to_recycle], function(x) rep.int(x, n))
    out <- .mapply(.f, .l, list(...))
    if (!is.null(nms <- names(.subset2(.l, 1L)))) names(out) <- nms
    out
}

transpose <- function(.l) {
    if (!length(.l)) return(.l) # styler: off
    inner_names <- names(.subset2(.l, 1L))
    if (is.null(inner_names)) {
        fields <- seq_along(.subset2(.l, 1L))
    } else {
        fields <- inner_names
        names(fields) <- fields
        .l <- lapply(.l, function(x) {
            if (is.null(names(x))) names(x) <- inner_names # styler: off
            x
        })
    }

    # This way missing fields are subsetted as `NULL` instead of causing
    # an error
    .l <- lapply(.l, as.list)

    lapply(fields, function(i) lapply(.l, .subset2, i))
}

# nocov end
