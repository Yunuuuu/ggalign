# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-purrr.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-purrr.R
# last-updated: 2024-11-13
# license: https://unlicense.org
# ---

# This file provides a minimal shim to provide a purrr-like API on top of
# base R functions. They are not drop-in replacements but allow a similar style
# of programming.
#
# Note: these functions won't support lambda syntax.

# ## Changelog
# 2024-11-13:
# rename `transpose()` to `list_transpose()`
#
# 2024-11-12:
# First release
#
# nocov start

map <- function(.x, .f, ...) lapply(.x, .f, ...)

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

pmap <- function(.l, .f, ...) {
    out <- .mapply(.f, .l, list(...))
    if (!is.null(nms <- names(.subset2(.l, 1L)))) names(out) <- nms
    out
}

pmap_lgl <- function(.l, .f, ...) {
    .purrr_pmap_mold(.l, .f, ..., mold = logical(1L))
}

pmap_int <- function(.l, .f, ...) {
    .purrr_pmap_mold(.l, .f, ..., mold = integer(1L))
}

pmap_dbl <- function(.l, .f, ...) {
    .purrr_pmap_mold(.l, .f, ..., mold = double(1L))
}

pmap_chr <- function(.l, .f, ...) {
    .purrr_pmap_mold(.l, .f, ..., mold = character(1L))
}

.purrr_pmap_mold <- function(.l, .f, ..., mold) {
    nms <- names(.subset2(.l, 1L))
    dots <- list(...)
    out <- vapply(seq_along(nms), function(i) {
        do.call(.f, args = c(lapply(.l, .subset2, i), dots))
    }, mold, USE.NAMES = FALSE)
    if (!is.null(nms)) names(out) <- nms
    out
}

map2 <- function(.x, .y, .f, ...) pmap(list(.x, .y), .f, ...)

map2_lgl <- function(.x, .y, .f, ...) {
    .purrr_pmap_mold(list(.x, .y), .f, ..., mold = logical(1L))
}

map2_int <- function(.x, .y, .f, ...) {
    .purrr_pmap_mold(list(.x, .y), .f, ..., mold = integer(1L))
}

map2_dbl <- function(.x, .y, .f, ...) {
    .purrr_pmap_mold(list(.x, .y), .f, ..., mold = double(1L))
}

map2_chr <- function(.x, .y, .f, ...) {
    .purrr_pmap_mold(list(.x, .y), .f, ..., mold = character(1L))
}

imap <- function(.x, .f, ...) {
    nms <- names(.x)
    if (is.null(nms)) {
        .l <- list(.x, seq_along(.x))
    } else {
        .l <- list(.x, nms)
    }
    pmap(.l, .f, ...)
}

.purrr_imap_mold <- function(.x, .f, ..., mold) {
    if (is.null(nms <- names(.x))) {
        .l <- list(.x, seq_along(.x))
    } else {
        .l <- list(.x, nms)
    }
    .purrr_pmap_mold(.l, .f, ..., mold = mold)
}

imap_lgl <- function(.l, .f, ...) {
    .purrr_imap_mold(.l, .f, ..., mold = logical(1L))
}

imap_int <- function(.l, .f, ...) {
    .purrr_imap_mold(.l, .f, ..., mold = integer(1L))
}

imap_dbl <- function(.l, .f, ...) {
    .purrr_imap_mold(.l, .f, ..., mold = double(1L))
}

imap_chr <- function(.l, .f, ...) {
    .purrr_imap_mold(.l, .f, ..., mold = character(1L))
}

list_transpose <- function(.l) {
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
