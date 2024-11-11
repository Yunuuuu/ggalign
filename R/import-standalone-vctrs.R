# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-vctrs.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-vctrs.R
# last-updated: 2024-11-10
# license: https://unlicense.org
# imports: [vctrs (>= 0.5.0), rlang]
# ---

# when developing R package, instead of depending on `dplyr`
# we prefer use the basic function from `vctrs`
#
# Note: these functions won't check arguments
#
# Please initialize the package docs and import vctrs
# 1. run `usethis::use_package_doc()`
# 2. in package docs, please add #' @import vctrs

# ## Changelog
# 2024-11-11:
# - Added `inner_join`
# - Added `left_join`
# - Added `right_join`
# - Added `cross_join`
# - Added `replace_na`
# - Added `coalesce`
# - Added `na_if`
#
# 2024-11-10:
# - Added `full_join`
# - Added `if_else`
#
# nocov start

#' @importFrom rlang set_names
full_join <- function(x, y, by = vec_set_intersect(names(x), names(y)),
                      by.x = by, by.y = by, suffix = c(".x", ".y")) {
    loc <- vec_locate_matches(x[by.x], set_names(y[by.y], by.x), remaining = NA)
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack")
    ans <- join_bind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer),
        suffix = suffix
    )
    new_rows <- which(vec_detect_missing(x_slicer)) # should come from `y`
    if (length(new_rows)) {
        ans[new_rows, by.x] <- vec_slice(y[by.y], y_slicer[new_rows])
    }
    ans
}

#' @importFrom rlang set_names
inner_join <- function(x, y, by = vec_set_intersect(names(x), names(y)),
                       by.x = by, by.y = by, suffix = c(".x", ".y")) {
    loc <- vec_locate_matches(
        x[by.x], set_names(y[by.y], by.x),
        no_match = "drop"
    )
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack")
    join_bind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer),
        suffix = suffix
    )
}

#' @importFrom rlang set_names
left_join <- function(x, y, by = vec_set_intersect(names(x), names(y)),
                      by.x = by, by.y = by, suffix = c(".x", ".y")) {
    loc <- vec_locate_matches(x[by.x], set_names(y[by.y], by.x))
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack") # can have NA value
    join_bind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer),
        suffix = suffix
    )
}

#' @importFrom rlang set_names
right_join <- function(x, y, by = vec_set_intersect(names(x), names(y)),
                       by.x = by, by.y = by, suffix = c(".x", ".y")) {
    loc <- vec_locate_matches(x[by.x], set_names(y[by.y], by.x),
        no_match = "drop", remaining = NA
    )
    x_slicer <- .subset2(loc, "needles") # can have NA value
    y_slicer <- .subset2(loc, "haystack")
    join_bind(
        # drop duplicated join column
        vec_slice(x[vec_set_difference(names(x), by.x)], x_slicer),
        vec_slice(y, y_slicer),
        suffix = suffix
    )
}

cross_join <- function(x, y, suffix = c(".x", ".y")) {
    x_size <- vec_size(x)
    y_size <- vec_size(y)
    x_out <- vec_rep_each(x, times = y_size)
    y_out <- vec_rep(y, times = x_size)
    join_bind(x_out, y_out, suffix)
}

join_bind <- function(x, y, suffix) {
    x_names <- names(x)
    y_names <- names(y)
    common <- vec_set_intersect(x_names, y_names)
    if (length(common)) { # add suffix to duplicated names
        index <- vec_match(common, x_names)
        names(x)[index] <- paste0(x_names[index], .subset(suffix, 1L))
        index <- vec_match(common, y_names)
        names(y)[index] <- paste0(y_names[index], .subset(suffix, 2L))
    }
    vec_cbind(x, y, .name_repair = "check_unique")
}

if_else <- function(condition, true, false, na = NULL) {
    # output size from `condition`
    size <- vec_size(condition)

    # output type from `true`/`false`/`na`
    ptype <- vec_ptype_common(true = true, false = false, na = na)

    args <- vec_recycle_common(
        true = true, false = false, na = na, .size = size
    )
    args <- vec_cast_common(!!!args, .to = ptype)

    out <- vec_init(ptype, size)

    loc_true <- condition
    loc_false <- !condition

    out <- vec_assign(out, loc_true, vec_slice(args$true, loc_true))
    out <- vec_assign(out, loc_false, vec_slice(args$false, loc_false))

    if (!is.null(na)) {
        loc_na <- vec_detect_missing(condition)
        out <- vec_assign(out, loc_na, vec_slice(args$na, loc_na))
    }

    out
}

#' Replace NAs with specified values
#' @noRd
replace_na <- function(x, y) {
    vec_assign(x = x, i = vec_detect_missing(x), value = y)
}

#' Find the first non-missing element
#' @param ... A list of atomic vector (You shouldn't input `NULL`).
#' @noRd
coalesce <- function(...) {
    dots <- vec_recycle_common(...)
    out <- .subset2(dots, 1L)
    for (i in 2:...length()) {
        out <- replace_na(out, .subset2(dots, i))
    }
    out
}

#' Convert values to `NA`
#' @noRd
na_if <- function(x, y) {
    y <- vec_cast(x = y, to = x, x_arg = "y", to_arg = "x")
    y <- vec_recycle(y, size = vec_size(x), x_arg = "y")
    na <- vec_init(x)
    vec_assign(x, vec_equal(x, y, na_equal = TRUE), na)
}

# nocov end
