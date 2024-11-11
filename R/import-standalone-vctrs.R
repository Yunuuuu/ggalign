# Standalone file: do not edit by hand
# Source: <https://github.com/Yunuuuu/standalone/blob/main/R/standalone-vctrs.R>
# ----------------------------------------------------------------------
#
# ---
# repo: Yunuuuu/standalone
# file: standalone-vctrs.R
# last-updated: 2024-11-10
# license: https://unlicense.org
# imports: vctrs (>= 0.5.0)
# ---

# Please initialize the package docs and import vctrs
# 1. run `usethis::use_package_doc()`
# 2. in package docs, please add #' @import vctrs

# ## Changelog
# 2024-11-10:
# - Added `full_join`
# - Added `if_else`
#
# nocov start

full_join <- function(x, y, by = vec_set_intersect(names(x), names(y)),
                      by.x = by, by.y = by) {
    haystack <- y[by.y]
    names(haystack) <- by.x
    loc <- vec_locate_matches(x[by.x], haystack, remaining = NA)
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack")
    ans <- vec_cbind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[vec_set_difference(names(y), by.y)], y_slicer)
    )
    new_rows <- which(vec_detect_missing(x_slicer)) # should come from `y`
    if (length(new_rows)) {
        ans[new_rows, by.x] <- vec_slice(y[by.y], y_slicer[new_rows])
    }
    ans
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

# nocov end
