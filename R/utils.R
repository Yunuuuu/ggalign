`%||%` <- function(x, y) if (is.null(x)) y else x

#' Read Example Data
#'
#' This function reads example data from the file. If no file is specified, it
#' returns a list of available example files.
#'
#' @param file A string representing the name of the example file to be read. If
#' `NULL`, the function will return a list of available example file names.
#' @return If `file` is `NULL`, returns a character vector of available example
#' file names. Otherwise, returns the contents of the specified example file,
#' read as an R object.
#' @examples
#' read_example()
#' @export
read_example <- function(file = NULL) {
    if (is.null(file)) {
        dir(example_file())
    } else {
        readRDS(example_file(file, mustWork = TRUE))
    }
}

example_file <- function(..., base = "extdata") {
    system.file(base, ..., package = pkg_nm())
}

with_options <- function(code, ...) {
    opts <- options(...)
    on.exit(options(opts))
    force(code)
}

# This will work with most things but be aware that it might fail with some
# complex objects. For example, according to `?S3Methods`, calling foo on
# matrix(1:4, 2, 2) would try `foo.matrix`, then `foo.numeric`, then
# `foo.default`; whereas this code will just look for `foo.matrix` and
# `foo.default`.
#' @importFrom utils getS3method
#' @importFrom methods extends
has_method <- function(x, f, inherit = TRUE, default = TRUE) {
    x_class <- class(x)
    if (inherit) {
        if (isS4(x)) x_class <- extends(x_class)
        if (default) x_class <- c(x_class, "default")
    } else {
        x_class <- .subset(x_class, 1L)
    }
    for (cls in x_class) {
        if (!is.null(getS3method(f, cls, optional = TRUE))) {
            return(TRUE)
        }
    }
    return(FALSE)
}

#' For functions with a `call` argument, we check if the call originates from
#' the current package. If it does, we use the caller's call; if not, we use the
#' current call directly. Used by `align()` and `free()`
#' @noRd
#' @importFrom utils packageName
override_call <- function(call = NULL) {
    # if no caller call
    if (is.null(call) || is.function(f <- .subset2(call, 1L))) {
        return(TRUE)
    }
    # if call from the current package
    !identical(
        packageName(environment(eval(f))),
        pkg_nm()
    )
}

#' @importFrom utils packageName
pkg_nm <- function() packageName(topenv(environment()))

#' @importFrom vctrs vec_unrep vec_set_difference vec_c
make_order <- function(order) {
    l <- length(order)
    index <- seq_len(l)

    # for order not set by user, we use heuristic algorithm to define the order
    need_action <- is.na(order)
    if (all(need_action)) { # shorthand for the usual way, we don't set any
        return(index)
    } else if (all(!need_action)) { # we won't need do something special
        return(order(order))
    }

    # 1. for outliers, we always put them in the two tail
    # 2. for order has been set and is not the outliers,
    #    we always follow the order
    # 3. non-outliers were always regarded as the integer index
    used <- as.integer(order[!need_action & order >= 1L & order <= l])

    # we flatten user index to continuous integer sequence
    sequence <- vec_unrep(used) # key is the sequence start
    start <- .subset2(sequence, "key")
    end <- pmin(
        start + .subset2(sequence, "times") - 1L,
        vec_c(start[-1L] - 1L, l) # the next start - 1L
    )
    used <- .mapply(function(s, e) s:e, list(s = start, e = end), NULL)

    # following index can be used
    unused <- vec_set_difference(index, unlist(used, FALSE, FALSE))

    # we assign the candidate index to the order user not set.
    order[need_action] <- unused[seq_len(sum(need_action))]

    # make_order(c(NA, 1, NA)): c(2, 1, 3)
    # make_order(c(NA, 1, 3)): c(2, 1, 3)
    # make_order(c(NA, 1, 3, 1)): c(2, 4, 3, 1)
    order(order)
}

# library(data.table)
# library(vctrs)
# `%nest_unique%` <- function(x, y) {
#     ans <- new_data_frame(list(x = x, y = y))
#     ans <- unique(ans)
#     !anyDuplicated(ans$x)
# }
# `%nest_vctrs%` <- function(x, y) {
#     ans <- new_data_frame(list(x = x, y = y))
#     ans <- vec_unique(ans)
#     !vec_duplicate_any(.subset2(ans, "x"))
# }
# `%nest_vctrs_loc%` <- function(x, y) {
#     # we don't check the inputs for performance
#     loc <- vec_unique_loc(new_data_frame(list(x = x, y = y)))
#     !vec_duplicate_any(vec_slice(x, loc))
# }
# `%nest_data_table%` <- function(x, y) {
#     ans <- data.table(x = x, y = y)
#     ans <- unique(ans)
#     !anyDuplicated(.subset2(ans, "x"))
# }
# `%nest_split%` <- function(x, y) {
#     all(lengths(lapply(split(y, x), unique)) == 1L)
# }
# `%nest_table%` <- function(x, y) {
#     all(rowSums(table(x, y) > 0L) == 1L)
# }
# foo <- rep(seq(10^4L / 2L), each = 4)
# bar <- rep(seq(10^4L), each = 2)
# bench::mark(
#     nest_unique = bar %nest_unique% foo,
#     nest_vctrs = bar %nest_vctrs% foo,
#     nest_vctrs_loc = bar %nest_vctrs_loc% foo,
#     nest_data_table = bar %nest_data_table% foo,
#     nest_split = bar %nest_split% foo,
#     nest_table = bar %nest_table% foo,
# )
#> Warning: Some expressions had a GC in every iteration; so filtering is
#> disabled.
#> # A tibble: 6 × 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 nest_unique       5.37ms   7.49ms    134.      1.33MB    69.2
#> 2 nest_vctrs       200.3µs 214.57µs   3591.     754.7KB     6.00
#> 3 nest_vctrs_loc  193.99µs 207.29µs   4490.    706.95KB     6.00
#> 4 nest_data_table 402.71µs 459.55µs   1918.    985.25KB     4.00
#> 5 nest_split       11.87ms  14.08ms     69.8     1.15MB    54.3
#> 6 nest_table      183.52ms 189.87ms      5.20  576.35MB     8.67
#' @importFrom vctrs new_data_frame vec_unique_loc vec_duplicate_any
`%nest%` <- function(x, y) {
    # we don't check the inputs for performance
    loc <- vec_unique_loc(new_data_frame(list(x = x, y = y)))
    !vec_duplicate_any(vec_slice(x, loc))
}

save_png <- function(code, width = 400L, height = 400L) {
    path <- tempfile(fileext = ".png")
    grDevices::png(path, width = width, height = height)
    on.exit(grDevices::dev.off())
    print(code)
    path
}

add_class <- function(x, ...) {
    class(x) <- c(..., class(x))
    x
}

na_if <- function(x, y) ifelse(is.na(x), y, x)

###########################################################
switch_position <- function(position, x, y) {
    switch(position,
        top = ,
        bottom = x,
        left = ,
        right = y
    )
}

to_direction <- function(position) {
    switch_position(position, "vertical", "horizontal")
}

is_vertical <- function(direction) direction == "vertical"

is_horizontal <- function(direction) direction == "horizontal"

switch_direction <- function(direction, h, v) {
    if (is_horizontal(direction)) {
        h
    } else {
        v
    }
}

to_coord_axis <- function(direction) {
    switch_direction(direction, "y", "x")
}

to_matrix_axis <- function(direction) {
    switch_direction(direction, "row", "column")
}

##########################################################
#' @importFrom vctrs data_frame
data_frame0 <- function(...) data_frame(..., .name_repair = "minimal")

as_data_frame0 <- function(data, ...) {
    as.data.frame(
        x = data, ...,
        make.names = FALSE,
        stringsAsFactors = FALSE,
        fix.empty.names = FALSE
    )
}

quickdf <- function(x) {
    class(x) <- "data.frame"
    attr(x, "row.names") <- .set_row_names(length(.subset2(x, 1L)))
    x
}

#' @importFrom rlang set_names
#' @importFrom vctrs vec_locate_matches vec_slice
full_join <- function(x, y, by = intersect(names(x), names(y)),
                      by.x = by, by.y = by) {
    loc <- vec_locate_matches(x[by.x], set_names(y[by.y], by.x), remaining = NA)
    x_slicer <- .subset2(loc, "needles")
    y_slicer <- .subset2(loc, "haystack")
    ans <- vec_cbind(
        vec_slice(x, x_slicer),
        # drop duplicated join column
        vec_slice(y[setdiff(names(y), by.y)], y_slicer)
    )
    if (anyNA(x_slicer)) {
        new_rows <- which(is.na(x_slicer)) # should come from `y`
        ans[new_rows, by.x] <- vec_slice(y[by.y], y_slicer[new_rows])
    }
    ans
}

#' @importFrom vctrs new_data_frame vec_rep vec_rep_each
melt_matrix <- function(matrix) {
    row_nms <- rownames(matrix)
    col_nms <- colnames(matrix)
    data <- new_data_frame(list(
        .row_index = vec_rep(seq_len(nrow(matrix)), ncol(matrix)),
        .column_index = vec_rep_each(seq_len(ncol(matrix)), nrow(matrix)),
        value = c(matrix)
    ))
    if (!is.null(row_nms)) data$.row_names <- row_nms[data$.row_index]
    if (!is.null(col_nms)) data$.column_names <- col_nms[data$.column_index]
    data
}

fct_rev <- function(x) {
    ans <- as.factor(x)
    factor(ans, levels = rev(levels(ans)))
}

imap <- function(.x, .f, ...) {
    nms <- names(.x)
    out <- .mapply(.f, list(.x, nms %||% seq_along(.x)), NULL)
    if (!is.null(nms)) names(out) <- nms
    out
}

#' @importFrom vctrs list_sizes
compact <- function(.x) .x[list_sizes(.x) > 0L]

#' Rename elements in a list, data.frame or vector
#'
#' This is akin to `dplyr::rename` and `plyr::rename`. It renames elements given
#' as names in the `replace` vector to the values in the `replace` vector
#' without touching elements not referenced.
#'
#' @param x A data.frame or a named vector or list
#' @param replace A named character vector. The names identifies the elements in
#' `x` that should be renamed and the values gives the new names.
#'
#' @return `x`, with new names according to `replace`
#'
#' @keywords internal
#' @importFrom rlang set_names
#' @importFrom vctrs vec_slice<-
#' @noRd
rename <- function(x, replace) {
    set_names(x, function(nms) {
        nms <- set_names(nms)
        vec_slice(nms, names(replace)) <- replace
        nms
    })
}

reverse_trans <- function(x) sum(range(x, na.rm = TRUE)) - x

fclass <- function(x) .subset(class(x), 1L)

is_scalar <- function(x) length(x) == 1L

is_scalar_numeric <- function(x) {
    length(x) == 1L && is.numeric(x)
}

transpose <- function(.l) {
    if (!length(.l)) return(.l) # styler: off
    inner_names <- names(.l[[1L]])
    if (is.null(inner_names)) {
        fields <- seq_along(.l[[1L]])
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

oxford_and <- function(chr, code = TRUE, quote = TRUE, sep = ", ") {
    oxford_comma(code_quote(chr, code, quote), sep = sep, final = "and")
}

oxford_or <- function(chr, code = TRUE, quote = TRUE, sep = ", ") {
    oxford_comma(code_quote(chr, code, quote), sep = sep, final = "or")
}

code_quote <- function(x, code = TRUE, quote = TRUE) {
    if (quote) x <- paste0("\"", x, "\"")
    if (code) x <- paste0("`", x, "`")
    x
}
