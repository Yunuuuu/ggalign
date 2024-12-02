`%||%` <- function(x, y) if (is.null(x)) y else x

# `vec_rep`
recycle_whole <- function(x, len) {
    out <- x %% len
    if (out == 0L) len else out
}

# `vec_rep_each`
recycle_each <- function(x, len = NULL) {
    if (is.null(len)) {
        x
    } else {
        (x - 1L) %/% len + 1L
    }
}

#' @importFrom utils modifyList
update_non_waive <- function(old, new, keep_null = TRUE) {
    modifyList(old,
        new[!vapply(new, is.waive, logical(1L), USE.NAMES = FALSE)],
        keep.null = keep_null
    )
}

###########################################################
format_object_name <- function(name, format = NULL) {
    if (is.null(format)) {
        name
    } else {
        sprintf("{.%s %s}", format, name)
    }
}

object_name <- function(object, format) UseMethod("object_name")

#' @export
object_name.StackLayout <- function(object, format = "fn") {
    ans <- format_object_name(object@name, format)
    if (!is.null(position <- .subset2(object@heatmap, "position"))) {
        ans <- sprintf("the %s annotation %s", position, ans)
    } else {
    }
    ans
}

#' @export
object_name.QuadLayout <- function(object, format = "fn") {
    format_object_name(object@name, format)
}

#' @export
object_name.ggalign_align_plot <- function(object, format = "fn") {
    format_object_name(.subset2(object, "workflow"), format)
}

#' @export
object_name.AlignProto <- function(object, format = "fn") {
    format_object_name(snake_class(object), format)
}

#################################################################
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

#' @param ans Whether to assign the final results into the 'ans' variable.
#' @noRd
fn_body_append <- function(fn, ..., ans = FALSE) {
    args <- rlang::fn_fmls(fn)
    body <- rlang::fn_body(fn)
    body <- as.list(body)
    if (ans) body[[length(body)]] <- rlang::expr(ans <- !!body[[length(body)]])
    body <- as.call(c(body, rlang::enexprs(...)))
    rlang::new_function(args, body)
}

# This will work with most things but be aware that it might fail with some
# complex objects. For example, according to `?S3Methods`, calling foo on
# matrix(1:4, 2, 2) would try `foo.matrix`, then `foo.numeric`, then
# `foo.default`; whereas this code will just look for `foo.matrix` and
# `foo.default`.
#' @importFrom utils getS3method
#' @importFrom methods extends
has_method <- function(x, f, inherit = TRUE, default = inherit) {
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
#> # A tibble: 6 x 6
#>   expression           min   median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>      <bch:tm> <bch:tm>     <dbl> <bch:byt>    <dbl>
#> 1 nest_unique       5.37ms   7.49ms    134.      1.33MB    69.2
#> 2 nest_vctrs       200.3us 214.57us   3591.     754.7KB     6.00
#> 3 nest_vctrs_loc  193.99us 207.29us   4490.    706.95KB     6.00
#> 4 nest_data_table 402.71us 459.55us   1918.    985.25KB     4.00
#> 5 nest_split       11.87ms  14.08ms     69.8     1.15MB    54.3
#> 6 nest_table      183.52ms 189.87ms      5.20  576.35MB     8.67
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
    class(x) <- vec_unique(c(..., class(x)))
    x
}

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

fct_rev <- function(x) {
    ans <- as.factor(x)
    factor(ans, levels = rev(levels(ans)))
}

reverse_trans <- function(x) sum(range(x, na.rm = TRUE)) - x

fclass <- function(x) .subset(class(x), 1L)

is_scalar <- function(x) length(x) == 1L

is_scalar_numeric <- function(x) length(x) == 1L && is.numeric(x)

# utils function to collapse characters ---------------------------
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

oxford_comma <- function(chr, sep = ", ", final = "and") {
    n <- length(chr)

    if (n < 2L) {
        return(chr)
    }

    head <- chr[seq_len(n - 1L)]
    last <- chr[n]

    head <- paste(head, collapse = sep)

    # Write a or b. But a, b, or c.
    if (n > 2L) {
        paste0(head, sep, final, " ", last)
    } else {
        paste0(head, " ", final, " ", last)
    }
}
