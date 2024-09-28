`%||%` <- function(x, y) if (is.null(x)) y else x

`%|w|%` <- function(x, y) if (inherits(x, "waiver")) y else x

#' @importFrom utils packageName
pkg_nm <- function() packageName(topenv(environment()))

#' @param ans Whether to assign the final results into the 'ans' variable.
#' @noRd
body_append <- function(fn, ..., ans = TRUE) {
    args <- rlang::fn_fmls(fn)
    body <- rlang::fn_body(fn)
    body <- as.list(body)
    if (ans) body[[length(body)]] <- rlang::expr(ans <- !!body[[length(body)]])
    body <- as.call(c(body, rlang::enexprs(...)))
    rlang::new_function(args, body)
}

make_order <- function(order) {
    # we always add 0.5 offset, this will ensure the order set is always
    # in the front of the order not set
    # make_order(c(NA, 1, NA)): c(2, 1, 3) instead c(1, 2, 3)
    order[is.na(order)] <- seq_along(order)[is.na(order)] + 0.5
    order(order)
}

#' @importFrom data.table data.table
`%nest%` <- function(x, y) {
    ans <- data.table(x = x, y = y)
    ans <- unique(ans)
    !anyDuplicated(ans$x)
}

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

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) {
        rlang::as_function(x)
    } else {
        x
    }
}

#' @importFrom ggplot2 zeroGrob
get_grob <- function(x, name) {
    nms <- .subset2(.subset2(x, "layout"), "name")
    pattern <- paste0("^", name, "$")
    ind <- grep(pattern, nms)
    if (length(ind) == 0) {
        return(zeroGrob())
    }
    .subset2(.subset2(x, "grobs"), grep(pattern, nms))
}

#' @importFrom vctrs vec_slice
#' @importFrom gtable gtable_trim
subset_gt <- function(gt, index, trim = TRUE) {
    gt$layout <- vec_slice(.subset2(gt, "layout"), index)
    gt$grobs <- .subset(.subset2(gt, "grobs"), index)
    if (trim) gtable_trim(gt) else gt
}

gtable_trim_widths <- function(gt) {
    layout <- .subset2(gt, "layout")
    w <- range(.subset2(layout, "l"), .subset2(layout, "r"))
    gt$widths <- .subset2(gt, "widths")[seq.int(w[1L], w[2L])]
    if (is.matrix(respect <- .subset2(gt, "respect"))) {
        respect <- respect[, seq.int(w[1L], w[2L]), drop = FALSE]
        if (all(respect == 0L)) respect <- FALSE
        gt$respect <- respect
    }
    layout$l <- .subset2(layout, "l") - w[1L] + 1L
    layout$r <- .subset2(layout, "r") - w[1L] + 1L
    gt$layout <- layout
    gt
}

gtable_trim_heights <- function(gt) {
    layout <- .subset2(gt, "layout")
    h <- range(.subset2(layout, "t"), .subset2(layout, "b"))
    gt$heights <- .subset2(gt, "heights")[seq.int(h[1L], h[2L])]
    if (is.matrix(respect <- .subset2(gt, "respect"))) {
        respect <- respect[seq.int(h[1L], h[2L]), , drop = FALSE]
        if (all(respect == 0L)) respect <- FALSE
        gt$respect <- respect
    }
    layout$t <- .subset2(layout, "t") - h[1L] + 1L
    layout$b <- .subset2(layout, "b") - h[1L] + 1L
    gt$layout <- layout
    gt
}

trim_area <- function(area) {
    w <- min(.subset2(area, "l"), .subset2(area, "r"))
    h <- min(.subset2(area, "t"), .subset2(area, "b"))
    area$l <- .subset2(area, "l") - w + 1L
    area$r <- .subset2(area, "r") - w + 1L
    area$t <- .subset2(area, "t") - h + 1L
    area$b <- .subset2(area, "b") - h + 1L
    area
}

is.waive <- function(x) inherits(x, "waiver")

add_default_mapping <- function(plot, default_mapping) {
    mapping <- .subset2(plot, "mapping")
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    plot$mapping <- default_mapping
    plot
}

#' @importFrom rlang env_clone
ggproto_clone <- function(ggproto) {
    ans <- env_clone(ggproto)
    class(ans) <- class(ggproto)
    ans
}

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

#' @importFrom data.table melt setDF setnames
melt_matrix <- function(matrix) {
    row_nms <- rownames(matrix)
    col_nms <- colnames(matrix)
    data <- as.data.table(matrix)
    setnames(data, as.character(seq_len(ncol(matrix))))
    data$.row_index <- seq_len(nrow(data))
    data <- melt(data,
        id.vars = ".row_index",
        variable.name = ".column_index",
        variable.factor = FALSE,
        value.name = "value",
        verbose = FALSE
    )
    data$.column_index <- as.integer(data$.column_index)
    if (!is.null(row_nms)) data$.row_names <- row_nms[data$.row_index]
    if (!is.null(col_nms)) data$.column_names <- col_nms[data$.column_index]
    setDF(data)
}

fct_rev <- function(x) {
    ans <- as.factor(x)
    factor(ans, levels = rev(levels(ans)))
}

quickdf <- function(x) {
    class(x) <- "data.frame"
    attr(x, "row.names") <- .set_row_names(length(.subset2(x, 1L)))
    x
}

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

imap <- function(.x, .f, ...) {
    nms <- names(.x)
    out <- .mapply(.f, list(.x, nms %||% seq_along(.x)), NULL)
    if (!is.null(nms)) names(out) <- nms
    out
}

compact <- function(.x) .x[lengths(.x) > 0L]

recycle_scalar <- function(x, length, arg = rlang::caller_arg(x)) {
    l <- length(x)
    if (l == 1L || l == length) {
        rep_len(x, length)
    } else {
        if (length != 1L) {
            msg <- sprintf("1 or %d", length) # nolint
        } else {
            msg <- "1"
        }
        cli::cli_abort("length of {.arg {arg}} can only be {msg}")
    }
}

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
#' @noRd
rename <- function(x, replace) {
    current_names <- names(x)
    old_names <- names(replace)
    missing_names <- setdiff(old_names, current_names)
    if (length(missing_names) > 0) {
        replace <- replace[!old_names %in% missing_names]
        old_names <- names(replace)
    }
    names(x)[match(old_names, current_names)] <- as.vector(replace)
    x
}

reverse_trans <- function(x) sum(range(x, na.rm = TRUE)) - x

fclass <- function(x) .subset(class(x), 1L)

is_scalar <- function(x) length(x) == 1L

is_scalar_numeric <- function(x) {
    length(x) == 1L && is.numeric(x)
}

is_discrete <- function(x) {
    is.factor(x) || is.character(x) || is.logical(x)
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

# Use chartr() for safety since toupper() fails to convert i to I in Turkish locale
lower_ascii <- "abcdefghijklmnopqrstuvwxyz"
upper_ascii <- "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
to_lower_ascii <- function(x) chartr(upper_ascii, lower_ascii, x)
to_upper_ascii <- function(x) chartr(lower_ascii, upper_ascii, x)
snakeize <- function(x) {
    x <- gsub("([A-Za-z])([A-Z])([a-z])", "\\1_\\2\\3", x)
    x <- gsub(".", "_", x, fixed = TRUE)
    x <- gsub("([a-z])([A-Z])", "\\1_\\2", x)
    to_lower_ascii(x)
}

firstUpper <- function(s) {
    paste0(to_upper_ascii(substring(s, 1L, 1L)), substring(s, 2L))
}

snake_class <- function(x) snakeize(fclass(x))
