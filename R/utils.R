`%||%` <- function(x, y) if (is.null(x)) y else x

`%|w|%` <- function(x, y) if (inherits(x, "waiver")) y else x

pkg_nm <- function() utils::packageName(topenv(environment()))

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) {
        rlang::as_function(x)
    } else {
        x
    }
}

melt_matrix <- function(matrix) {
    row_nms <- rownames(matrix)
    col_nms <- colnames(matrix)
    data <- as_tibble0(matrix, rownames = NULL) # nolint
    colnames(data) <- seq_len(ncol(data))
    data$.row_index <- seq_len(nrow(data))
    data <- tidyr::pivot_longer(data,
        cols = !".row_index",
        names_to = ".column_index",
        values_to = "value"
    )
    data$.column_index <- as.integer(data$.column_index)
    if (!is.null(row_nms)) data$.row_names <- row_nms[data$.row_index]
    if (!is.null(col_nms)) data$.column_names <- col_nms[data$.column_index]
    data
}

# Since ggplot2 always use tibble, we'll use it too.
tibble0 <- function(...) {
    tibble::tibble(..., .name_repair = "minimal")
}

as_tibble0 <- function(data, ...) {
    tibble::as_tibble(data, ..., .name_repair = "minimal")
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

# List of all aesthetics known to ggplot
# (In the future, .all_aesthetics should be removed in favor
# of direct assignment to ggplot_global$all_aesthetics, see below.)
.all_aesthetics <- c(
    "adj", "alpha", "angle", "bg", "cex", "col", "color",
    "colour", "fg", "fill", "group", "hjust", "label", "linetype", "lower",
    "lty", "lwd", "max", "middle", "min", "pch", "radius", "sample", "shape",
    "size", "srt", "upper", "vjust", "weight", "width", "x", "xend", "xmax",
    "xmin", "xintercept", "y", "yend", "ymax", "ymin", "yintercept", "z"
)

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
