`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_nm <- function() utils::packageName(topenv(environment()))

#' Build matrix for heatmap and anno_cluster
#' @param matrix A matrix, if it is a simple vector, it will be converted to a
#' one-column matrix. Data.frame will also be coerced into matrix.
build_matrix <- function(
    matrix, id = "V1",
    arg = rlang::caller_arg(matrix),
    call = rlang::caller_env()) {
    assert_string(id, call = call)
    if (inherits(matrix, "data.frame")) {
        matrix <- as.matrix(matrix)
    } else if (is.matrix(matrix)) {

    } else if (is.atomic(matrix)) {
        cli::cli_alert_info(
            "convert simple vector {.arg {arg}} to one-column {.cls matrix}"
        )
        matrix <- matrix(matrix, ncol = 1L)
        colnames(matrix) <- id
    } else {
        cli::cli_abort(paste(
            "{.arg {arg}} must be a {.cls matrix},",
            "a simple vector, or a {.cls data.frame}."
        ), call = call)
    }
    matrix
}

#' Build data.frame for annotation.
#' @param data A data.frame, if it is a simple vector, it will be converted to a
#' one-column data.frame with "V1" as column name. Matrix will also be coerced
#' into data.frame.
build_tibble <- function(
    data, id = "V1",
    arg = rlang::caller_arg(data),
    call = rlang::caller_env()) {
    assert_string(id)
    if (is.matrix(data)) {
        data <- as_tibble0(data, rownames = id)
    } else if (is.atomic(data)) {
        cli::cli_alert_info(
            "convert simple vector {.arg {arg}} to one-column {.cls data.frame}"
        )
        data <- tibble::as_tibble_col(data, column_name = id)
    } else if (!inherits(data, "data.frame")) {
        cli::cli_abort(paste(
            "{.arg {arg}} must be a {.cls data.frame},",
            "a simple vector, or a {.cls matrix}."
        ), call = call)
    }
    data
}

build_name <- function(name, type = c("heatmap", "annotation"), arg = rlang::caller_arg(name)) {
    if (is.null(name)) {
        type <- match.arg(type, c("heatmap", "annotation"))
        name <- sprintf("%s-%d", type, set_index(type))
    } else if (!rlang::is_string(name)) {
        cli::cli_abort("{.arg {arg}} must be a string")
    }
    name
}

set_index <- local({
    heatmap_index <- 0L
    annotation_index <- 0L
    function(x) {
        switch(x,
            heatmap = heatmap_index <<- heatmap_index + 1L,
            annotation = annotation_index <<- annotation_index + 1L
        )
    }
})

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

compact <- function(.x) {
    Filter(length, .x)
}

pindex <- function(array, ...) {
    if (length(dim(array)) != ...length()) {
        cli::cli_abort("Indexing must have as many as the number of dimentions of array")
    }
    dots <- rlang::list2(...)
    # all index must be atomic
    right <- vapply(dots, function(x) {
        is.atomic(x) && !is.null(x) && !is.matrix(x)
    }, logical(1L))
    if (!all(right)) {
        cli::cli_abort("All elements in {.arg ...} must be atomic")
    }
    l <- lengths(dots)
    if (any(l == 0L)) {
        cli::cli_abort("Empty index is not allowed")
    }
    expected_len <- max(l)
    if (any(l > 1L & l < expected_len)) {
        cli::cli_abort("Only length one are recycled")
    }
    if (expected_len != 1L) {
        dots <- lapply(dots, rep_len, length.out = expected_len)
    }
    array[do.call("cbind", dots)]
}

recycle_dots <- function(..., length = NULL, args = NULL) {
    args <- args %||%
        unlist(lapply(substitute(...()), as.character), use.names = FALSE)
    lst <- rlang::list2(...)
    l <- lengths(lst)
    expected_len <- length %||% max(l)
    if (!all(l == 1L | l == expected_len)) {
        cli::cli_abort(c(
            "{.arg {args}} must have compatible sizes",
            i = "Only length one will be recycled."
        ))
    }
    lapply(lst, rep_len, length.out = expected_len)
}

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

allow_lambda <- function(x) {
    if (rlang::is_formula(x) || rlang::is_string(x)) {
        rlang::as_function(x)
    } else {
        x
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

reverse_trans <- function(x) {
    sum(range(x, na.rm = TRUE)) - x
}

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
