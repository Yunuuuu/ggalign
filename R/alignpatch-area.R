area <- function(t, l, b = t, r = l) {
    if (missing(t) || missing(l)) {
        one_area <- list(
            t = integer(0L),
            l = integer(0L),
            b = integer(0L),
            r = integer(0L)
        )
    } else {
        len <- max(length(t), length(l), length(b), length(r))
        one_area <- list(
            t = rep_len(as.integer(t), len),
            l = rep_len(as.integer(l), len),
            b = rep_len(as.integer(b), len),
            r = rep_len(as.integer(r), len)
        )
        if (any(t > b)) {
            cli::cli_abort("{.arg t} must be less than {.arg b}")
        }
        if (any(l > r)) {
            cli::cli_abort("{.arg l} must be less than {.arg r}")
        }
    }
    class(one_area) <- "align_area"
    one_area
}

as_areas <- function(x) UseMethod("as_areas")

#' @export
as_areas.default <- function(x) {
    cli::cli_abort("Cannot convert {.obj_type_friendly {x}} into a design area")
}

#' @export
as_areas.align_area <- function(x) x

#' @export
as_areas.character <- function(x) {
    call <- current_call() # used for message only
    # split into rows
    x <- .subset2(strsplit(x, split = "\n"), 1L)
    x <- lapply(x, trimws)
    if (identical(x[[1L]], "")) x[[1L]] <- NULL
    if (identical(x[[length(x)]], "")) x[[length(x)]] <- NULL
    x <- lapply(x, function(x) .subset2(strsplit(x, split = ""), 1L))
    ncols <- lengths(x)
    ncol <- .subset(ncols, 1L)
    if (any(ncols != ncol)) {
        cli::cli_abort("character layout must be rectangular", call = call)
    }
    row <- rep(seq_along(x), each = ncol)
    col <- rep(seq_len(ncol), length(x))
    x <- unlist(x, use.names = FALSE)
    # here, area will be reordered by the levels of `x`
    area_list <- imap(split(seq_along(x), x), function(i, name) {
        if (identical(name, "#")) {
            return(area())
        }
        area_rows <- range(row[i])
        area_cols <- range(col[i])
        t <- .subset(area_rows, 1L)
        l <- .subset(area_cols, 1L)
        b <- .subset(area_rows, 2L)
        r <- .subset(area_cols, 2L)
        if (!all(x[row >= t & row <= b & col >= l & col <= r] ==
            x[.subset(i, 1L)])) {
            cli::cli_abort("Patch areas must be rectangular", call = call)
        }
        area(t = t, l = l, b = b, r = r)
    })
    do.call(c, area_list)
}

# For area from patchwork
#' @export
as_areas.patch_area <- function(x) {
    class(x) <- "align_area"
    x
}

#' @export
c.align_area <- function(..., recursive = FALSE) {
    if (...length() == 0L) {
        return(area())
    }
    all_areas <- list(...)
    area(
        unlist(lapply(all_areas, .subset2, "t")),
        unlist(lapply(all_areas, .subset2, "l")),
        unlist(lapply(all_areas, .subset2, "b")),
        unlist(lapply(all_areas, .subset2, "r"))
    )
}

#' @export
length.align_area <- function(x) length(.subset2(x, "t"))

#' @export
print.align_area <- function(x, ...) {
    cat(
        length(x), "patch areas, spanning",
        max(x$r), "columns and", max(x$b), "rows\n\n"
    )
    print(data.frame(unclass(x), row.names = paste0(seq_along(x), ": ")))
    invisible(x)
}
