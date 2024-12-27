#' Helper function to create a pair of links
#'
#' - `pair_link`: Helper function to create a pair of links.
#' - `link_range`: Helper function to create a range of observations.
#'
#' @param ... A list of formula, each side of the formula should be an integer
#'   or character index, or a `link_range()` object to define the linked
#'   observations. For integer indices, you can wrap them with [`I()`] to
#'   indicate the order based on the layout. You can also use `waiver()` to
#'   inherit the values from the opposite link argument.
#' @export
pair_links <- function(...) {
    pairs <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
    links <- vector("list", length(pairs))
    for (i in seq_along(pairs)) {
        if (rlang::is_formula(pair <- .subset2(pairs, i))) {
            link1 <- rlang::eval_tidy(
                rlang::f_lhs(pair),
                env = rlang::f_env(pair)
            )
            link2 <- rlang::eval_tidy(
                rlang::f_rhs(pair),
                env = rlang::f_env(pair)
            )
            if (!is_valid_link(link1) || !is_valid_link(link2)) {
                cli_abort(
                    "Input must be either an integer or a character index, or a {.fn link_range} object."
                )
            }
            links[[i]] <- new_pair_link(link1, link2, arg1 = "", arg2 = "")
        } else {
            cli_abort(c(
                "{.arg ...} must be created with {.code ~}",
                i = "Location: {i}"
            ))
        }
    }
    # remove links with both are `NULL`
    links <- links[!vapply(links, function(link) {
        all(vapply(link, is.null, logical(1L), USE.NAMES = FALSE))
    }, logical(1L), USE.NAMES = FALSE)]
    new_pair_links(links)
}

#' @export
c.ggalign_pair_links <- function(...) {
    new_pair_links(NextMethod())
}

new_pair_links <- function(x) structure(x, class = "ggalign_pair_links")

#' @export
print.ggalign_pair_links <- function(x, ..., header = TRUE) {
    l <- length(x)
    if (isTRUE(header)) {
        header <- sprintf(
            "<%s> %d pair%s of links%s",
            fclass(x), l,
            if (l > 1L) "s" else "",
            if (l > 0L) ":" else ""
        )
    } else {
        header <- NULL
    }
    if (l > 0L) {
        link1 <- vapply(x, function(link) {
            deparse_link(.subset2(link, "link1"), ...)
        }, character(1L), USE.NAMES = FALSE)
        link2 <- vapply(x, function(link) {
            deparse_link(.subset2(link, "link2"), ...)
        }, character(1L), USE.NAMES = FALSE)
        rownms <- c("", paste(seq_along(link1), link1, sep = ":  "))
        rownms <- format(rownms, justify = "right")
        link1 <- format(c("link1", link1), justify = "right")
        link2 <- format(c("link2", link2), justify = "left")
        content <- paste0("  ", rownms, link1, " ~ ", link2)
    } else {
        content <- NULL
    }
    cat(c(header, content), sep = "\n")
    invisible(x)
}

new_pair_link <- function(link1 = NULL, link2 = NULL, arg1 = caller_arg(link1),
                          arg2 = caller_arg(link2), call = caller_call()) {
    link1 <- new_link(link1, arg = arg1, call = call)
    link2 <- new_link(link2, arg = arg2, call = call)
    structure(list(link1 = link1, link2 = link2), class = "ggalign_pair_link")
}

new_link <- function(link, arg = caller_arg(link), call = caller_call()) {
    if (is.numeric(link)) {
        if (inherits(link, "AsIs")) {
            link <- I(vec_cast(link, integer(), x_arg = arg, call = call))
        } else {
            link <- vec_cast(link, integer(), x_arg = arg, call = call)
        }
    }
    link
}

is_valid_link <- function(x) {
    is.waive(x) ||
        is.null(x) ||
        is.numeric(x) ||
        is.character(x) ||
        inherits(x, "link_range")
}

#' @export
print.ggalign_pair_link <- function(x, ...) {
    header <- "A pair of link:"
    content <- c(
        paste("  link1:", deparse_link(.subset2(x, "link1"))),
        paste("  link2:", deparse_link(.subset2(x, "link2")))
    )
    cat(c(header, content), sep = "\n")
    invisible(x)
}

#' @return A single string
#' @noRd
deparse_link <- function(x, ...) UseMethod("deparse_link")

#' @export
deparse_link.integer <- function(x, trunc = 3L, head = trunc - 1L,
                                 tail = 1L, ...) {
    l <- length(x)
    out <- paste(deparse(x), collapse = " ")
    if (l > trunc && startsWith(out, "c")) {
        out <- sprintf("c(%s)", paste(c(
            x[seq_len(head)], "...", x[seq.int(l - tail + 1L, l)]
        ), collapse = ", "))
    }
    out
}

#' @export
deparse_link.character <- function(x, trunc = 3L, head = trunc - 1L,
                                   tail = 1L, ...) {
    l <- length(x)
    if (l <= trunc) {
        out <- paste(deparse(x), collapse = " ")
    } else {
        out <- sprintf("c(%s)", paste(c(
            x[seq_len(head)], "...", x[seq.int(l - tail + 1L, l)]
        ), collapse = ", "))
    }
    out
}

#' @export
deparse_link.NULL <- function(x, ...) ""

#' @export
deparse_link.AsIs <- function(x, ...) {
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    sprintf("I(%s)", deparse_link(x, ...))
}

#' @export
deparse_link.link_range <- function(x, ...) {
    sprintf(
        "link_range(%s, %s)",
        deparse_link(.subset2(x, "point1"), ...),
        deparse_link(.subset2(x, "point2"), ...)
    )
}

#' @export
deparse_link.waiver <- function(x, ...) "waiver()"

#' @param point1,point2 A single integer or character index, defining the lower
#'   and higher bounds of the range. For integer indices, wrap them with [`I()`]
#'   to indicate the ordered index by the layout.
#' @export
#' @rdname pair_links
link_range <- function(point1, point2) {
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg ...} must be a single numeric or character index")
    }
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg ...} must be a single numeric or character index")
    }
    point1 <- new_link(point1)
    point2 <- new_link(point2)
    structure(list(point1 = point1, point2 = point2), class = "link_range")
}

make_link_data <- function(link, design, labels = NULL,
                           arg = caller_arg(link)) {
    if (is.null(link)) {
        return(NULL)
    }

    n <- .subset2(design, "nobs")
    if (!inherits(link, "AsIs") || is.character(link)) {
        # match the original data index
        if (inherits(link, "link_range")) {
            point1 <- .subset2(link, "point1")
            if (!inherits(point1, "AsIs") || is.character(point1)) {
                point1 <- vec_as_location(
                    point1,
                    n = n,
                    names = labels,
                    arg = "point1",
                    call = quote(link_range())
                )
            }
            point2 <- .subset2(link, "point2")
            if (!inherits(point2, "AsIs") || is.character(point2)) {
                point2 <- vec_as_location(
                    point2,
                    n = n,
                    names = labels,
                    arg = "point2",
                    call = quote(link_range())
                )
            }
            link <- match(c(point1, point2), .subset2(design, "index"))
            link <- (link[1L]):(link[2L])
        } else {
            link <- vec_as_location(
                link,
                n = n,
                names = labels,
                arg = arg,
                call = quote(link())
            )
            link <- match(link, .subset2(design, "index"))
        }
    } else if (inherits(link, "link_range")) {
        point1 <- .subset2(link, "point1")
        # for character, we always match the original data
        if (is.character(point1)) {
            point1 <- vec_as_location(
                point1,
                n = n,
                names = labels,
                arg = "point1",
                call = quote(link_range())
            )
            point1 <- match(point1, .subset2(design, "index"))
        }
        point2 <- .subset2(link, "point2")
        if (is.character(point2)) {
            point2 <- vec_as_location(
                point2,
                n = n,
                names = labels,
                arg = "point2",
                call = quote(link_range())
            )
            point2 <- match(point2, .subset2(design, "index"))
        }
        link <- point1:point2
    }

    # always use integer, otherwise, will cause error when drawing
    # due to loss of precision, I don't know why, it should be integer already?
    vec_cast(link, integer())
}
