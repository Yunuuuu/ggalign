#' Helper function to create a pair of links
#'
#' - `link`: Helper function to create a pair of links.
#' - `link_range`: Helper function to target a range of observations.
#'
#' @param link1,link2 An integer or character index, or a `link_range()` object
#'   to define the linked observations. For integer indices, you can wrap them
#'   with [`I()`] to indicate the order based on the layout. You can also use
#'   `waiver()` to inherit the values from the opposite link argument.
#' @export
link <- function(link1 = NULL, link2 = NULL) {
    if (!is_valid_link(link1)) {
        cli_abort(
            "{.arg link1} must be a numeric or character index or a {.fn link_range} object"
        )
    }
    if (!is_valid_link(link2)) {
        cli_abort(
            "{.arg link2} must be a numeric or character index or a {.fn link_range} object"
        )
    }
    if (is.null(link1) && is.null(link2)) {
        cli_abort(
            "At least one of {.arg link1} or {.arg link2} must not be `NULL`."
        )
    }
    new_link(link1, link2)
}

new_link <- function(link1 = NULL, link2 = NULL) {
    structure(list(link1 = link1, link2 = link2), class = "ggalign_link")
}

check_link_list <- function(link, arg = caller_arg(link),
                            call = caller_call()) {
    if (is_valid_link(link)) {
        list(link)
    } else if (is.list(link)) {
        valid <- vapply(link, is_valid_link, logical(1L), USE.NAMES = FALSE)
        if (!all(valid)) {
            cli_abort(
                "{.arg {arg}} must be a numeric or character index, a {.fn link_range} object, or a list of these.",
                call = call
            )
        }
        link
    } else {
        cli_abort(
            "{.arg {arg}} must be a numeric or character index, a {.fn link_range} object, or a list of these.",
            call = call
        )
    }
}

is_valid_link <- function(x) {
    is.null(x) ||
        is.numeric(x) ||
        is.character(x) ||
        inherits(x, "link_range")
}

#' @param point1,point2 A single integer or character index, defining the lower
#'   and higher bounds of the range. For integer indices, wrap them with [`I()`]
#'   to indicate the ordered index by the layout.
#' @export
#' @rdname link
link_range <- function(point1, point2) {
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg ...} must be a single numeric or character index")
    }
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg ...} must be a single numeric or character index")
    }
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
