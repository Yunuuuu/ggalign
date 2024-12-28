#' Helper function to create a pair of links
#'
#' - `pair_link`: Helper function to create a pair of links.
#' - `range_link`: Helper function to create a range of observations.
#'
#' @param ... A list of formulas, where each side of the formula should be an
#'   `integer` or `character` index, or a `range_link()` object defining the
#'   linked observations, use `NULL` to indicate no link in this side. If only
#'   the left-hand side of the formula exists, you can input it directly. For
#'   integer indices, wrap them with [`I()`] to indicate their order according
#'   to the layout. You can also use `waiver()` to inherit values from the
#'   opposite link argument.
#' @examples
#' x <- pair_links(
#'     1:2,
#'     c("a", "b"),
#'     range_link(1, 6) ~ c("a", "b"),
#'     ~ 1:2,
#'     ~letters,
#'     ~NULL # a blank link
#' )
#' x
#'
#' # we can modify it as usual list
#' x[[1]] <- NULL # remove the first link
#' x$a <- ~LETTERS
#' x
#'
#' # modify with a list of links
#' x[1:2] <- list(~ c("a", "b"), ~ range_link("a", "b"))
#' x
#' @export
pair_links <- function(...) {
    pairs <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
    pairs <- pairs[!vapply(pairs, is.null, logical(1L), USE.NAMES = FALSE)]
    new_pair_links(lapply(pairs, as_pair_link, x_arg = "..."))
}

new_pair_links <- function(x = list(), ..., class = character()) {
    # use list_of()?
    new_vctr(x, ..., class = c(class, "ggalign_pair_links"))
}

#' @export
obj_print_data.ggalign_pair_links <- function(x, ...) {
    if (length(x) > 0L) {
        link1 <- vapply(x, function(link) {
            deparse_link(.subset2(link, "link1"), ...)
        }, character(1L), USE.NAMES = FALSE)
        link2 <- vapply(x, function(link) {
            deparse_link(.subset2(link, "link2"), ...)
        }, character(1L), USE.NAMES = FALSE)
        nms <- rlang::names2(x)
        no <- nms == ""
        nms[no] <- seq_along(link1)[no]
        nms <- c("", paste0(nms, ":  "))
        nms <- format(nms, justify = "right")
        link1 <- format(c("link1", link1), justify = "right")
        link2 <- format(c("link2", link2), justify = "left")
        content <- paste0("  ", nms, link1, " ~ ", link2)
        cat(content, sep = "\n")
    }
    invisible(x)
}

#' @export
`[<-.ggalign_pair_links` <- function(x, i, value) {
    value <- lapply(value, as_pair_link, x_arg = "value")
    NextMethod()
}

#' @export
`[[<-.ggalign_pair_links` <- function(x, i, value) {
    value <- as_pair_link(value)
    NextMethod()
}

#' @export
`$<-.ggalign_pair_links` <- function(x, i, value) {
    value <- as_pair_link(value)
    NextMethod()
}

#########################################################
#' @param point1,point2 A single integer or character index, defining the lower
#'   and higher bounds of the range. For integer indices, wrap them with [`I()`]
#'   to indicate the ordered index by the layout.
#' @export
#' @rdname pair_links
range_link <- function(point1, point2) {
    if (!is_scalar(point1) ||
        (!is.character(point1) && !is.numeric(point1))) {
        cli_abort("{.arg point1} must be a single numeric or character index")
    }
    if (!is_scalar(point2) ||
        (!is.character(point2) && !is.numeric(point2))) {
        cli_abort("{.arg point2} must be a single numeric or character index")
    }
    point1 <- as_obs_link(point1)
    point2 <- as_obs_link(point2)
    structure(list(point1 = point1, point2 = point2),
        class = "ggalign_range_link"
    )
}

is_range_link <- function(x) inherits(x, "ggalign_range_link")

new_pair_link <- function(link1 = NULL, link2 = NULL, ..., class = character()) {
    if (is.null(link1) && is.null(link2)) {
        .data <- list() # `list_of` require length 0
    } else {
        .data <- list(link1 = link1, link2 = link2)
    }
    structure(
        .Data = .data,
        ...,
        class = c(class, "ggalign_pair_link")
    )
}

#' @export
vec_proxy.ggalign_pair_link <- function(x, ...) x

#' @export
print.ggalign_pair_link <- function(x, ...) obj_print(x, ...)

#' @export
obj_print_header.ggalign_pair_link <- function(x, ...) {
    cat(sprintf("<%s>", vec_ptype_full(x)), sep = "\n")
    invisible(x)
}

#' @export
obj_print_data.ggalign_pair_link <- function(x, ...) {
    if (length(x) > 0L) {
        content <- c(
            paste("  link1:", deparse_link(.subset2(x, "link1"), ...)),
            paste("  link2:", deparse_link(.subset2(x, "link2"), ...))
        )
        cat(content, sep = "\n")
    }
    invisible(x)
}

#' @export
length.ggalign_pair_link <- function(x) as.integer(length(unclass(x)) > 0L)

##################################################
#' @export
vec_ptype2.ggalign_pair_link.ggalign_pair_link <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.ggalign_pair_link.NULL <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.NULL.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.numeric <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.numeric.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.character <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.character.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.formula <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.formula.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.waiver <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.waiver.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.ggalign_range_link <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.ggalign_range_link.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.AsIs <- function(x, y, ...) {
    NextMethod()
}

#' @export
vec_ptype2.AsIs.ggalign_pair_link <- function(x, y, ...) {
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    vec_ptype2(x, y)
}

#############################################################
#' @export
vec_cast.ggalign_pair_link.numeric <- function(x, to, ...,
                                               x_arg = caller_arg(x),
                                               to_arg = "",
                                               call = caller_env()) {
    new_pair_link(as_obs_link(x, arg = x_arg, call = call))
}

#' @export
vec_cast.ggalign_pair_link.double <- vec_cast.ggalign_pair_link.numeric

#' @export
vec_cast.ggalign_pair_link.integer <- function(x, to, ...,
                                               x_arg = caller_arg(x),
                                               to_arg = "",
                                               call = caller_env()) {
    new_pair_link(x)
}

#' @export
vec_cast.ggalign_pair_link.character <- vec_cast.ggalign_pair_link.integer

#' @export
vec_cast.ggalign_pair_link.ggalign_range_link <-
    vec_cast.ggalign_pair_link.integer

#' @export
vec_cast.ggalign_pair_link.AsIs <- vec_cast.ggalign_pair_link.integer

#' @export
vec_cast.ggalign_pair_link.formula <- function(x, to, ...,
                                               x_arg = caller_arg(x),
                                               to_arg = "",
                                               call = caller_env()) {
    link1 <- rlang::eval_tidy(rlang::f_lhs(x), env = rlang::f_env(x))
    link1 <- as_obs_link(link1, arg = x_arg, call = call)
    link2 <- rlang::eval_tidy(rlang::f_rhs(x), env = rlang::f_env(x))
    link2 <- as_obs_link(link2, arg = x_arg, call = call)
    new_pair_link(link1, link2)
}

as_pair_link <- function(x, ...) vec_cast(x, to = new_pair_link(), ...)

########################################################
as_obs_link <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
    UseMethod("as_obs_link")
}

#' @export
as_obs_link.NULL <- function(x, ...) x

#' @export
as_obs_link.AsIs <- function(x, ...) I(NextMethod())

#' @export
as_obs_link.numeric <- function(x, ..., arg = caller_arg(link),
                                call = caller_env()) {
    vec_cast(x, integer(), x_arg = arg, call = call)
}

#' @export
as_obs_link.integer <- as_obs_link.NULL

#' @export
as_obs_link.double <- as_obs_link.numeric

#' @export
as_obs_link.character <- as_obs_link.NULL

#' @export
as_obs_link.waiver <- as_obs_link.NULL

#' @export
as_obs_link.ggalign_range_link <- as_obs_link.NULL

#' @export
as_obs_link.default <- function(x, ..., arg = caller_arg(link),
                                call = caller_env()) {
    stop_incompatible_cast(
        x, new_pair_link(),
        x_arg = arg, to_arg = "",
        call = call
    )
}

#' @export
print.ggalign_range_link <- function(x, ...) {
    cat(deparse_link(x))
    invisible(x)
}

#' @return A single string
#' @noRd
deparse_link <- function(x, ...) UseMethod("deparse_link")

#' @export
deparse_link.integer <- function(x, trunc = 3L, head = trunc - 1L,
                                 tail = 1L, ...) {
    l <- length(x)
    out <- paste(
        deparse(x, control = c("keepNA", "niceNames", "showAttributes")),
        collapse = " "
    )
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
deparse_link.ggalign_range_link <- function(x, ...) {
    sprintf(
        "range_link(%s, %s)",
        deparse_link(.subset2(x, "point1"), ...),
        deparse_link(.subset2(x, "point2"), ...)
    )
}

#' @export
deparse_link.waiver <- function(x, ...) "waiver()"

###################################################
make_link_data <- function(link, design, labels = NULL,
                           arg = caller_arg(link)) {
    if (is.null(link)) {
        return(NULL)
    }

    n <- .subset2(design, "nobs")
    if (!inherits(link, "AsIs") || is.character(link)) {
        # match the original data index
        if (is_range_link(link)) {
            point1 <- .subset2(link, "point1")
            if (!inherits(point1, "AsIs") || is.character(point1)) {
                point1 <- vec_as_location(
                    point1,
                    n = n,
                    names = labels,
                    arg = "point1",
                    call = quote(range_link())
                )
            }
            point2 <- .subset2(link, "point2")
            if (!inherits(point2, "AsIs") || is.character(point2)) {
                point2 <- vec_as_location(
                    point2,
                    n = n,
                    names = labels,
                    arg = "point2",
                    call = quote(range_link())
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
    } else if (is_range_link(link)) {
        point1 <- .subset2(link, "point1")
        # for character, we always match the original data
        if (is.character(point1)) {
            point1 <- vec_as_location(
                point1,
                n = n,
                names = labels,
                arg = "point1",
                call = quote(range_link())
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
                call = quote(range_link())
            )
            point2 <- match(point2, .subset2(design, "index"))
        }
        link <- point1:point2
    }

    # always use integer, otherwise, will cause error when drawing
    # due to loss of precision, I don't know why, it should be integer already?
    vec_cast(link, integer())
}
