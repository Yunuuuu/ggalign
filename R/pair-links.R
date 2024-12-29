#' Helper function to create a pair of links
#'
#' - `pair_links`: Helper function to create pair of links.
#' - `range_link`: Helper function to create a range of observations.
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of formulas, where each side
#'   of the formula should be an `integer` or `character` index, or a
#'   `range_link()` object defining the linked observations. Use `NULL` to
#'   indicate no link on that side. You can also combine these by wrapping them
#'   into a single `list()`. If only the left-hand side of the formula exists,
#'   you can input it directly. For integer indices, wrap them with [`I()`] to
#'   preserve their order according to the layout. You can also use [`waiver()`]
#'   to inherit values from the opposite link argument.
#' @examples
#' x <- pair_links(
#'     1:2,
#'     c("a", "b"),
#'     range_link(1, 6) ~ c("a", "b"),
#'     ~ 1:2,
#'     ~letters,
#'     # waiver() indicates the right hand is the same of the left hand
#'     range_link(1, 6) ~ waiver(),
#'     ~NULL # an empty link
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
    new_pair_links(lapply(pairs, as_pair_link, x_arg = "..."))
}

new_pair_links <- function(x = list(), ..., class = character()) {
    ans <- new_vctr(x, ..., class = c(class, "ggalign_pair_links"))
    if (anyDuplicated(names(ans))) {
        cli_abort("Found duplicated names for pair of links")
    }
    ans
}

#' @export
names.ggalign_pair_links <- function(x) {
    nms <- rlang::names2(vec_data(x))
    no <- nms == ""
    nms[no] <- seq_along(x)[no]
    nms
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
        nms <- c("", paste0(names(x), ":  "))
        nms <- format(nms, justify = "right")
        empty <- character(length(link2))
        empty[link1 == "" & link2 == ""] <- "  <empty>"
        empty <- format(c("", empty), justify = "left")
        link1 <- format(c("link1", link1), justify = "right")
        link2 <- format(c("link2", link2), justify = "left")
        content <- paste0("  ", nms, link1, " ~ ", link2, empty)
        cat("", content, "", sep = "\n")
    }
    invisible(x)
}

#' @export
obj_print_footer.ggalign_pair_links <- function(x, ...) {
    NextMethod()
    n <- sum(lengths(x, use.names = FALSE))
    cat(sprintf("A total of %d link%s", n, if (n > 1L) "s" else ""), sep = "\n")
    invisible(x)
}

#' @export
`[<-.ggalign_pair_links` <- function(x, i, value) {
    value <- lapply(value, as_pair_link, x_arg = "value")
    NextMethod()
}

#' @export
`[[<-.ggalign_pair_links` <- function(x, i, value) {
    # let `NULL` to remove the link
    if (!is.null(value)) value <- as_pair_link(value)
    NextMethod()
}

#' @export
`$<-.ggalign_pair_links` <- function(x, i, value) {
    # let `NULL` to remove the link
    if (!is.null(value)) value <- as_pair_link(value)
    NextMethod()
}

#' @export
vec_ptype2.ggalign_pair_links.list <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.list.ggalign_pair_links <- function(x, y, ...) {
    y
}

#' @export
vec_cast.ggalign_pair_links.list <- function(x, to, ...,
                                             x_arg = caller_arg(x),
                                             to_arg = "",
                                             call = caller_env()) {
    new_pair_links(lapply(x, as_pair_link, x_arg = x_arg, call = call))
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

########################################################
new_pair_link <- function(link1 = NULL, link2 = NULL,
                          ..., class = character()) {
    structure(
        .Data = list(link1 = link1, link2 = link2),
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
            sprintf("  link1: %s", deparse_link(.subset2(x, "link1"), ...)),
            sprintf("  link2: %s", deparse_link(.subset2(x, "link2"), ...))
        )
        cat(content, sep = "\n")
    }
    invisible(x)
}

#' @param x A `ggalign_pair_link` object.
#' @noRd
#' @export
length.ggalign_pair_link <- function(x) {
    sum(!vapply(x, is.null, logical(1L), USE.NAMES = FALSE))
}

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
vec_ptype2.ggalign_pair_link.integer <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.integer.ggalign_pair_link <- function(x, y, ...) {
    y
}

#' @export
vec_ptype2.ggalign_pair_link.double <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.double.ggalign_pair_link <- function(x, y, ...) {
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
    cl <- oldClass(y)
    oldClass(y) <- cl[cl != "AsIs"]
    vec_ptype2(x, y)
}

#' @export
vec_ptype2.AsIs.ggalign_pair_link <- function(x, y, ...) {
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    vec_ptype2(x, y)
}

#' @export
vec_ptype2.ggalign_pair_link.list <- function(x, y, ...) {
    x
}

#' @export
vec_ptype2.list.ggalign_pair_link <- function(x, y, ...) {
    y
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
vec_cast.ggalign_pair_link.list <- vec_cast.ggalign_pair_link.numeric

#' @export
vec_cast.ggalign_pair_link.AsIs <- function(x, to, ...,
                                            x_arg = caller_arg(x),
                                            to_arg = "",
                                            call = caller_env()) {
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    I(vec_cast(x, to, x_arg = x_arg, call = call))
}

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

as_pair_link <- function(x, ...) {
    if (is.null(x)) { # vec_cast() cannot convert `NULL`
        new_pair_link()
    } else {
        vec_cast(x, to = new_pair_link(), ...)
    }
}

########################################################
as_obs_link <- function(x, ..., arg = caller_arg(x), call = caller_env()) {
    UseMethod("as_obs_link")
}

#' @export
as_obs_link.NULL <- function(x, ...) x

#' @export
as_obs_link.AsIs <- function(x, ...) {
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    I(as_obs_link(x))
}

#' @export
as_obs_link.numeric <- function(x, ..., arg = caller_arg(x),
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
as_obs_link.list <- function(x, ..., arg = caller_arg(x),
                             call = caller_env()) {
    x <- x[!vapply(x, is.null, logical(1L), USE.NAMES = FALSE)]
    if (is_empty(x)) return(NULL) # styler: off
    lapply(x, as_obs_link, arg = arg, call = call)
}

#' @export
as_obs_link.ggalign_range_link <- as_obs_link.NULL

#' @export
as_obs_link.default <- function(x, ..., arg = caller_arg(x),
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

###########################################################
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

#' @export
deparse_link.list <- function(x, trunc = 3L, head = trunc - 1L,
                              tail = 1L, ...) {
    l <- length(x)
    if (l <= trunc) {
        out <- vapply(x, deparse_link, character(1L), ...,
            trunc = trunc, head = head, tail = tail,
            USE.NAMES = FALSE
        )
    } else {
        out <- c(
            vapply(x[seq_len(head)],
                deparse_link, character(1L), ...,
                trunc = trunc, head = head, tail = tail,
                USE.NAMES = FALSE
            ),
            "...",
            vapply(x[seq.int(l - tail + 1L, l)],
                deparse_link, character(1L), ...,
                trunc = trunc, head = head, tail = tail,
                USE.NAMES = FALSE
            )
        )
    }
    sprintf("list(%s)", paste(out, collapse = ", "))
}

###################################################
make_pair_link_data <- function(pair_link, design1, design2,
                                labels1, labels2,
                                call = caller_call()) {
    input1 <- .subset2(pair_link, 1L)
    input2 <- .subset2(pair_link, 2L)

    # melt waiver, let waiver() inherit from the another link
    if (is.waive(input1) ||
        (is.list(input1) &&
            any(vapply(input1, is.waive, logical(1L), USE.NAMES = FALSE)))) {
        link1 <- c(input1, if (is.list(input2)) input2 else list(input2))
    } else {
        link1 <- input1
    }
    if (is.waive(input2) ||
        (is.list(input2) &&
            any(vapply(input2, is.waive, logical(1L), USE.NAMES = FALSE)))) {
        link2 <- c(input2, if (is.list(input1)) input1 else list(input1))
    } else {
        link2 <- input2
    }

    # make the data
    link1 <- make_link_data(link1, design = design1, labels = labels1)
    link2 <- make_link_data(link2, design = design2, labels = labels2)
    if (is.null(link1) && is.null(link2)) {
        return(NULL)
    }
    list(link1 = link1, link2 = link2)
}

make_link_data <- function(link, design, labels = NULL,
                           arg = caller_arg(link)) {
    if (is_empty(link)) {
        return(NULL)
    }
    link <- link_to_location(
        link,
        n = .subset2(design, "nobs"),
        names = labels,
        index = .subset2(design, "index"),
        arg = arg
    )
    if (is_empty(link)) {
        return(NULL)
    }
    # always use integer, otherwise, will cause error when drawing
    # due to loss of precision, I don't know why, it should be integer already?
    vec_cast(link, integer())
}

link_to_location <- function(x, ...) UseMethod("link_to_location")

#' @export
link_to_location.AsIs <- function(x, n, names = NULL, index = NULL, ...,
                                  arg = caller_arg(x),
                                  call = caller_call()) {
    cl <- oldClass(x)
    oldClass(x) <- cl[cl != "AsIs"]
    link_to_location(
        x,
        n = n,
        names = names,
        index = index,
        data_index = TRUE,
        arg = arg,
        call = call
    )
}

#' @export
link_to_location.character <- function(x, n, names = NULL, index = NULL, ...,
                                       arg = caller_arg(x),
                                       call = caller_call()) {
    ans <- vec_as_location(
        x,
        n = n,
        names = names,
        arg = arg,
        call = call
    )
    match(ans, index)
}

#' @export
link_to_location.integer <- function(x, n, names = NULL, index = NULL, ...,
                                     data_index = FALSE, arg = caller_arg(x),
                                     call = caller_call()) {
    ans <- vec_as_location(
        x,
        n = n,
        names = names,
        arg = arg,
        call = call
    )
    if (isTRUE(data_index)) ans else match(ans, index)
}

#' @export
link_to_location.ggalign_range_link <- function(x, n, names = NULL,
                                                index = NULL, ...,
                                                data_index = FALSE,
                                                arg = caller_arg(x),
                                                call = caller_call()) {
    point1 <- link_to_location(
        .subset2(x, "point1"),
        n = n,
        names = names,
        index = index,
        data_index = data_index,
        arg = "point1",
        call = quote(range_link())
    )
    point2 <- link_to_location(
        .subset2(x, "point2"),
        n = n,
        names = names,
        index = index,
        data_index = data_index,
        arg = "point2",
        call = quote(range_link())
    )
    point1:point2
}

#' @export
link_to_location.list <- function(x, n, names = NULL,
                                  index = NULL, ...,
                                  data_index = FALSE,
                                  arg = caller_arg(x),
                                  call = caller_call()) {
    ans <- lapply(
        x, link_to_location,
        n = n,
        names = names,
        index = index,
        data_index = data_index,
        arg = arg, call = call
    )
    unlist(ans, FALSE, FALSE)
}

#' @export
link_to_location.waiver <- function(x, n, names = NULL,
                                    index = NULL, ...) {
    integer()
}
