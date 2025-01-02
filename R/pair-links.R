#' Helper function to create pairs of observation groups
#'
#' @description
#' [`ggmark()`] and [`cross_link()`] allow users to add links between
#' observations. These functions help define the linked observations. The
#' selected pairs will either be linked together, or each group in the pair will
#' be linked separately to the same plot area.
#'
#' - `pair_links`: Helper function to create pairs of observation groups.
#' - `range_link`: Helper function to create a range of observations.
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of formulas, where each side
#'   of the formula should be an `integer` or `character` index of the original
#'   data, or a `range_link()` object defining the linked observations. Use
#'   `NULL` to indicate no link on that side. You can also combine these by
#'   wrapping them into a single `list()`. If only the left-hand side of the
#'   formula exists, you can input it directly. For integer indices, wrap them
#'   with [`I()`] to use the ordering from the layout. You can also use
#'   [`waiver()`] to inherit values from the other group.
#' @examples
#' x <- pair_links(
#'     # group on the left hand only
#'     1:2,
#'     c("a", "b"),
#'     range_link(1, 6),
#'     range_link("a", "b"),
#'     # group on the right hand only
#'     ~ 1:2,
#'     ~ c("a", "b"),
#'     ~ range_link(1, 6),
#'     # group on the both side
#'     range_link(1, 6) ~ c("a", "b"),
#'     # waiver() indicates the right hand is the same of the left hand
#'     range_link(1, 6) ~ waiver(),
#'     # the same for the left hand
#'     waiver() ~ 1:2,
#'     ~NULL # an empty link
#' )
#' x
#'
#' # we can modify it as usual list
#' x[[1]] <- NULL # remove the first link
#' x$a <- ~LETTERS
#' x
#'
#' # modify with a list
#' x[1:2] <- list(~ c("a", "b"), ~ range_link("a", "b"))
#' x
#' @export
pair_links <- function(...) {
    pairs <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
    new_pair_links(lapply(pairs, as_pair_link, x_arg = "..."))
}

new_pair_links <- function(x = list(), ..., class = character()) {
    new_vctr(x, ..., class = c(class, "ggalign_pair_links"))
}

#' @export
obj_print_data.ggalign_pair_links <- function(x, ...) {
    if (length(x) > 0L) {
        hand1 <- vapply(x, function(hand) {
            deparse_link(hand, ..., hand = "hand1")
        }, character(1L), USE.NAMES = FALSE)
        hand2 <- vapply(x, function(hand) {
            deparse_link(hand, ..., hand = "hand2")
        }, character(1L), USE.NAMES = FALSE)
        nms <- c("", paste0(names_or_index(x), ":  "))
        nms <- format(nms, justify = "right")
        empty <- character(length(hand2))
        empty[hand1 == "" & hand2 == ""] <- "  <empty>"
        empty <- format(c("", empty), justify = "left")
        hand1 <- format(c("hand1", hand1), justify = "right")
        hand2 <- format(c("hand2", hand2), justify = "left")
        content <- paste0("  ", nms, hand1, " ~ ", hand2, empty)
        cat("", content, "", sep = "\n")
    }
    invisible(x)
}

#' @export
obj_print_footer.ggalign_pair_links <- function(x, ...) {
    NextMethod()
    n <- sum(lengths(x, use.names = FALSE))
    cat(sprintf(
        "A total of %d group%s", n,
        if (n > 1L) "s" else ""
    ), sep = "\n")
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
new_pair_link <- function(hand1 = NULL, hand2 = NULL,
                          ..., class = character()) {
    structure(
        .Data = list(hand1 = hand1, hand2 = hand2),
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
            sprintf("  hand1: %s", deparse_link(.subset2(x, "hand1"), ...)),
            sprintf("  hand2: %s", deparse_link(.subset2(x, "hand2"), ...))
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
    vec_ptype2(x, remove_class(y, "AsIs"))
}

#' @export
vec_ptype2.AsIs.ggalign_pair_link <- function(x, y, ...) {
    vec_ptype2(remove_class(x, "AsIs"), y)
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
    I(vec_cast(remove_class(x, "AsIs"), to, x_arg = x_arg, call = call))
}

#' @export
vec_cast.ggalign_pair_link.formula <- function(x, to, ...,
                                               x_arg = caller_arg(x),
                                               to_arg = "",
                                               call = caller_env()) {
    hand1 <- rlang::eval_tidy(rlang::f_lhs(x), env = rlang::f_env(x))
    hand1 <- as_obs_link(hand1, arg = x_arg, call = call)
    hand2 <- rlang::eval_tidy(rlang::f_rhs(x), env = rlang::f_env(x))
    hand2 <- as_obs_link(hand2, arg = x_arg, call = call)
    new_pair_link(hand1, hand2)
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
    I(as_obs_link(remove_class(x, "AsIs")))
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
deparse_link <- function(x, ...) deparse_link2(x, ...) %||% ""

#' @return A single string or `NULL`
#' @noRd
deparse_link2 <- function(x, ...) UseMethod("deparse_link2")

# Basic object
#' @export
deparse_link2.integer <- function(x, trunc = 3L, head = trunc - 1L,
                                  tail = 1L, ...) {
    l <- length(x)
    ans <- paste(
        deparse(x, control = c("keepNA", "niceNames", "showAttributes")),
        collapse = " "
    )
    if (l > trunc && startsWith(ans, "c")) {
        ans <- sprintf("c(%s)", paste(c(
            x[seq_len(head)], "...", x[seq.int(l - tail + 1L, l)]
        ), collapse = ", "))
    }
    ans
}

#' @export
deparse_link2.character <- function(x, trunc = 3L, head = trunc - 1L,
                                    tail = 1L, ...) {
    l <- length(x)
    if (l <= trunc) {
        ans <- paste(deparse(x), collapse = " ")
    } else {
        ans <- sprintf("c(%s)", paste(c(
            x[seq_len(head)], "...", x[seq.int(l - tail + 1L, l)]
        ), collapse = ", "))
    }
    ans
}

#' @export
deparse_link2.waiver <- function(x, ...) "waiver()"

#' @export
deparse_link2.NULL <- function(x, ...) NULL

# To allow `I()` to be used to the whole formula, we must define the method for
# this, though `ggalign_pair_link` shouldn't be considered as an observation
#' @export
deparse_link2.ggalign_pair_link <- function(x, ..., hand) {
    deparse_link2(.subset2(x, hand), ...)
}

#' @export
deparse_link2.AsIs <- function(x, ...) {
    ans <- NextMethod()
    if (!is.null(ans)) ans <- sprintf("I(%s)", ans)
    ans
}

# Recurse version
#' @export
deparse_link2.ggalign_range_link <- function(x, ...) {
    sprintf(
        "range_link(%s, %s)",
        deparse_link(.subset2(x, "point1"), ...),
        deparse_link(.subset2(x, "point2"), ...)
    )
}

#' @export
deparse_link2.list <- function(x, trunc = 3L, head = trunc - 1L,
                               tail = 1L, ...) {
    l <- length(x)
    if (l <= trunc) {
        ans <- vapply(x, deparse_link, character(1L), ...,
            trunc = trunc, head = head, tail = tail,
            USE.NAMES = FALSE
        )
    } else {
        ans <- c(
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
    sprintf("list(%s)", paste(ans, collapse = ", "))
}

###################################################
make_pair_link_data <- function(pair_link, design1, design2,
                                labels1, labels2,
                                call = caller_call()) {
    hand1 <- .subset2(pair_link, 1L)
    hand2 <- .subset2(pair_link, 2L)

    # make the data
    hand1 <- make_link_data(hand1,
        design = design1, labels = labels1,
        other = hand2, data_index = !inherits(pair_link, "AsIs")
    )
    hand2 <- make_link_data(hand2,
        design = design2, labels = labels2,
        other = hand1, data_index = !inherits(pair_link, "AsIs")
    )
    if (is.null(hand1) && is.null(hand2)) {
        return(NULL)
    }
    list(hand1 = hand1, hand2 = hand2)
}

make_link_data <- function(link, design, labels, other, data_index,
                           arg = caller_arg(link), call = caller_call()) {
    link <- link_to_location(
        link,
        n = .subset2(design, "nobs"),
        labels = labels,
        index = .subset2(design, "index"),
        other = other,
        data_index = data_index,
        arg = arg, call = call
    )
    if (is_empty(link)) {
        return(NULL)
    }
    # always use integer, otherwise, will cause error when drawing
    # due to loss of precision, I don't know why, it should be integer already?
    vec_unique(vec_cast(link, integer()))
}

link_to_location <- function(x, ...) UseMethod("link_to_location")

#' @export
link_to_location.AsIs <- function(x, ..., data_index) {
    link_to_location(remove_class(x, "AsIs"), ..., data_index = FALSE)
}

#' @export
link_to_location.character <- function(x, ..., n, labels, index,
                                       arg = caller_arg(x),
                                       call = caller_call()) {
    ans <- vec_as_location(
        x,
        n = n,
        names = labels,
        arg = arg,
        call = call
    )
    match(ans, index) # character always match the original data
}

#' @export
link_to_location.integer <- function(x, ..., n, labels, index, data_index,
                                     arg = caller_arg(x),
                                     call = caller_call()) {
    ans <- vec_as_location(
        x,
        n = n,
        names = labels,
        arg = arg,
        call = call
    )
    # integer index by default match the original data
    if (isTRUE(data_index)) match(ans, index) else ans
}

#' @export
link_to_location.ggalign_range_link <- function(x, ..., arg = caller_arg(x),
                                                call = caller_call()) {
    point1 <- link_to_location(
        .subset2(x, "point1"),
        ...,
        arg = "point1",
        call = quote(range_link())
    )
    point2 <- link_to_location(
        .subset2(x, "point2"),
        ...,
        arg = "point2",
        call = quote(range_link())
    )
    point1:point2
}

#' @export
link_to_location.list <- function(x, ...) {
    unlist(lapply(x, link_to_location, ...), FALSE, FALSE)
}

#' @export
link_to_location.waiver <- function(x, ..., other) {
    link_to_location(other %|w|% NULL, ...)
}

#' @export
link_to_location.NULL <- function(x, ...) NULL
