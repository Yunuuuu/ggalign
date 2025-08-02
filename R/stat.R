#' Get the statistics from the layout
#'
#' @param x A `r rd_layout()`.
#' @inheritParams rlang::args_dots_used
#' @return The statistics
#' @export
ggalign_stat <- function(x, ...) {
    UseMethod("ggalign_stat")
}

#' @param position A string of `r oxford_or(.TLBR)`.
#' @export
#' @rdname ggalign_stat
ggalign_stat.QuadLayout <- function(x, position, ...) {
    ggalign_stat(x = slot(x, position), ...)
}

#' @param what A single number or string of the plot elements in the stack
#' layout.
#' @export
#' @rdname ggalign_stat
ggalign_stat.StackLayout <- function(x, what, ...) {
    plot_list <- x@plot_list
    index <- vec_as_location2(
        what,
        n = length(plot_list),
        names = names(plot_list),
        missing = "error"
    )
    ggalign_stat(x = .subset2(plot_list, index), ...)
}

#' @export
`ggalign_stat.ggalign::CraftBox` <- function(x, ...) {
    ggalign_stat(prop(x, "craftsman"), ...)
}

#' @export
ggalign_stat.CraftAlign <- function(x, ...) {
    rlang::check_dots_empty()
    .subset2(x, "statistics")
}

#' @export
ggalign_stat.default <- function(x, ...) {
    cli_abort(sprintf("no statistics found for %s", object_name(x)))
}
