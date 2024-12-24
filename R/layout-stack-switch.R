#########################################################
#' Determine the active context of stack layout
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `stack_active` is an alias for `stack_switch()`, which sets `what = NULL` by
#' default.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quad_switch
#' @inheritParams stack_discrete
#' @param what What should get activated for the stack layout?
#' `r rd_chain_what()`, this is useful when the active context is a
#' [`quad_layout()`] object, where any `align_*()` will be added to the
#' [`quad_layout()`]. By removing the active context, we can add `align_*()`
#' into the [`stack_layout()`].
#' @return A `stack_switch` object which can be added to [stack_layout()].
#' @examples
#' stack_discrete("h", matrix(1:9, nrow = 3L)) +
#'     ggheatmap() +
#'     # ggheamtap will set the active context, directing following addition
#'     # into the heatmap plot area. To remove the heatmap active context,
#'     # we can use `stack_active()` which will direct subsequent addition into
#'     # the stack
#'     stack_active() +
#'     # here we add a dendrogram to the stack.
#'     align_dendro()
#' @export
stack_switch <- function(sizes = NULL, what = waiver(), ...) {
    rlang::check_dots_empty()
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    structure(list(what = what, sizes = sizes), class = "stack_switch")
}

#' @export
#' @rdname stack_switch
stack_active <- function(sizes = NULL, ...) {
    rlang::check_dots_empty()
    stack_switch(sizes, what = NULL)
}
