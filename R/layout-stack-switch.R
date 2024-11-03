#########################################################
#' Determine the active context of stack layout
#'
#' `stack_active` is an alias for `stack_switch()`, which sets `what = NULL` by
#' default, with additional arguments for backward compatibility.
#'
#' @inheritParams quad_switch
#' @inheritParams stack_align
#' @param what What should get activated for the stack layout?
#' `r rd_stack_what()`, this is useful when the active context is a
#' [`quad_layout()`] object, where any `align_*()` will be added to the
#' [`quad_layout()`]. By removing the active context, we can add `align_*()`
#' into the [`stack_layout()`].
#' @return A `stack_active` object which can be added to [stack_layout()].
#' @examples
#' stack_align(matrix(1:9, nrow = 3L), "h") +
#'     ggheatmap() +
#'     # ggheamtap will set the active context, directing following addition
#'     # into the heatmap plot area. To remove the heatmap active context,
#'     # we can use `stack_active()` which will direct subsequent addition into
#'     # the stack
#'     stack_active() +
#'     # here we add a dendrogram to the stack.
#'     align_dendro()
#' @export
stack_switch <- function(sizes = NULL, what = waiver()) {
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    structure(list(what = what, sizes = sizes), class = "stack_switch")
}

#' @export
#' @rdname stack_switch
stack_active <- function(sizes = NULL, what = NULL,
                         ...,
                         # following parameters have replaced with `action`
                         # argument
                         guides = deprecated(),
                         free_spaces = deprecated(), plot_data = deprecated(),
                         theme = deprecated(), free_labs = deprecated()) {
    rlang::check_dots_empty()
    deprecate_action(
        "stack_active",
        plot_data = plot_data,
        theme = theme,
        free_spaces = free_spaces,
        free_labs = free_labs,
        guides = guides
    )
    if (!is.null(what)) {
        lifecycle::deprecate_warn(
            when = "0.0.5",
            what = "stack_active(what)",
            with = "stack_switch(what)",
            details = "Ability to change `what` will be dropped in next release."
        )
    } else {
        what <- NULL
    }
    ans <- stack_switch(sizes, what)
    ans
}
