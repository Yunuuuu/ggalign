#' Determine the active context of heatmap layout
#'
#' @param position Which heatmap annotation should get activated? Possible
#' values are follows:
#'    * A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#'    * `NULL`: means set the active context into the `heatmap` itself.
#' @param size An [unit][grid::unit] object to set the total size of the heatmap
#' annotation. Only used if `position` is a string.
#'  - If `position` is `"top"` or `"bottom"`, `size` set the total height of the
#' annotation.
#'  - If `position` is `"left"` or `"right"`, `size` set the total width of the
#' annotation.
#' @inheritParams rlang::args_dots_empty
#' @inheritParams heatmap_layout
#' @param width,height `r rd_heatmap_size()`. Only used when `position` is
#' `NULL`.
#' @param free_guides Override the `guides` collection behavior specified in the
#' heatmap layout for the annotation stack layout. Only used when `position` is
#' a string.
#' @param what What should get activated for the anntoation stack? Only used
#' when `position` is a string. `r rd_stack_what()`.
#' @param guides `r lifecycle::badge("deprecated")` Please use `action` argument
#' instead.
#' @param free_spaces `r lifecycle::badge("deprecated")` Please use `action`
#' argument instead.
#' @param plot_data `r lifecycle::badge("deprecated")` Please use `action`
#' argument instead.
#' @param theme `r lifecycle::badge("deprecated")` Please use `action` argument
#' instead.
#' @param free_labs `r lifecycle::badge("deprecated")` Please use `action`
#' argument instead.
#' @return A `heatmap_active` object which can be added into [heatmap_layout].
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     align_dendro()
#' @importFrom lifecycle deprecated
#' @export
hmanno <- function(position = NULL, size = NULL, action = NULL,
                   width = NULL, height = NULL, free_guides = waiver(),
                   what = waiver(), ...,
                   # following parameters have replaced with `action` argument
                   guides = deprecated(),
                   free_spaces = deprecated(), plot_data = deprecated(),
                   theme = deprecated(), free_labs = deprecated()) {
    rlang::check_dots_empty()
    if (is.null(action)) {
        action <- plot_action()
    } else {
        assert_action(action)
    }
    if (!is.null(position)) position <- match.arg(position, .TLBR)
    if (!is.null(size)) size <- check_size(size)
    if (!is.null(width)) width <- check_size(width)
    if (!is.null(height)) height <- check_size(height)
    assert_layout_position(free_guides)
    if (!is.waive(what)) what <- check_stack_context(what)
    action <- deprecate_action(
        action, "hmanno", plot_data, theme,
        free_spaces, free_labs,
        guides = guides
    )
    structure(
        list(
            position = position, size = size, action = action,
            width = width, height = height, free_guides = free_guides,
            what = what
        ),
        class = "heatmap_active"
    )
}

#' Determine the active context of stack layout
#'
#' @inheritParams hmanno
#' @inheritParams stack_layout
#' @param what What should get activated for the stack layout?
#' `r rd_stack_what()`, this is useful when the active context is a
#' [heatmap_layout()] object, where any `align_*()` will be added into the
#' heatmap. By removing the active context, we can add `align_*()` into the
#' [stack_layout()].
#' @return A `stack_active` object which can be added into
#' [StackLayout][stack_layout].
#' @examples
#' ggstack(matrix(1:9, nrow = 3L)) +
#'     ggheatmap() +
#'     # ggheamtap will set the active context, directing following addition
#'     # into the heatmap plot area. To remove the heatmap active context,
#'     # we can use `stack_active()` which will direct subsequent addition into
#'     # the stack
#'     stack_active() +
#'     # here we add a dendrogram to the stack.
#'     align_dendro()
#' @export
stack_active <- function(action = NULL, sizes = NULL, what = NULL,
                         ...,
                         # following parameters have replaced with `action`
                         # argument
                         guides = deprecated(),
                         free_spaces = deprecated(), plot_data = deprecated(),
                         theme = deprecated(), free_labs = deprecated()) {
    rlang::check_dots_empty()
    if (is.null(action)) {
        action <- plot_action() # To-DO: Use `NULL` to indicates the default
    } else {
        assert_action(action)
    }
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    action <- deprecate_action(
        action, "stack_active", plot_data, theme,
        free_spaces, free_labs,
        guides = guides
    )
    structure(
        list(what = what, sizes = sizes, action = action),
        class = "stack_active"
    )
}

########################################################
match_context <- function(what) {
    if (!is.null(what)) what <- match.arg(what, .TLBR)
    what
}

#' @keywords internal
set_context <- function(x, context) UseMethod("set_context")

#' @export
set_context.HeatmapLayout <- function(x, context) {
    x@active <- context
    x
}

#' @export
set_context.StackLayout <- function(x, context) {
    if (is.null(context)) {
    } else if ((l <- length(x@plots)) == 0L) {
        cli::cli_abort("No contexts in the stack layout to be activated")
    } else if (is.character(name <- context)) {
        context <- match(name, names(x@plots))
        if (is.na(context)) {
            cli::cli_abort("Cannot find {name} plot in this stack layout")
        }
    } else if (context > l) {
        cli::cli_abort(c(
            "Cannot determine the context",
            i = paste(
                "the stack layout has only {l} context{?s} but you provided",
                "an integer index ({context})"
            )
        ))
    }
    x@active <- context
    x
}

#' @keywords internal
get_context <- function(x) UseMethod("get_context")

#' @export
get_context.Layout <- function(x) x@active
