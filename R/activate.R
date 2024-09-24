#' Determine the active context of heatmap layout
#'
#' @param position Which heatmap annotation should get activated? Possible
#' values are follows:
#'    * A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#'    * `NULL`: means set the active context into the `heatmap` itself.
#' @param size A [unit][grid::unit] object to set the total size of the heatmap
#' annotation. This will only be used if `position` is a string of
#' `r rd_values(.TLBR, final = "or")`.
#'  - If position is `"top"` or `"bottom"`, `size` set the total height of the
#' annotation.
#'  - If position is `"left"` or `"right"`, `size` set the total width of the
#' annotation.
#' @param guides A boolean value or a string containing one or more of
#' `r rd_values(.tlbr)` indicates which guide should be collected. If `NULL`, no
#' guides will be collected. Default: `"tlbr"`.
#' @param free_labs A boolean value or a string containing one or more of
#' `r rd_values(.tlbr)` indicates which axis title should be free from
#' alignment. If `NULL`, all axis title will be aligned. Default: `"tlbr"`.
#' @param free_spaces A boolean value or a string containing one or more of
#' `r rd_values(.tlbr)` indicates which border spaces should be removed. If
#' `NULL` (default), no space will be removed.
#' @param plot_data A function used to transform the plot data before rendering.
#' By default, it'll inherit from the parent layout. If no parent layout, the
#' default is `NULL`, which means we won't want to modify anything.
#'
#' Used to modify the data after layout has been created, but before the data is
#' handled of to the ggplot2 for rendering. Use this hook if the you needs
#' change the default data for all `geoms`.
#' @param theme A [theme()][ggplot2::theme] object, which will be added to all
#' plots in the layout. Use `NULL` if you don't want to any theme components to
#' be added. Use [waiver()][ggplot2::waiver()], if you want to inherit from the
#' parent layout.
#' @param what What should get activated for the anntoation stack? Only used
#' when position is not `NULL`. See [stack_active] for details.
#' @inheritParams heatmap_layout
#' @return A `heatmap_active` object which can be added into [heatmap_layout].
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     align_dendro()
#' @export
hmanno <- function(position = NULL, size = NULL,
                   guides = NA, free_labs = NA, free_spaces = NA,
                   plot_data = NA, theme = NA, what = waiver(),
                   width = NULL, height = NULL) {
    if (!is.null(position)) position <- match.arg(position, .TLBR)
    if (!is.null(size)) size <- check_size(size)
    if (!is.null(width)) width <- check_size(width)
    if (!is.null(height)) height <- check_size(height)
    if (!is.waive(what)) what <- check_stack_context(what)
    active <- new_active(
        guides = guides,
        free_labs = free_labs,
        free_spaces = free_spaces,
        plot_data = plot_data,
        theme = theme
    )
    structure(
        list(
            position = position, size = size, width = width, height = height,
            what = what, active = active
        ),
        class = "heatmap_active"
    )
}

#' Determine the active context of stack layout
#'
#' @inheritParams hmanno
#' @inheritParams stack_layout
#' @param what What should get activated for the stack layout? Possible values
#' are follows:
#'    * A single number or string of the plot elements in the stack layout.
#'      Usually you are waive to use this, since the adding procedure can be
#'      easily changed.
#'    * `NULL`: Remove any active context, this is useful when the active
#'      context is a [heatmap_layout()] object, where any `Align` objects will
#'      be added into the heatmap. By removing the active context, we can add
#'      `Align` object into the [stack_layout()] .
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
stack_active <- function(guides = NA, free_labs = NA, free_spaces = NA,
                         plot_data = NA, theme = NA, what = NULL,
                         sizes = NULL) {
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    active <- new_active(
        guides = guides,
        free_labs = free_labs,
        free_spaces = free_spaces,
        plot_data = plot_data,
        theme = theme
    )
    structure(
        list(what = what, sizes = sizes, active = active),
        class = "stack_active"
    )
}

new_active <- function(guides, free_labs, free_spaces, plot_data, theme,
                       call = caller_call()) {
    if (!identical(guides, NA) && !is.waive(guides)) {
        guides <- check_layout_position(guides, call = call)
    }
    if (!identical(free_labs, NA) && !is.waive(free_labs)) {
        free_labs <- check_layout_position(free_labs, call = call)
    }
    if (!identical(free_spaces, NA) && !is.waive(free_spaces) &&
        !is.null(free_spaces)) {
        free_spaces <- check_layout_position(free_spaces, call = call)
    }
    if (!identical(plot_data, NA) && !is.waive(plot_data)) {
        plot_data <- check_plot_data(plot_data, call = call)
    }
    if (!identical(theme, NA) && !is.waive(theme) && !is.null(theme)) {
        assert_s3_class(theme, "theme", call = call)
    }
    structure(
        list(
            guides = guides,
            free_labs = free_labs,
            free_spaces = free_spaces,
            plot_data = plot_data,
            theme = theme
        ),
        class = "active"
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
