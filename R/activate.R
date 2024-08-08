#' Determine the context of subsequent manipulations
#'
#' @param x A [ggheatmap][ggheatmap] object.
#' @param what What should get activated? Possible values are follows:
#'    * A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#'    * `NULL`: means set the active context into the `heatmap` itself.
#' @return A object with the same class of `x`, whose active context will be
#' `set`.
#' @export
activate <- function(x, what) UseMethod("activate")

#' @export
activate.HeatmapLayout <- function(x, what) {
    what <- match_context(what)
    set_context(x, what)
}

#' @export
activate.StackLayout <- function(x, what) {
    what <- check_stack_context(what)
    set_context(x, what)
}

#' Determine the active context of stack layout
#'
#' @param what What should get activated for the stack layout? Possible values
#' are follows:
#'    * A single number or string of the plot elements in the stack layout.
#'    * `NULL`: means set the active context into the last `Align` object. In
#'      this way, we can add other `Align` objects into the `StackLayout`.
#'
#' @inheritParams layout_stack
#' @return A `active` object which can be added into
#' [StackLayout][layout_stack].
#' @export
active <- function(what = NULL, sizes = NULL, guides = NULL,
                   align_axis_title = NULL, plot_data = NA) {
    what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    if (is.na(plot_data) && is_scalar(plot_data)) {
        plot_data <- NA
    } else {
        plot_data <- check_plot_data(plot_data)
    }
    structure(what,
        sizes = sizes,
        guides = guides,
        align_axis_title = align_axis_title,
        plot_data = plot_data,
        class = c("stack_active", "active")
    )
}

#' Determine the active context of heatmap layout
#'
#' @param position Which heatmap annotation should get activated? Possible
#' values are follows:
#'    * A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#'    * `NULL`: means set the active context into the `heatmap` itself.
#' @param size A [unit][grid::unit] object to set the total size of the heatmap
#' annotation. This will only be used if `what` is a string of `"top"`,
#' `"left"`, `"bottom"`, or `"right"`.
#'  - If position is `"top"` or `"bottom"`, `size` set the total height of the
#' annotation.
#'  - If position is `"left"` or `"right"`, `size` set the total width of the
#' annotation.
#' @inheritParams layout_heatmap
#' @inheritParams active
#' @return A `active` object which can be added into
#' [HeatmapLayout][layout_heatmap].
#' @export
hmanno <- function(position = NULL, size = NULL,
                   what = waiver(), width = NULL, height = NULL,
                   guides = NULL, align_axis_title = NULL, plot_data = NA) {
    if (is.null(position)) {
        position <- NA
    } else {
        position <- match.arg(position, GGHEAT_ELEMENTS)
    }
    if (!is.null(size)) size <- set_size(size)
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(width)) width <- set_size(width)
    if (!is.null(height)) height <- set_size(height)
    if (is.na(plot_data) && is_scalar(plot_data)) {
        plot_data <- NA
    } else {
        plot_data <- check_plot_data(plot_data)
    }
    structure(position,
        what = what, size = size, width = width, height = height,
        guides = guides, align_axis_title = align_axis_title,
        plot_data = plot_data,
        class = c("heatmap_active", "active")
    )
}

########################################################
match_context <- function(what) {
    if (!is.null(what)) what <- match.arg(what, GGHEAT_ELEMENTS)
    what
}

GGHEAT_ELEMENTS <- c("top", "left", "bottom", "right")

#' @keywords internal
set_context <- function(x, context) UseMethod("set_context")

#' @importFrom methods slot<-
#' @export
set_context.HeatmapLayout <- function(x, context) {
    slot(x, "active") <- context
    x
}

#' @importFrom methods slot<-
#' @export
set_context.StackLayout <- function(x, context) {
    if (is.na(context)) {
        context <- which(vapply(x@plots, is.align, logical(1L)))
        if (is_empty(context)) {
            context <- NULL
        } else {
            context <- max(context)
        }
    } else if ((l <- length(x@plots)) == 0L) {
        cli::cli_abort("No contexts in the stack layout to be activated")
    } else if (is.character(name <- context)) {
        context <- which(name == names(x@plots))
        if (length(context) == 0L) {
            cli::cli_abort("Cannot find {name} plot in this stack layout")
        }
    } else if (context > l) {
        cli::cli_abort(
            "Outlier context, the stack layout has only {l} context{?s}"
        )
    }
    slot(x, "active") <- context
    x
}

#' @keywords internal
get_context <- function(x) UseMethod("get_context")

#' @importFrom methods slot
#' @export
get_context.Layout <- function(x) slot(x, "active")
