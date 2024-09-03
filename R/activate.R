#' Determine the context of subsequent manipulations
#'
#' @param x A [layout_heatmap()] or [layout_stack()] object.
#' @param what What should get activated? See `position` of [hmanno()] or `what`
#' of [stack_active()].
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
#' @param width,height Heatmap body width/height, can be a [unit][grid::unit]
#' object. Only used when `position` is `NULL`.
#' @inheritParams plot_grid
#' @param free_labs A boolean value or a character of the axis position (`"t"`,
#' `"l"`, `"b"`, `"r"`) indicates which axis title should be free from
#' alignment. By default, all axis title won't be aligned.
#' @param free_spaces A character specifies the ggplot elements which won't
#' count space sizes when alignment. If `NULL`, no space will be removed. See
#' [free_space()] for allowed values.
#' @param plot_data A function used to transform the plot data before rendering.
#' By default, it'll inherit from the parent layout. If no parent layout, the
#' default is to not modify the data. Use `NULL`, if you don't want to modify
#' anything.
#'
#' Used to modify the data after layout has been created, but before the data is
#' handled of to the ggplot2 for rendering. Use this hook if the you needs
#' change the default data for all `geoms`.
#' @param theme `r rd_theme()` Only used when position is `NULL`.
#' @inheritParams stack_active
#' @param what What should get activated for the anntoation stack? See
#' [stack_active] for details.
#' @return A `active` object which can be added into
#' [HeatmapLayout][layout_heatmap].
#' @export
hmanno <- function(position = NULL, size = NULL, width = NULL, height = NULL,
                   guides = NA, free_labs = NA, free_spaces = NA,
                   plot_data = NA, theme = NULL, what = waiver()) {
    if (is.null(position)) {
        position <- NA
    } else {
        position <- match.arg(position, HEATMAP_ANNOTATION_POSITION)
    }
    if (!is.null(size)) size <- check_size(size)
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(width)) width <- check_size(width)
    if (!is.null(height)) height <- check_size(height)
    if (!identical(guides, NA) && !is.waive(guides)) {
        guides <- check_guides(guides)
    }
    if (!identical(free_labs, NA) && !is.waive(free_labs)) {
        free_labs <- check_layout_labs(free_labs)
    }
    if (!identical(free_spaces, NA) && !is.waive(free_spaces) &&
        !is.null(free_spaces)) {
        free_spaces <- check_ggelements(free_spaces)
    }
    if (!identical(plot_data, NA) && !is.waive(plot_data)) {
        plot_data <- check_plot_data(plot_data)
    }
    assert_s3_class(theme, "theme", null_ok = TRUE)
    structure(position,
        what = what, size = size, width = width, height = height,
        guides = guides, free_labs = free_labs, free_spaces = free_spaces,
        plot_data = plot_data, theme = theme,
        class = c("heatmap_active", "active")
    )
}

#' Determine the active context of stack layout
#'
#' @param sizes A numeric or [unit][grid::unit] object of length `3` indicates
#' the relative widths (`direction = "horizontal"`) / heights (`direction =
#' "vertical"`).
#' @inheritParams hmanno
#' @inheritParams plot_grid
#' @param what What should get activated for the stack layout? Possible values
#' are follows:
#'    * A single number or string of the plot elements in the stack layout.
#'      Usually you are waive to use this, since the adding procedure can be
#'      easily changed.
#'    * `NULL`: Remove any active context, this is useful when the active
#'      context is a [layout_heatmap()] object, where any `Align` objects will
#'      be added into the heatmap. By removing the active context, we can add
#'      `Align` object into the `StackLayout`.
#' @return A `active` object which can be added into
#' [StackLayout][layout_stack].
#' @export
stack_active <- function(sizes = NULL, guides = NA,
                         free_labs = NA, free_spaces = NA, plot_data = NA,
                         theme = NULL, what = NULL) {
    what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    if (!identical(guides, NA) && !is.waive(guides)) {
        guides <- check_guides(guides)
    }
    if (!identical(free_labs, NA) && !is.waive(free_labs)) {
        free_labs <- check_layout_labs(free_labs)
    }
    if (!identical(free_spaces, NA) && !is.waive(free_spaces) &&
        !is.null(free_spaces)) {
        free_spaces <- check_ggelements(free_spaces)
    }
    if (!identical(plot_data, NA) && !is.waive(plot_data)) {
        plot_data <- check_plot_data(plot_data)
    }
    assert_s3_class(theme, "theme", null_ok = TRUE)
    structure(what,
        sizes = sizes,
        guides = guides,
        free_labs = free_labs,
        free_spaces = free_spaces,
        plot_data = plot_data,
        theme = theme,
        class = c("stack_active", "active")
    )
}

########################################################
match_context <- function(what) {
    if (!is.null(what)) what <- match.arg(what, HEATMAP_ANNOTATION_POSITION)
    what
}

HEATMAP_ANNOTATION_POSITION <- c("top", "left", "bottom", "right")

#' @keywords internal
set_context <- function(x, context) UseMethod("set_context")

#' @export
set_context.HeatmapLayout <- function(x, context) {
    x@active <- context
    x
}

#' @export
set_context.StackLayout <- function(x, context) {
    if (is.na(context)) {
        context <- NULL
    } else if ((l <- length(x@plots)) == 0L) {
        cli::cli_abort("No contexts in the stack layout to be activated")
    } else if (is.character(name <- context)) {
        context <- which(name == names(x@plots))
        if (length(context) == 0L) {
            cli::cli_abort("Cannot find {name} plot in this stack layout")
        }
    } else if (context > l) {
        cli::cli_abort(paste(
            "Canno determine the context",
            "the stack layout has only {l} context{?s}",
            sep = ", "
        ))
    }
    x@active <- context
    x
}

#' @keywords internal
get_context <- function(x) UseMethod("get_context")

#' @export
get_context.Layout <- function(x) x@active
