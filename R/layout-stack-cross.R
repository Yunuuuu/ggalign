#' Arrange Plots Crosswise Horizontally or Vertically
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `stack_cross` function is derived from `stack_discrete` and allows for
#' different layout ordering indices within a single layout.
#'
#' Two aliases are provided for convenience:
#' - `stack_crossv`: A special case of `stack_cross` that sets
#'   `direction = "v"` for vertical alignment.
#' - `stack_crossh`: A special case of `stack_cross` that sets
#'   `direction = "h"` for horizontal alignment.
#'
#' @inheritParams stack_discrete
#' @seealso [`ggcross()`]
#' @export
stack_cross <- function(direction, data = NULL, ...,
                        theme = NULL, sizes = NA) {
    UseMethod("stack_cross", data)
}

#' @export
#' @rdname stack_cross
stack_crossv <- function(data = NULL, ...) {
    stack_cross(data = data, direction = "v", ...)
}

#' @export
#' @rdname stack_cross
stack_crossh <- function(data = NULL, ...) {
    stack_cross(data = data, direction = "h", ...)
}

#' @include layout-stack-.R
methods::setClass(
    "StackCross",
    contains = "StackLayout",
    list(index_list = "list", cross_points = "integer"),
    prototype = list(index_list = list(), cross_points = integer())
)

#' @export
stack_cross.default <- function(direction, data = NULL, ...) {
    ans <- stack_discrete(data = data, direction = direction, ...)
    ans <- methods::as(ans, "StackCross")
    ans@name <- "stack_cross"
    ans
}

#' @importFrom grid unit.c
#' @importFrom rlang is_empty is_string
stack_build_composer.StackCross <- function(stack, schemes, theme,
                                             extra_design) {
    # check if we should initialize the layout observations
    layout_design <- stack@design
    if (is_discrete_design(layout_design) &&
        is.null(.subset2(layout_design, "nobs")) &&
        any(vapply(plot_list, is_cross_plot, logical(1L), USE.NAMES = FALSE))) {
        cli_abort(sprintf(
            "You must initialize the layout observations to plot the %s",
            object_name(stack)
        ))
    }
    plot_list <- stack@plot_list

    direction <- stack@direction
    position <- .subset2(stack@heatmap, "position")
    plot_list <- vec_chop(
        plot_list,
        sizes = diff(c(0L, stack@cross_points, length(plot_list)))
    )
    index_list <- c(stack@index_list, list(.subset2(layout_design, "index")))

    # build the stack
    composer <- stack_composer(direction)

    # for `free_spaces`, if we have applied it in the whole stack layout
    # we shouln't use it for a single plot. Otherwise, the guide legends
    # collected by the layout will overlap with the axis of the plot in the
    # layout.
    #
    # this occurs in the annotation stack (`position` is not `NULL`).
    stack_spaces <- .subset2(.subset2(schemes, "scheme_align"), "free_spaces")
    stack_spaces <- .subset2(.subset2(schemes, "scheme_align"), "free_spaces")
    if (is_string(stack_spaces) && !is.null(position)) {
        released_spaces <- stack_spaces
    } else {
        released_spaces <- NULL
    }
    previous_design <- NULL
    for (i in seq_along(plot_list)) {
        plots <- .subset2(plot_list, i)
        # prepare design for current group
        design <- layout_design
        design["index"] <- list(.subset2(index_list, i))
        design <- setup_design(design)

        if (is_empty(plots)) {
            previous_design <- design
            next
        }

        # we remove the plot without actual plot area
        keep <- vapply(plots, function(plot) {
            # we remove objects without plot area
            # Now, only `ggalign_plot` will contain `NULL`
            !is_ggalign_plot(plot) || !is.null(plot@plot)
        }, logical(1L), USE.NAMES = FALSE)
        plots <- .subset(plots, keep)

        if (is_empty(plots)) {
            previous_design <- design
            next
        }

        # we reorder the plots based on the `order` slot
        plot_order <- vapply(plots, function(plot) {
            # always keep cross() in the start
            if (is_cross_plot(plot)) {
                1L
            } else if (is_ggalign_plot(plot)) {
                .subset2(plot@active, "order")
            } else {
                .subset2(plot@plot_active, "order")
            }
        }, integer(1L), USE.NAMES = FALSE)
        plots <- .subset(plots, make_order(plot_order))
        composer <- stack_composer_add(
            plots,
            composer,
            schemes = schemes,
            theme = theme,
            design = design,
            extra_design = extra_design,
            direction = direction,
            position = position,
            released_spaces = released_spaces,
            previous_design = previous_design
        )
        previous_design <- design
    }
    composer
}
