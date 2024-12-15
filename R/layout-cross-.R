#' Arrange Plots Crosswise Horizontally or Vertically
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `cross_align` function aligns observations, and allow different layout
#' ordering index in a single layout. `cross_layout` is an alias for
#' `cross_align`.
#'
#' Two aliases are provided for convenience:
#' - `cross_alignv`: A special case of `cross_align` that sets `direction =
#'   "vertical"`.
#' - `cross_alignh`: A special case of `cross_align` that sets `direction =
#'   "horizontal"`.
#'
#' @inheritParams stack_align
#' @seealso [`ggcross()`]
#' @export
cross_align <- function(data = NULL, direction, ...,
                        theme = NULL, sizes = NA) {
    UseMethod("cross_align")
}

#' @usage NULL
#' @export
#' @rdname cross_align
cross_layout <- cross_align

#' @export
#' @rdname cross_align
cross_alignv <- function(data = NULL, ...) {
    cross_align(data = data, direction = "vertical", ...)
}

#' @export
#' @rdname cross_align
cross_alignh <- function(data = NULL, ...) {
    cross_align(data = data, direction = "horizontal", ...)
}

#' @include layout-stack-.R
methods::setClass(
    "CrossLayout",
    contains = "StackLayout",
    list(index_list = "list", cross_points = "integer"),
    prototype = list(index_list = list(), cross_points = integer())
)

#' @export
cross_align.default <- function(data = NULL, direction, ...) {
    ans <- stack_align(data = data, direction = direction, ...)
    ans <- methods::as(ans, "CrossLayout")
    ans@name <- "cross_align"
    ans
}

#' @importFrom grid unit.c
#' @importFrom rlang is_empty is_string
stack_build_composer.CrossLayout <- function(stack, schemes, theme,
                                             extra_coords) {
    # check if we should initialize the layout observations
    layout_coords <- stack@layout
    if (!is.null(layout_coords) &&
        is.null(.subset2(layout_coords, "nobs")) &&
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
    index_list <- c(stack@index_list, list(.subset2(layout_coords, "index")))

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
    previous_coords <- NULL
    for (i in seq_along(plot_list)) {
        plots <- .subset2(plot_list, i)
        # prepare coords for current group
        coords <- layout_coords
        coords["index"] <- list(.subset2(index_list, i))
        coords <- setup_layout_coords(coords)

        if (is_empty(plots)) {
            previous_coords <- coords
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
            previous_coords <- coords
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
            coords = coords,
            extra_coords = extra_coords,
            direction = direction,
            position = position,
            released_spaces = released_spaces,
            previous_coords = previous_coords
        )
        previous_coords <- coords
    }
    composer
}
