#' Arrange plots crosswise horizontally or vertically
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `stack_cross` function is derived from [`stack_discrete()`] and allows
#' for different layout ordering indices within a single layout.
#'
#' Two aliases are provided for convenience:
#' - `stack_crossv`: A special case of `stack_cross` that sets
#'   `direction = "v"` for vertical alignment.
#' - `stack_crossh`: A special case of `stack_cross` that sets
#'   `direction = "h"` for horizontal alignment.
#'
#' @param data `r rd_layout_data()`, [`fortify_matrix()`] will be used to
#' convert the data to a matrix.
#' @param ... Additional arguments passed to [`fortify_matrix()`].
#' @inheritParams stack_layout
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

#' @include layout-chain-stack-.R
StackCross <- methods::setClass(
    "StackCross",
    contains = "StackLayout",
    # A list of old domain
    list(odomain = "list", cross_points = "integer", break_points = "integer"),
    prototype = list(
        odomain = list(),
        cross_points = integer(),
        break_points = integer()
    )
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
resolve_stack_layout.StackCross <- function(stack, schemes, theme,
                                            extra_domain) {
    # check if we should initialize the layout observations
    layout_domain <- stack@domain
    # styler: off
    if (is_discrete_domain(layout_domain) &&
        is.na(prop(layout_domain, "nobs")) &&
        any(vapply(plot_list, is_cross_craftbox, logical(1L),
                   USE.NAMES = FALSE))) {
        cli_abort(sprintf(
            "You must initialize the layout observations to plot the %s",
            object_name(stack)
        ))
    }
    # styler: on
    plot_list <- stack@plot_list

    direction <- stack@direction
    position <- .subset2(stack@heatmap, "position")
    plot_list <- vec_chop(
        plot_list,
        sizes = diff(c(0L, stack@cross_points, length(plot_list)))
    )
    domain_list <- c(stack@odomain, list(layout_domain))

    # build the stack
    composer <- stack_composer(direction)

    # for `free_spaces`, if we have applied it in the whole stack layout we
    # shouln't use it for a single plot. Otherwise, the guide legends collected
    # by the layout will overlap with the axis of the plot in the layout.
    #
    # this occurs in the annotation stack (`position` is not `NULL`).
    stack_spaces <- prop(schemes_get(schemes, "scheme_align"), "free_spaces")
    if (is_string(stack_spaces) && !is.null(position)) {
        released_spaces <- stack_spaces
    } else {
        released_spaces <- NULL
    }
    previous_domain <- NULL
    for (i in seq_along(plot_list)) {
        plots <- .subset2(plot_list, i)

        # prepare domain for current group
        domain <- domain_init(.subset2(domain_list, i))

        if (is_empty(plots)) {
            previous_domain <- domain
            next
        }

        # we remove the plot without actual plot area
        keep <- vapply(plots, function(plot) {
            # we remove objects without plot area
            # Now, only `CraftBox` will contain `NULL`
            !is_craftbox(plot) || !is.null(plot@plot)
        }, logical(1L), USE.NAMES = FALSE)
        plots <- .subset(plots, keep)

        if (is_empty(plots)) {
            previous_domain <- domain
            next
        }

        # we reorder the plots based on the `order` slot
        plot_order <- vapply(plots, function(plot) {
            # always keep cross() in the start
            if (is_cross_craftbox(plot)) {
                1L
            } else if (is_craftbox(plot)) {
                prop(plot@active, "order")
            } else {
                prop(plot@plot_active, "order")
            }
        }, integer(1L), USE.NAMES = FALSE)
        plots <- .subset(plots, make_order(plot_order))
        composer <- stack_composer_add(
            plots,
            stack = stack,
            composer,
            schemes = schemes,
            theme = theme,
            domain = domain,
            extra_domain = extra_domain,
            direction = direction,
            position = position,
            released_spaces = released_spaces,
            previous_domain = previous_domain
        )
        previous_domain <- domain
    }
    composer
}
