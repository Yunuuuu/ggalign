####################################################################
# add annotation into annotation list
#' Put plots horizontally or vertically
#'
#' `ggstack` is an alias of `stack_layout`.
#'
#' @param data A numeric or character vector, a data frame, or a matrix.
#' @param direction A string of `"horizontal"` or `"vertical"`, indicates the
#' direction of the stack layout.
#' @param ... Additional arguments passed to [`fortify_stack()`].
#' @param sizes A numeric or a [unit][grid::unit] object of length `3` indicates
#' the relative widths (`direction = "horizontal"`) / heights (`direction =
#' "vertical"`).
#' @inheritParams heatmap_layout
#' @return A `StackLayout` object.
#' @examples
#' ggstack(matrix(rnorm(100L), nrow = 10L)) + align_dendro()
#' @export
stack_layout <- function(data = NULL, direction = NULL, ..., sizes = NA,
                         action = NULL, theme = NULL) {
    UseMethod("stack_layout")
}

# Used to place multiple objects in one axis
# usually the heatmap annotations
#' @importFrom grid unit
#' @importFrom ggplot2 waiver
#' @keywords internal
methods::setClass(
    "StackLayout",
    contains = "Layout",
    list(
        data = "ANY", plots = "list", direction = "character",
        heatmap = "list", # used by heatmap annotation
        sizes = "ANY", # used by stack layout
        panel = "ANY", index = "ANY", nobs = "ANY"
    ),
    prototype = list(
        plots = list(),
        heatmap = list( # used by heatmap annotation
            position = NULL, # annotation position
            size = unit(NA, "null"), # total annotation size
            free_guides = waiver()
        ),
        panel = NULL, index = NULL, nobs = NULL
    )
)

#' @export
#' @rdname stack_layout
ggstack <- stack_layout

#' @export
stack_layout.default <- function(data = NULL, direction = NULL, ..., sizes = NA,
                                 action = NULL, theme = NULL) {
    data <- fortify_stack(data = data, ...)
    .stack_layout(data,
        # if inherit from the parent layout data, we'll inherit
        # the action data function, but now, no parent layout
        action_data = if (is.null(data)) waiver() else NULL,
        direction = direction, sizes = sizes,
        action = action, theme = theme
    )
}

.stack_layout <- function(data = NULL, action_data = NULL,
                          direction = NULL, sizes = NA,
                          action = NULL, theme = NULL,
                          call = caller_call()) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    sizes <- check_stack_sizes(sizes, call = call)
    action <- check_action(action, data = action_data, call = call)
    if (is.null(data)) {
        nobs <- NULL
    } else {
        nobs <- NROW(data)
    }
    methods::new("StackLayout",
        data = data,
        direction = direction,
        theme = theme, action = action, # used by the layout
        # @param sizes the relative size of the vertical direction with this
        # stack, which won't be used by heatmap annotation.
        sizes = sizes, nobs = nobs
    )
}

#' Reports whether `x` is a `StackLayout` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is_ggstack(ggstack(1:10))
#' @export
is_ggstack <- function(x) methods::is(x, "StackLayout")
