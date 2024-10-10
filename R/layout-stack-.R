####################################################################
# add annotation into annotation list
#' Put plots horizontally or vertically
#'
#' `ggstack` is an alias of `stack_layout`.
#'
#' @param data A numeric or character vector, a data frame, or a matrix.
#' @param direction A string of `"horizontal"` or `"vertical"`, indicates the
#' direction of the stack layout.
#' @param sizes A numeric or [unit][grid::unit] object of length `3` indicates
#' the relative widths (`direction = "horizontal"`) / heights (`direction =
#' "vertical"`).
#' @inheritParams align_plots
#' @inheritParams rlang::args_dots_empty
#' @return A `StackLayout` object.
#' @examples
#' ggstack(matrix(rnorm(100L), nrow = 10L)) + align_dendro()
#' @export
stack_layout <- function(data, direction = NULL, sizes = NA, guides = waiver(),
                         ...) {
    rlang::check_dots_empty()
    if (missing(data)) {
        .stack_layout(
            data = NULL, nobs = NULL,
            direction = direction, sizes = sizes, guides = guides,
            call = current_call()
        )
    } else {
        UseMethod("stack_layout")
    }
}

# Used to place multiple objects in one axis
# usually the heatmap annotations
#' @keywords internal
#' @importFrom grid unit
methods::setClass(
    "StackLayout",
    contains = "Layout",
    list(
        data = "ANY",
        plots = "list",
        params = "list",
        direction = "character",
        position = "ANY", # used by heatmap layout
        panel = "ANY",
        index = "ANY",
        nobs = "ANY"
    ),
    prototype = list(position = NULL, panel = NULL, index = NULL, nobs = NULL)
)

#' @export
#' @rdname stack_layout
ggstack <- stack_layout

#' @export
stack_layout.matrix <- function(data, ...) {
    .stack_layout(
        data = data, nobs = nrow(data), ...,
        call = current_call()
    )
}

#' @export
stack_layout.data.frame <- stack_layout.matrix

#' @export
stack_layout.numeric <- function(data, ...) {
    .stack_layout(
        data = as.matrix(data), nobs = length(data), ...,
        call = current_call()
    )
}

#' @export
stack_layout.character <- stack_layout.numeric

#' @export
stack_layout.NULL <- function(data, ...) {
    .stack_layout(data = data, nobs = NULL, ..., call = current_call())
}

#' @importFrom rlang caller_call
#' @importFrom ggplot2 waiver
#' @importFrom grid unit
.stack_layout <- function(data, direction = NULL, sizes = NA, guides = waiver(),
                          nobs, call = caller_call()) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    sizes <- check_stack_sizes(sizes)
    if (!is.null(guides) && !is.waive(guides)) {
        assert_position(guides, call = call)
    }
    methods::new("StackLayout",
        data = data, 
        direction = direction,
        params = list(
            # @param sizes the relative size of the vertical direction with this
            # stack, which won't be used by heatmap annotation.
            sizes = sizes,
            guides = guides,
            # @param size the total size of the stack, used by heatmap
            # annotation.
            size = unit(NA, "null"),
            free_guides = waiver(), # only used by heatmap annotation
            plot_data = waiver(),
            free_labs = waiver(),
            free_spaces = waiver(),
            theme = waiver()
        ),
        nobs = nobs,
        # following parameters are used by ggsave
        theme = NULL
    )
}

#' @export
stack_layout.default <- function(data, ...) {
    cli::cli_abort(c(
        paste(
            "{.arg data} must be a numeric or character vector,",
            "a data frame, or a matrix."
        ),
        i = "You have provided {.obj_type_friendly {data}}"
    ))
}

#' Reports whether `x` is a `StackLayout` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is_ggstack(ggstack(1:10))
#' @export
is_ggstack <- function(x) methods::is(x, "StackLayout")
