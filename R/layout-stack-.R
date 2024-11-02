#' Put plots horizontally or vertically
#'
#' This function integrates the functionalities of `stack_free()` and
#' `stack_align()` into a single interface. `ggstack` is an alias for
#' `stack_layout`.
#'
#' @param data `r rd_layout_data()`:
#'  - If `type` is `align`, [`fortify_data_frame()`] will be used to get a data
#'    frame.
#'  - If `type` is `free`, [`fortify_matrix()`] will be used to get a matrix.
#' @inheritParams stack_align
#' @param type A string indicating the stack layout type: `"align"` for
#'   aligned plots ([`stack_align()`]) or `"free"` for free stacking
#'   ([`stack_free()`]).
#' @return A `StackLayout` object.
#' @seealso
#'  - [`stack_align()`]
#'  - [`stack_free()`]
#' @examples
#' small_mat <- matrix(rnorm(56), nrow = 7L)
#' stack_layout(small_mat, "horizontal", "align") + align_dendro()
#'
#' # ggstack is an alias for `stack_layout`
#' ggstack(small_mat, "horizontal") + align_dendro()
#'
#' # this is the same with:
#' stack_align(small_mat, "h") + align_dendro()
#' @export
stack_layout <- function(data = NULL, direction = NULL, type = NULL, ...) {
    type <- match.arg(type, c("align", "free"))
    switch(type,
        align = stack_align(data = data, direction = direction, ...),
        free = stack_free(data = data, direction = direction, ...)
    )
}

#' @usage NULL
#' @export
#' @rdname stack_layout
ggstack <- stack_layout

#################################################################
#' Arrange Plots Horizontally or Vertically
#'
#' @description
#' The `stack_align` function aligns observations, while `stack_free` does not.
#'
#' Several aliases are provided for convenience:
#' - `stack_alignv`: A special case of `stack_align` that sets `direction =
#'   "vertical"`.
#' - `stack_alignh`: A special case of `stack_align` that sets `direction =
#'   "horizontal"`.
#' - `stack_freev`: A special case of `stack_free` that sets `direction =
#'   "vertical"`.
#' - `stack_freeh`: A special case of `stack_free` that sets `direction =
#'   "horizontal"`.
#'
#' @param data `r rd_layout_data()`:
#'  - For `stack_free`, [`fortify_data_frame()`] will be used to convert data to
#'    a data frame.
#'  - For `stack_align`, [`fortify_matrix()`] will be used to convert data to a
#'    matrix.
#' @param direction A string indicating the direction of the stack layout,
#' either `"horizontal"` or `"vertical"`.
#' @param ... Additional arguments passed to [`fortify_data_frame()`] or
#' [`fortify_matrix()`].
#' @inheritParams quad_layout
#' @param sizes A numeric or a [`unit`][grid::unit] object of length `3`
#' indicating the relative heights (for `direction = "horizontal"`) or widths
#' (for `direction = "vertical"`).
#' @examples
#' stack_align(matrix(rnorm(56), nrow = 7L), "h") +
#'     align_dendro()
#' @export
stack_align <- function(data = NULL, direction = NULL, ...,
                        action = NULL, theme = NULL, sizes = NA) {
    UseMethod("stack_align")
}

#' @export
#' @rdname stack_align
stack_alignv <- function(data = NULL, ...) {
    stack_align(data = data, direction = "vertical", ...)
}

#' @export
#' @rdname stack_align
stack_alignh <- function(data = NULL, ...) {
    stack_align(data = data, direction = "horizontal", ...)
}

#' @export
stack_align.default <- function(data = NULL, direction = NULL, ...,
                                action = NULL, theme = NULL, sizes = NA) {
    # the observations are rows, we use matrix to easily
    # reshape it into a long formated data frame for ggplot,
    # and we can easily determine the number of observations
    # from matrix
    data <- fortify_matrix(data = data, ...)

    # if inherit from the parent layout data, we'll inherit the action data
    # function.
    action <- check_action(action,
        data = if (is.waive(data)) waiver() else NULL
    )

    # `waiver()` is used for further extension, it indicates data will
    # inherit from the parent layout. Now, `waiver()` won't be used as a
    # child layout.
    if (!is.null(data) && !is.function(data) && !is.waive(data)) {
        # if we have provided data, we initialize the `nobs`
        nobs <- NROW(data)
    } else {
        nobs <- NULL
    }
    layout <- new_layout_params(nobs = nobs)
    new_stack_layout(
        name = "stack_align",
        data = data, direction = direction, layout = layout,
        action = action, theme = theme, sizes = sizes
    )
}

################################################################
#' @export
#' @rdname stack_align
stack_free <- function(data = NULL, direction = NULL, ...,
                       action = NULL, theme = NULL, sizes = NA) {
    UseMethod("stack_free")
}

#' @export
#' @rdname stack_align
stack_freev <- function(data = NULL, ...) {
    stack_free(data = data, direction = "vertical", ...)
}

#' @export
#' @rdname stack_align
stack_freeh <- function(data = NULL, ...) {
    stack_free(data = data, direction = "horizontal", ...)
}

#' @export
stack_free.default <- function(data = NULL, direction = NULL, ...,
                               action = NULL, theme = NULL, sizes = NA) {
    data <- fortify_data_frame(data = data, ...)
    # if inherit from the parent layout data, we'll inherit the action data
    # function.
    action <- check_action(action,
        data = if (is.waive(data)) waiver() else NULL
    )
    new_stack_layout(
        name = "stack_free",
        data = data, direction = direction, layout = NULL,
        action = action, theme = theme, sizes = sizes
    )
}

# Added to avoid `aes()` end in `stack_free.default`
#' @export
stack_free.uneval <- function(data, ...) {
    cli::cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn stack_free}"
    ))
}

new_stack_layout <- function(name, data, direction, layout,
                             action = NULL, theme = NULL, sizes = NA,
                             call = caller_call()) {
    sizes <- check_stack_sizes(sizes, call = call)
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    if (!is.null(direction)) {
        direction <- match.arg(direction, c("horizontal", "vertical"))
    } else {
        lifecycle::deprecate_warn(
            when = "0.0.5",
            what = sprintf("%s(direction = 'must be provided')", name),
            details = "no default value of 'direction' argument in the next release"
        )
        direction <- "horizontal"
    }
    methods::new("StackLayout",
        name = name, data = data, direction = direction,
        theme = theme, action = action, # used by the layout
        sizes = sizes, layout = layout
    )
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
        name = "character",
        data = "ANY", plots = "list", direction = "character",
        heatmap = "list", # used by heatmap annotation
        sizes = "ANY", # used by stack layout
        layout = "ANY" # used to align observations
    ),
    prototype = list(
        plots = list(),
        heatmap = list( # used by heatmap annotation
            position = NULL, # annotation position
            size = unit(NA, "null"), # total annotation size
            free_guides = waiver()
        )
    )
)
