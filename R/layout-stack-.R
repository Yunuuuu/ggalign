#' Arrange Plots Horizontally or Vertically by aligning discrete axis
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The `stack_discrete` function arranges plots by aligning discrete variables.
#' `stack_align` is an alias for `stack_discrete` for historical reasons.
#'
#' Several aliases are provided for convenience:
#' - `stack_discretev` and `stack_alignv`: A special case of `stack_discrete`
#'   that sets `direction = "vertical"`.
#' - `stack_discreteh` and `stack_alignh`: A special case of `stack_discrete`
#'   that sets `direction = "horizontal"`.
#'
#' @param direction A string indicating the direction of the stack layout,
#' either `"h"`(`horizontal`) or `"v"`(`vertical`).
#' @param data `r rd_layout_data()`, [`fortify_matrix()`] will be used to
#'    convert data to a matrix.
#' @param ... Additional arguments passed to [`fortify_matrix()`].
#' @param theme A [`theme()`][ggplot2::theme] object used to customize various
#' elements of the layout, including `guides`, `title`, `subtitle`, `caption`,
#' `margins`, `panel.border`, and `background`. By default, the theme will
#' inherit from the parent `layout`. It also controls the panel spacing for all
#' plots in the layout.
#'
#' @param sizes A numeric value or a [`unit`][grid::unit] object. When used for
#' the [`quad_layout()`] annotation, it must be of length `1`. When used in
#' the [`stack_layout()`] with a nested [`quad_layout()`], it should be of
#' length `3`, specifying the relative heights (for `direction = "horizontal"`) 
#' or widths (for `direction = "vertical"`) to be applied to the layout.
#'
#' @examples
#' set.seed(123)
#' stack_discrete("h", matrix(rnorm(56), nrow = 7L)) +
#'     align_dendro()
#' @export
stack_discrete <- function(direction, data = NULL, ...,
                           theme = NULL, sizes = NA) {
    UseMethod("stack_discrete", data)
}

#' @export
#' @rdname stack_discrete
stack_align <- stack_discrete

#' @export
#' @rdname stack_discrete
stack_discretev <- function(data = NULL, ...) {
    stack_discrete(data = data, direction = "v", ...)
}

#' @export
#' @rdname stack_discrete
stack_alignv <- stack_discretev

#' @export
#' @rdname stack_discrete
stack_discreteh <- function(data = NULL, ...) {
    stack_discrete(data = data, direction = "h", ...)
}

#' @export
#' @rdname stack_discrete
stack_alignh <- stack_discreteh

#' @export
stack_discrete.default <- function(direction, data = NULL, ...,
                                   theme = NULL, sizes = NA) {
    direction <- check_direction(direction)
    # the observations are rows, we use matrix to easily
    # reshape it into a long formated data frame for ggplot,
    # and we can easily determine the number of observations
    # from matrix
    data <- data %|w|% NULL
    data <- fortify_matrix(data = data, ...)
    schemes <- default_schemes()
    if (!is.null(data) && !is.function(data)) {
        # if we have provided data, we initialize the `nobs`
        nobs <- vec_size(data)
    } else {
        nobs <- NULL
    }
    new_stack_layout(
        name = "stack_discrete",
        data = data, direction = direction,
        design = discrete_design(nobs = nobs),
        schemes = schemes, theme = theme, sizes = sizes
    )
}

#' @export
stack_discrete.function <- function(direction, data = NULL, ...) {
    cli_abort(paste0(
        "{.arg data} must be a {.cls matrix}, ",
        "or an object coercible by {.fn fortify_matrix}, or a valid ",
        "{.cls matrix}-like object coercible by {.fn as.matrix}"
    ))
}

#' @export
stack_discrete.formula <- stack_discrete.function

################################################################
#' Arrange Plots Horizontally or Vertically by aligning continuous axis
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The `stack_continuous` function arranges plots by aligning continuous
#' variables. The alias `stack_free` is retained for historical reasons.
#'
#' - `stack_continuousv()`/`stack_freev`: A special case of `stack_free` that
#'   sets `direction = "vertical"`.
#' - `stack_continuoush()`/`stack_freeh`: A special case of `stack_free` that
#'   sets `direction = "horizontal"`.
#'
#' @param data `r rd_layout_data()`, [`fortify_data_frame()`] will be used to
#'    convert data to a data frame. Note that if the data is a `matrix`, it will
#'    be automatically converted to a long-formatted data frame, which differs
#'    from `ggplot2`'s behavior.
#' @param limits A [`continuous_limits()`] object specifying the left/lower
#' limit and the right/upper limit of the scale. Used to align the continuous
#' axis.
#' @inheritParams stack_discrete
#' @param ... Additional arguments passed to [`fortify_data_frame()`].
#' @export
stack_continuous <- function(direction, data = NULL, ..., limits = NULL,
                             theme = NULL, sizes = NA) {
    UseMethod("stack_continuous", data)
}

#' @export
#' @rdname stack_continuous
stack_free <- stack_continuous

#' @export
#' @rdname stack_continuous
stack_continuousv <- function(data = NULL, ...) {
    stack_continuous(data = data, direction = "v", ...)
}

#' @export
#' @rdname stack_continuous
stack_freev <- stack_continuousv

#' @export
#' @rdname stack_continuous
stack_continuoush <- function(data = NULL, ...) {
    stack_continuous(data = data, direction = "h", ...)
}

#' @export
#' @rdname stack_continuous
stack_freeh <- stack_continuoush

#' @export
stack_continuous.default <- function(direction, data = NULL, ...,
                                     limits = NULL, theme = NULL, sizes = NA) {
    assert_limits(limits)
    direction <- check_direction(direction)
    data <- data %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    schemes <- default_schemes()
    new_stack_layout(
        name = "stack_continuous",
        data = data, direction = direction, design = limits,
        schemes = schemes, theme = theme, sizes = sizes
    )
}

#' @export
stack_continuous.function <- function(direction, data = NULL, ...) {
    cli_abort(paste0(
        "{.arg data} must be a {.cls data.frame}, ",
        "or an object coercible by {.fn fortify_data_frame}, or a valid ",
        "{.cls data.frame}-like object coercible by {.fn as.data.frame}"
    ))
}

#' @export
stack_continuous.formula <- stack_continuous.function

#' @importFrom methods new
new_stack_layout <- function(data, direction, design,
                             schemes = NULL, theme = NULL, sizes = NA,
                             name = NULL, call = caller_call()) {
    sizes <- check_stack_sizes(sizes, call = call)
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    if (is.null(name)) {
        if (is_continuous_design(design)) {
            name <- "stack_continuous"
        } else {
            name <- "stack_discrete"
        }
    }
    new(
        "StackLayout",
        name = name, data = data,
        direction = direction,
        theme = theme, schemes = schemes, # used by the layout
        sizes = sizes, design = design
    )
}

#' Arrange plots horizontally or vertically
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function integrates the functionalities of `stack_discrete()` and
#' `stack_continuous()` into a single interface.
#'
#' @param data `r rd_layout_data()`:
#'  - If `limits` is not provided, [`fortify_matrix()`] will be used to get a
#'    matrix.
#'  - If `limits` is specified, [`fortify_data_frame()`] will be used to get a
#'    data frame. Note that if the data is a `matrix`, it will be automatically
#'    converted to a long-formatted data frame, which differs from `ggplot2`'s
#'    behavior.
#'
#' @inheritParams stack_continuous
#'
#' @return A `StackLayout` object.
#' @seealso
#'  - [`stack_discrete()`]
#'  - [`stack_continuous()`]
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7L)
#'
#' stack_horizontal(small_mat) + align_dendro()
#'
#' # this is the same with:
#' stack_discreteh(small_mat) + align_dendro()
#'
#' # For vertical layout:
#' stack_vertical(small_mat) + align_dendro()
#'
#' @export
stack_layout <- function(direction, data = NULL, ...,
                         limits = waiver()) {
    if (is.waive(limits)) {
        stack_discrete(data = data, direction = direction, ...)
    } else {
        stack_continuous(
            data = data, direction = direction,
            limits = limits, ...
        )
    }
}

#' @export
#' @rdname stack_layout
stack_horizontal <- function(data = NULL, ..., limits = waiver()) {
    stack_layout(data = data, direction = "h", limits = limits, ...)
}

#' @export
#' @rdname stack_layout
stack_vertical <- function(data = NULL, ..., limits = waiver()) {
    stack_layout(data = data, direction = "v", limits = limits, ...)
}

############################################################
# Used to place multiple objects in one axis
#' @importFrom grid unit
#' @importFrom ggplot2 waiver
#' @keywords internal
#' @include layout-chain.R
methods::setClass(
    "StackLayout",
    contains = "ChainLayout",
    list(
        direction = "character",
        heatmap = "list", # used by heatmap annotation
        sizes = "ANY" # used by stack layout
    ),
    prototype = list(heatmap = list(position = NULL, free_guides = waiver()))
)
