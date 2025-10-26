#' Arrange plots horizontally or vertically
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If `limits` is provided, a continuous variable will be required and aligned
#' in the direction specified (`stack_continuous`). Otherwise, a discrete
#' variable will be required and aligned (`stack_discrete`).
#'
#' Several aliases are provided for convenience:
#' - `stack_vertical`: A special case of `stack_layout` that sets `direction
#'   = "v"`.
#' - `stack_horizontal`: A special case of `stack_layout` that sets `direction
#'   = "h"`.
#' - `stack_discretev`: A special case of `stack_discrete` that sets `direction
#'   = "v"`.
#' - `stack_discreteh`: A special case of `stack_discrete` that sets `direction
#'   = "h"`.
#' - `stack_continuousv()`: A special case of `stack_free` that sets `direction
#'   = "v"`.
#' - `stack_continuoush()`: A special case of `stack_free` that sets `direction
#'   = "h"`.
#'
#' For historical reasons, the following aliases are available:
#' - `stack_align` is an alias for `stack_discrete`.
#' - `stack_alignv` is an alias for `stack_discretev`.
#' - `stack_alignh` is an alias for `stack_discreteh`.
#' - `stack_free` is an alias for `stack_continuous`.
#' - `stack_freev` is an alias for `stack_continuousv`.
#' - `stack_freeh` is an alias for `stack_continuoush`.
#'
#' @param direction A string indicating the direction of the stack layout,
#' either `"h"`(`horizontal`) or `"v"`(`vertical`).
#' @param data `r rd_layout_data()`:
#'  - If `limits` is not provided, [`fortify_matrix()`] will be used to get a
#'    matrix.
#'  - If `limits` is specified, [`fortify_data_frame()`] will be used to get a
#'    data frame.
#'
#' @param ... Additional arguments passed to [`fortify_data_frame()`] or
#' [`fortify_matrix()`].
#' @param theme A [`theme()`][ggplot2::theme] object used to customize various
#' elements of the layout, including `guides`, `title`, `subtitle`, `caption`,
#' `margins`, `panel.border`, and `background`. By default, the theme will
#' inherit from the parent `layout`. It also controls the panel spacing for all
#' plots in the layout.
#'
#' @param sizes A numeric value or a [`unit`][grid::unit] object. When used for
#' the [`quad_layout()`] annotation, it must be of length `1`. When used in the
#' [`stack_layout()`] with a nested [`quad_layout()`], it should be of length
#' `3`, specifying the relative heights (for `direction = "h"`) or widths (for
#' `direction = "v"`) to be applied to the layout.
#' @param limits A [`continuous_limits()`] object specifying the left/lower
#' limit and the right/upper limit of the scale. Used to align the continuous
#' axis.
#' @return A `StackLayout` object.
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7L)
#'
#' stack_horizontal(small_mat) + align_dendro()
#'
#' # this is the same with:
#' stack_discrete("h", small_mat) + align_dendro()
#'
#' stack_discreteh(small_mat) + align_dendro()
#'
#' # For vertical layout:
#' stack_vertical(small_mat) + align_dendro()
#'
#' @export
stack_layout <- function(direction, data = NULL, ...,
                         theme = NULL, sizes = NA, limits = waiver()) {
    if (is_waiver(limits)) {
        stack_discrete(
            data = data, direction = direction, ...,
            theme = theme, sizes = sizes
        )
    } else {
        stack_continuous(
            data = data, direction = direction, ...,
            theme = theme, sizes = sizes, limits = limits
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

###################################################################
#' @export
#' @rdname stack_layout
stack_discrete <- function(direction, data = NULL, ...,
                           theme = NULL, sizes = NA) {
    UseMethod("stack_discrete", data)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_align <- stack_discrete

#' @export
#' @rdname stack_layout
stack_discretev <- function(data = NULL, ...) {
    stack_discrete(data = data, direction = "v", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_alignv <- stack_discretev

#' @export
#' @rdname stack_layout
stack_discreteh <- function(data = NULL, ...) {
    stack_discrete(data = data, direction = "h", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
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

        # for data has dimention but one dimention is 0
        # as.matrix(data.frame(row.names = letters))
        if (nobs == 0L) {
            cli_abort("empty data is no allowed")
        }
    } else {
        nobs <- NA_integer_
    }
    new_stack_layout(
        name = "stack_discrete",
        data = data, direction = direction,
        domain = DiscreteDomain(nobs = nobs),
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
#' @export
#' @rdname stack_layout
stack_continuous <- function(direction, data = NULL, ..., limits = NULL,
                             theme = NULL, sizes = NA) {
    UseMethod("stack_continuous", data)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_free <- stack_continuous

#' @export
#' @rdname stack_layout
stack_continuousv <- function(data = NULL, ...) {
    stack_continuous(data = data, direction = "v", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_freev <- stack_continuousv

#' @export
#' @rdname stack_layout
stack_continuoush <- function(data = NULL, ...) {
    stack_continuous(data = data, direction = "h", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
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
        data = data, direction = direction, domain = limits,
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

#' @importFrom ggplot2 theme
new_stack_layout <- function(data, direction, domain,
                             schemes = NULL, theme = NULL, sizes = NA,
                             name = NULL, call = caller_call()) {
    if (is.null(name)) {
        if (is_discrete_domain(domain)) {
            name <- "stack_discrete"
        } else {
            name <- "stack_continuous"
        }
    }
    StackLayout(
        name = name, data = data,
        direction = direction,
        theme = theme %||% theme(), schemes = schemes, # used by the layout
        sizes = sizes, domain = domain
    )
}

#' @importFrom ggplot2 waiver
#' @importFrom grid is.unit
#' @importFrom S7 convert
#' @importFrom rlang is_atomic
#' @keywords internal
#' @include layout-chain-.R
StackLayout <- S7::new_class(
    "StackLayout", ChainLayout,
    properties = list(
        sizes = prop_grid_unit("sizes", validator = validator_size(3L)),
        # used by heatmap annotation
        heatmap = S7::new_property(
            S7::class_list,
            default = quote(list(
                position = NULL,
                free_guides = waiver(),
                # indicate whether or not the data is from the quad-layout
                # matrix
                quad_matrix = FALSE
            ))
        ),
        domain = S7::new_union(NULL, Domain)
    )
)

StackCross <- S7::new_class(
    "StackCross", StackLayout,
    # A list of old domain
    properties = list(
        odomain = S7::class_list,
        cross_points = S7::class_integer,
        break_points = S7::class_integer
    )
)
