#' Arrange Plots in the Quad-Side of a main plot by aligning discrete axis
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' The `quad_discrete` function arranges plots in the quad-side of a main plot
#' by aligning discrete variables along two axes. `quad_alignb` is an alias for
#' `quad_discrete` for historical reasons.
#'
#' @param data `r rd_layout_data()`. By default, it will try to inherit from
#' parent layout. [`fortify_matrix()`] will be used to convert data to a
#' matrix.
#' @param mapping Default list of aesthetic mappings to use for main plot in the
#' layout. If not specified, must be supplied in each layer added to the main
#' plot.
#' @param ... Additional arguments passed to [`fortify_matrix()`].
#' @param width,height The relative width/height of the main plot, can be a
#' [`unit`][grid::unit] object.
#' @inheritParams stack_discrete
#' @inheritParams align
#' @section ggplot2 specification:
#' The data in the underlying main plot contains following columns:
#'
#'  - `.xpanel` and `.ypanel`: the column and row panel
#'
#'  - `.x` and `.y`: an integer index of `x` and `y` coordinates
#'
#'  - `.discrete_x` and `.discrete_y`: a factor of the data labels (only
#'    applicable when `.row_names` and `.column_names` exists).
#'
#'  - `.row_names` and `.column_names`: A character of the row and column names
#'    of the original matrix (only applicable when names exist).
#'
#'  - `.row_index` and `.column_index`: the row and column index of the original
#'    matrix.
#'
#'  - `value`: the actual matrix value.
#'
#' @return A `QuadLayout` object.
#' @export
quad_discrete <- function(data = waiver(), mapping = aes(),
                          ...,
                          theme = NULL, active = NULL,
                          width = NA, height = NA) {
    UseMethod("quad_discrete")
}

#' @export
#' @rdname quad_discrete
quad_alignb <- quad_discrete

#' @export
quad_discrete.default <- function(data = waiver(), mapping = aes(),
                                  ...,
                                  theme = NULL, active = NULL,
                                  width = NA, height = NA) {
    data <- fortify_matrix(data = data, ...)
    new_quad_layout(
        name = "quad_discrete", data = data, xlim = waiver(), ylim = waiver(),
        mapping = mapping, active = active, theme = theme,
        width = width, height = height
    )
}

#' @export
quad_discrete.uneval <- function(data, ...) {
    cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn quad_discrete}"
    ))
}

#############################################################
#' Arrange Plots in the Quad-Side of a main plot by aligning continuous axis
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' These functions arrange plots around a main plot by aligning two continuous
#' axes. `ggside` is simply an alias for `quad_continuous`. `quad_free` is an
#' alias for `quad_continuous` for historical reasons.
#'
#' @param data `r rd_layout_data()`. By default, it will try to inherit from
#' parent layout. [`fortify_data_frame()`] will be used to convert data to a
#' data frame.
#' @param xlim,ylim A [`continuous_limits()`] object specifying the left/lower
#' limit and the right/upper limit of the scale. Used to align the continuous
#' axis.
#' @param ... Additional arguments passed to [`fortify_data_frame()`].
#' @inheritParams quad_discrete
#' @inherit quad_discrete return
#' @export
quad_continuous <- function(data = waiver(), mapping = aes(),
                            xlim = NULL, ylim = NULL,
                            ...,
                            theme = NULL, active = NULL,
                            width = NA, height = NA) {
    UseMethod("quad_continuous")
}

#' @usage NULL
#' @export
#' @rdname quad_continuous
ggside <- quad_continuous

#' @usage NULL
#' @export
#' @rdname quad_continuous
quad_free <- quad_continuous

#' @export
quad_continuous.default <- function(data = waiver(), mapping = aes(),
                                    xlim = NULL, ylim = NULL,
                                    ...,
                                    theme = NULL, active = NULL,
                                    width = NA, height = NA) {
    xlim <- xlim %|w|% NULL
    ylim <- ylim %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    new_quad_layout(
        name = "quad_continuous",
        data = data, xlim = xlim, ylim = ylim,
        mapping = mapping, active = active, theme = theme,
        width = width, height = height
    )
}

#' @export
quad_continuous.uneval <- function(data, ...) {
    cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn quad_free}"
    ))
}

##########################################################
#' Arrange Plots in the Quad-Side of a main plot
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' This function arranges plots around the quad-sides of a main plot, aligning
#' both horizontal and vertical axes, and can handle either discrete or
#' continuous variables.
#'
#' - If `xlim` is provided, a continuous variable will be required and aligned
#'   in the vertical direction. Otherwise, a discrete variable will be required
#'   and aligned.
#' - If `ylim` is provided, a continuous variable will be required and aligned
#'   in the horizontal direction. Otherwise, a discrete variable will be
#'   required and aligned.
#'
#' For historical reasons, the following aliases are available:
#' - `quad_alignh`: Align discrete variables in the horizontal direction and
#'   continuous variables in vertical direction.
#' - `quad_alignv`: Align discrete variables in the vertical direction and
#'   continuous variables in horizontal direction..
#'
#' @param data `r rd_layout_data()`. By default, this will attempt to inherit
#'   from the parent layout. If both `xlim` and `ylim` are provided, a `data
#'   frame` is required. When inherited from the annotation stack, no
#'   transposition will be applied. Otherwise, a `matrix` is required. When
#'   inherited from the column annotation stack, the data will be transposed.
#' @param ... Additional arguments passed to [`fortify_data_frame()`] or
#' [`fortify_matrix()`].
#' @inheritParams quad_continuous
#' @inherit quad_discrete return
#' @section ggplot2 specification:
#' If either `xlim` or `ylim` is not provided, the data will be converted to a
#' matrix using [`fortify_matrix()`], and the data in the underlying main plot
#' will contain the following columns:
#'
#'  - `.xpanel` and `.ypanel`: the column and row panel
#'
#'  - `.x` and `.y`: an integer index of `x` and `y` coordinates
#'
#'  - `.discrete_x` and `.discrete_y`: a factor of the data labels (only
#'    applicable when `.row_names` and `.column_names` exists).
#'
#'  - `.row_names` and `.column_names`: A character of the row and column names
#'    of the original matrix (only applicable when names exist).
#'
#'  - `.row_index` and `.column_index`: the row and column index of the original
#'    matrix.
#'
#'  - `value`: the actual matrix value.
#'
#' @export
quad_layout <- function(data = waiver(), mapping = aes(),
                        xlim = waiver(), ylim = waiver(),
                        ...,
                        theme = NULL, active = NULL,
                        width = NA, height = NA) {
    if (is.waive(xlim) && is.waive(ylim)) {
        quad_discrete(
            data = data, mapping = mapping,
            ..., active = active, theme = theme,
            width = width, height = height
        )
    } else if (!is.waive(xlim) && !is.waive(ylim)) {
        quad_continuous(
            data = data, mapping = mapping, xlim = xlim, ylim = ylim,
            ..., active = active, theme = theme,
            width = width, height = height
        )
    } else {
        data <- fortify_matrix(data = data, ...)
        new_quad_layout(
            name = "quad_layout",
            data = data, ylim = ylim, xlim = xlim,
            mapping = mapping, active = active, theme = theme,
            width = width, height = height
        )
    }
}

#' @export
#' @rdname quad_layout
quad_alignh <- function(...) {
    quad_layout(..., xlim = NULL, ylim = waiver())
}

#' @export
#' @rdname quad_layout
quad_alignv <- function(...) {
    quad_layout(..., xlim = waiver(), ylim = NULL)
}

#####################################################
#' @importFrom ggplot2 ggplot
#' @importFrom methods new
new_quad_layout <- function(name, data, xlim = waiver(), ylim = waiver(),
                            mapping = aes(), theme = NULL, active = NULL,
                            width = NA, height = NA,
                            class = "QuadLayout",
                            call = caller_call()) {
    if (!is.waive(xlim)) assert_limits(xlim, call = call)
    if (!is.waive(ylim)) assert_limits(ylim, call = call)
    if (is.waive(xlim) || is.waive(ylim)) {
        # If we need align discrete variables, data cannot be `NULL` and 
        # must be provided, here, we convert it to waiver() to indicate 
        # inherit from the parent layout
        data <- data %||% waiver()
        if (!is.waive(data) && !is.function(data)) {
            nrows <- NROW(data)
            ncols <- ncol(data)
        } else {
            nrows <- NULL
            ncols <- NULL
        }
    }
    horizontal <- ylim %|w|% discrete_design(nobs = nrows)
    vertical <- xlim %|w|% discrete_design(nobs = ncols)

    # always remove default axis titles
    # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
    # There are multiple ways to set labels in a plot, which take different
    # priorities. Here are the priorities from highest to lowest.
    # 1. The guide title.
    # 2. The scale name.
    # 3. The `labs()` function.
    # 4. The captured expression in aes().
    plot <- ggplot(mapping = mapping) +
        ggplot2::labs(x = NULL, y = NULL)

    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    # for `QuadLayout`, we use `NULL` to inherit data from parent layout
    # since `QuadLayout` must have data, and won't be waiver()
    # if inherit from the parent layout data, we'll inherit
    # the action data function
    schemes <- default_schemes(
        if (is.null(data)) waiver() else NULL,
        th = theme_no_panel()
    )

    # check arguments -----------------------------------
    width <- check_size(width, call = call)
    height <- check_size(height, call = call)
    assert_active(active, call = call)

    # Here we use S4 object to override the double dispatch of `+.gg` method
    new(
        class,
        # used by the layout
        data = data, theme = theme,
        schemes = schemes,
        plot_active = update_active(active, new_active(use = TRUE)),
        name = name,
        # used by the main body
        body_schemes = default_schemes(waiver()),
        # following parameters can be controlled by `quad_switch`
        width = width, height = height,
        # following parameters are used internally
        plot = plot, horizontal = horizontal, vertical = vertical
    )
}

# Used to create the QuadLayout
#' @include layout-.R
methods::setClass(
    "QuadLayout",
    contains = "LayoutProto",
    list(
        data = "ANY", plot = "ANY", body_schemes = "list", name = "character",
        # parameters for main body
        width = "ANY", height = "ANY",
        # Used to align axis
        horizontal = "ANY", vertical = "ANY",
        # top, left, bottom, right must be a StackLayout object.
        top = "ANY", left = "ANY", bottom = "ANY", right = "ANY",
        # If we regard `QuadLayout` as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        plot_active = "ANY"
    ),
    prototype = list(
        # used by QuadLayout
        top = NULL, left = NULL, bottom = NULL, right = NULL
    )
)

#' @export
is_layout_discrete.QuadLayout <- function(x, direction, ...) {
    is_discrete_design(slot(x, direction))
}

#' @export
is_layout_continuous.QuadLayout <- function(x, direction, ...) {
    is_continuous_design(slot(x, direction))
}
