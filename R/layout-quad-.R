#' Arrange plots in the quad-side of a main plot
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
#' The `quad_discrete` is a special case where both `xlim` and `ylim` are not
#' provided.
#'
#' The `quad_continuous` is a special case where both `xlim` and `ylim` are
#' provided.
#'
#' For historical reasons, the following aliases are available:
#' - `quad_alignh`: Align discrete variables in the horizontal direction and
#'   continuous variables in vertical direction.
#' - `quad_alignv`: Align discrete variables in the vertical direction and
#'   continuous variables in horizontal direction.
#' - `quad_alignb` is an alias for `quad_discrete`.
#' - `quad_free` is an alias for `quad_continuous`.
#'
#' @param data `r rd_layout_data()`. By default, this will attempt
#'   to inherit from the parent layout.
#'
#' If both `xlim` and `ylim` are provided, a `data frame` is required, and
#'   [`fortify_data_frame()`] will be used to convert the data to a data frame.
#'   When inherited by an annotation stack, no transposition will be applied.
#'
#' Otherwise, a `matrix` is required, and [`fortify_matrix()`] will be used to
#'   convert the data to a matrix. When inherited by the column annotation
#'   stack, the data will be transposed.
#' @param mapping Default list of aesthetic mappings to use for main plot in the
#' layout. If not specified, must be supplied in each layer added to the main
#' plot.
#' @param xlim,ylim A [`continuous_limits()`] object specifying the left/lower
#' limit and the right/upper limit of the scale. Used to align the continuous
#' axis.
#' @param width,height The relative width/height of the main plot, can be a
#' [`unit`][grid::unit] object.
#' @inheritParams stack_layout
#' @inheritParams align
#' @return A `QuadLayout` object.
#' @section ggplot2 specification:
#' If either `xlim` or `ylim` is not provided, the data input will be converted
#' to a matrix using [`fortify_matrix()`], and the data in the underlying main
#' plot will contain the following columns:
#'
#'  - `.panel_x` and `.panel_y`: the column and row panel groups.
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
#' Otherwise, the data input will be used for the main plot.
#'
#' @export
quad_layout <- function(data = waiver(), mapping = aes(),
                        xlim = waiver(), ylim = waiver(),
                        ...,
                        theme = NULL, active = NULL,
                        width = NA, height = NA) {
    QuadLayout(
        name = "quad_layout",
        ylim = ylim, xlim = xlim,
        data = data, mapping = mapping,
        active = active, theme = theme,
        width = width, height = height
    )
}

#' @export
#' @rdname quad_layout
quad_alignh <- function(..., ylim = waiver()) {
    quad_layout(..., xlim = NULL, ylim = ylim)
}

#' @export
#' @rdname quad_layout
quad_alignv <- function(..., xlim = waiver()) {
    quad_layout(..., xlim = xlim, ylim = NULL)
}

##########################################################
#' @export
#' @rdname quad_layout
quad_discrete <- function(data = waiver(), mapping = aes(),
                          ...,
                          theme = NULL, active = NULL,
                          width = NA, height = NA) {
    UseMethod("quad_discrete")
}

#' @export
#' @rdname quad_layout
#' @usage NULL
quad_alignb <- quad_discrete

#' @export
quad_discrete.default <- function(data = waiver(), mapping = aes(),
                                  ...,
                                  theme = NULL, active = NULL,
                                  width = NA, height = NA) {
    QuadLayout(
        name = "quad_discrete",
        xlim = waiver(), ylim = waiver(),
        data = data, mapping = mapping,
        active = active, theme = theme,
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
#' @export
#' @rdname quad_layout
quad_continuous <- function(data = waiver(), mapping = aes(),
                            xlim = NULL, ylim = NULL,
                            ...,
                            theme = NULL, active = NULL,
                            width = NA, height = NA) {
    UseMethod("quad_continuous")
}

#' @usage NULL
#' @export
#' @rdname quad_layout
ggside <- quad_continuous

#' @usage NULL
#' @export
#' @rdname quad_layout
quad_free <- quad_continuous

#' @export
quad_continuous.default <- function(data = waiver(), mapping = aes(),
                                    xlim = NULL, ylim = NULL,
                                    ...,
                                    theme = NULL, active = NULL,
                                    width = NA, height = NA) {
    QuadLayout(
        name = "quad_continuous",
        xlim = xlim %|w|% NULL, ylim = ylim %|w|% NULL,
        data = data, mapping = mapping,
        active = active, theme = theme,
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

#####################################################
# Used to create the QuadLayout
#' @importFrom ggplot2 ggplot theme
#' @importFrom grid is.unit unit
#' @include layout-chain-stack-.R
QuadLayout <- S7::new_class(
    "QuadLayout", LayoutProto,
    properties = list(
        current = S7::new_property(
            S7::new_union(NULL, S7::class_integer, S7::class_character),
            validator = function(value) {
                if (is.null(value)) return(NULL) # styler: off
                if (length(value) != 1L) {
                    return("must be a single integer number or a single string")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
            }
        ),
        plot = S7::new_property(
            Graph,
            validator = function(value) {
                if (!is.null(value) &&
                    length(prop(prop(value, "control"), "size")) != 2L) {
                    return("'size' property must be of length 2")
                }
            }
        ),
        # Used to align axis
        horizontal = S7::new_union(NULL, Domain),
        vertical = S7::new_union(NULL, Domain),
        # top, left, bottom, right must be a StackLayout object.
        top = S7::new_union(NULL, StackLayout),
        left = S7::new_union(NULL, StackLayout),
        bottom = S7::new_union(NULL, StackLayout),
        right = S7::new_union(NULL, StackLayout)
    ),
    constructor = function(name, xlim = waiver(), ylim = waiver(), ...,
                           data = waiver(), mapping = aes(),
                           theme = NULL, active = NULL,
                           width = NA, height = NA) {
        if (!is_waiver(xlim)) assert_limits(xlim)
        if (!is_waiver(ylim)) assert_limits(ylim)
        if (is_waiver(xlim) || is_waiver(ylim)) {
            data <- fortify_matrix(data, ...)
            if (!is.null(data) && !is_waiver(data) && !is.function(data)) {
                nrows <- NROW(data)
                ncols <- ncol(data)

                # for data has dimention but one dimention is 0
                # as.matrix(data.frame(row.names = letters))
                if (nrows == 0L || ncols == 0L) {
                    cli_abort("empty data is no allowed")
                }
            } else {
                nrows <- NA_integer_
                ncols <- NA_integer_
            }
        } else {
            data <- fortify_data_frame(data, ...)
        }
        width <- check_size(width)
        height <- check_size(height)
        if (!is.unit(width)) width <- unit(width, "null")
        if (!is.unit(height)) height <- unit(height, "null")
        horizontal <- ylim %|w|% DiscreteDomain(nobs = nrows)
        vertical <- xlim %|w|% DiscreteDomain(nobs = ncols)

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
        plot_schemes <- Schemes(scheme_data(waiver()))
        plot_control <- plot_control(unit.c(width, height))
        plot <- Graph(
            plot = plot,
            schemes = plot_schemes,
            control = plot_control
        )

        # when no data provided, we inherit the scheme data from the layout for
        # the main plot
        layout_schemes <- Schemes(
            scheme_data(
                if (is.null(data) || is_waiver(data)) {
                    waiver()
                } else {
                    NULL
                }
            ),
            scheme_theme(theme_no_strip())
        )
        layout_context <- layout_context(theme %||% theme(), layout_schemes)
        new_object(
            S7_object(),
            # used by the layout
            name = name,
            data = data,
            context = layout_context,
            control = layout_control(),
            plot = plot, horizontal = horizontal, vertical = vertical
        )
    }
)
