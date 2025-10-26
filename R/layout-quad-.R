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
                        theme = NULL, active = deprecated(),
                        width = deprecated(), height = deprecated()) {
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
                          theme = NULL, active = deprecated(),
                          width = deprecated(), height = deprecated()) {
    UseMethod("quad_discrete")
}

#' @export
#' @rdname quad_layout
#' @usage NULL
quad_alignb <- quad_discrete

#' @export
quad_discrete.default <- function(data = waiver(), mapping = aes(),
                                  ...,
                                  theme = NULL, active = deprecated(),
                                  width = deprecated(), height = deprecated()) {
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
                            theme = NULL, active = deprecated(),
                            width = deprecated(), height = deprecated()) {
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
                                    theme = NULL, active = deprecated(),
                                    width = deprecated(),
                                    height = deprecated()) {
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
#' @importFrom ggplot2 ggplot theme
QuadLayout <- S7::new_class(
    "QuadLayout", LayoutProto,
    properties = list(
        current = S7::new_property(
            S7::new_union(NULL, S7::class_integer, S7::class_character),
            validator = function(value) {
                if (!is.null(value) && length(value) != 1L) {
                    return("must be a single integer number or single string")
                }
            }
        ),
        graph = S7::new_property(
            Graph,
            validator = function(value) {
                if (isFALSE(prop(prop(value, "control"), "use"))) {
                    return("'use' in 'plot_control()' cannot be `FALSE` for QuadLayout")
                }
                if (length(prop(prop(value, "control"), "size")) != 2L) {
                    return("'size' in 'plot_control()' must be of length 2 for QuadLayout")
                }
            },
            setter = function(self, value) {
                if (S7_inherits(value, Graph)) {
                    # we should check control size, don't set `check = FALSE`
                    prop(self, "graph") <- value
                } else {
                    if (S7_inherits(graph <- prop(self, "graph"), Graph)) {
                        prop(graph, "plot", check = FALSE) <- value
                        prop(self, "graph", check = FALSE) <- graph
                    } else {
                        prop(self, "graph", check = FALSE) <- Graph(
                            plot = plot,
                            control = plot_control(size = c(NA, NA))
                        )
                    }
                }
                self
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
                           theme = NULL, active = deprecated(),
                           width = deprecated(), height = deprecated(),
                           call = caller_call()) {
        if (!is_waiver(xlim)) assert_limits(xlim, call = call)
        if (!is_waiver(ylim)) assert_limits(ylim, call = call)
        if (is_waiver(xlim) || is_waiver(ylim)) {
            data <- fortify_matrix(data, ...)
            # If we need align discrete variables, data cannot be `NULL` and
            # must be provided, here, we convert it to waiver() to indicate
            # inherit from the parent layout
            data <- data %||% waiver()
            if (!is_waiver(data) && !is.function(data)) {
                nrows <- NROW(data)
                ncols <- NCOL(data)

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
        plot_schemes <- schemes_complete(Schemes(scheme_data(waiver())))

        # setup controls for the main plot
        if (lifecycle::is_present(width)) {
            lifecycle::deprecate_soft(
                "1.2.0.9000",
                sprintf("%s(width)", as.character(.subset2(call, 1L))),
                details = "Please use `+ plot_control()` instead"
            )
            width <- check_size(width, call = call)
        } else {
            width <- NA
        }
        if (lifecycle::is_present(height)) {
            lifecycle::deprecate_soft(
                "1.2.0.9000",
                sprintf("%s(height)", as.character(.subset2(call, 1L))),
                details = "Please use `+ plot_control()` instead"
            )
            height <- check_size(height, call = call)
        } else {
            height <- NA
        }
        if (!is.unit(width)) width <- unit(width, "null")
        if (!is.unit(height)) height <- unit(height, "null")
        plot_control <- plot_control(unit.c(width, height))
        if (lifecycle::is_present(active)) {
            lifecycle::deprecate_soft(
                "1.2.0.9000",
                sprintf("%s(active)", as.character(.subset2(call, 1L))),
                details = "Please use `+ plot_control()` instead"
            )
            assert_active(active, allow_null = FALSE, call = call)
            plot_control <- ggalign_update(plot_control, active)
        }
        graph <- Graph(
            plot = plot,
            schemes = plot_schemes,
            control = plot_control
        )

        # when no data provided, we inherit the scheme data from the layout for
        # the main plot
        layout_schemes <- default_schemes(
            if (is.null(data)) waiver() else NULL,
            th = theme_no_strip()
        )
        new_object(
            S7_object(),
            # used by the layout
            name = name, data = data,
            titles = layout_title(),
            schemes = layout_schemes,
            theme = theme %||% theme(),
            graph = graph, horizontal = horizontal, vertical = vertical
        )
    }
)
