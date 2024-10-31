#' Arrange Plots in the Quad-Side of a main plot
#'
#' This function integrates the functionalities of `quad_free()`,
#' `quad_alignh()`, `quad_alignv()`, and `quad_alignb()` into a single
#' interface.
#'
#' @param data `r rd_layout_data()`. The conversion of data depends on the
#' `align` argument and will use, [`fortify_data_frame()`]
#' or [`fortify_matrix()`]:
#'  - If `align` is `NULL`, a data frame is required. When inherited by the
#'    annotation stack, no transposition will be applied.
#'  - If `align` is a string, a matrix is required. When inherited by the column
#'    annotation stack, it will be transposed.
#' @param align A string indicating the alignment direction:
#' - `"horizontal"`: Align plots horizontally.
#' - `"vertical"`: Align plots vertically.
#' - `"both"`: Align plots in both directions.
#'
#' By default, the function does not align observations.
#'
#' @inheritParams quad_free
#' @export
quad_layout <- function(data = NULL, align = NULL, mapping = aes(),
                        ...,
                        action = NULL, theme = NULL, context = NULL,
                        width = NA, height = NA) {
    if (!is.null(align)) {
        align <- match.arg(align, c("horizontal", "vertical", "both"))
    }
    if (is.null(align)) {
        quad_free(
            data = data, mapping = mapping,
            ..., action = action, context = context, theme = theme,
            width = width, height = height
        )
    } else {
        switch(align,
            both = quad_alignb(
                data = data, mapping = mapping,
                ..., action = action, context = context, theme = theme,
                width = width, height = height
            ),
            horizontal = quad_alignh(
                data = data, mapping = mapping,
                ..., action = action, context = context, theme = theme,
                width = width, height = height
            ),
            vertical = quad_alignv(
                data = data, mapping = mapping,
                ..., action = action, context = context, theme = theme,
                width = width, height = height
            )
        )
    }
}

##########################################################
#' Arrange Plots in the Quad-Side of a main plot
#'
#' These functions arrange plots around a main plot, allowing for flexible
#' alignment of observations in different directions.
#'
#' @description
#' - `quad_free`: Never align observations.
#' - `quad_alignh`: Align observations in the horizontal direction.
#' - `quad_alignv`: Align observations in the vertical direction.
#' - `quad_alignb`: Align observations in both horizontal and vertical
#'   directions.
#' @param data `r rd_layout_data()`.
#' - For `quad_free`, the function uses [`fortify_data_frame()`] to convert
#'   the data into a data frame.
#' - For all other functions, it employs [`fortify_matrix()`] to convert
#'   the data into a matrix.
#' @param mapping Default list of aesthetic mappings to use for the main plot.
#' If not specified, must be supplied in each layer added to the plot.
#' @param ... Additional arguments passed to [`fortify_matrix()`] or
#' [`fortify_data_frame()`].
#' @param action A [`plot_action()`] object used to define the default action
#' for the plots in the layout.
#' @inheritParams align_plots
#' @param context A [`plot_context()`] object that defines the action when
#'   added to a layout.
#' @param width,height The relative width/height of the main plot, can be a
#' [`unit`][grid::unit] object
#' @export
quad_free <- function(data = NULL, mapping = aes(),
                      ...,
                      action = NULL, theme = NULL, context = NULL,
                      width = NA, height = NA) {
    UseMethod("quad_free")
}

#' @export
quad_free.default <- function(data = NULL, mapping = aes(),
                              ...,
                              action = NULL, theme = NULL, context = NULL,
                              width = NA, height = NA) {
    data <- data %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    new_quad_layout(
        name = "quad_free",
        data = data, horizontal = NULL, vertical = NULL,
        mapping = mapping, action = action, context = context, theme = theme,
        width = width, height = height
    )
}

#' @export
quad_free.uneval <- function(data, ...) {
    cli::cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn quad_free}"
    ))
}

#########################################################################
#' @export
#' @rdname quad_free
quad_alignh <- function(data = NULL, mapping = aes(),
                        ...,
                        action = NULL, theme = NULL, context = NULL,
                        width = NA, height = NA) {
    UseMethod("quad_alignh")
}

#' @export
quad_alignh.default <- function(data = NULL, mapping = aes(),
                                ...,
                                action = NULL, theme = NULL, context = NULL,
                                width = NA, height = NA) {
    data <- data %|w|% NULL
    # we need a matrix to melted into long formated data frame
    data <- fortify_matrix(data = data, ...)
    if (!is.null(data) && !is.function(data)) {
        nrows <- NROW(data)
    } else {
        nrows <- NULL
    }
    new_quad_layout(
        name = "quad_alignh",
        data = data,
        horizontal = new_layout_params(nobs = nrows),
        vertical = NULL,
        mapping = mapping, action = action, context = context, theme = theme,
        width = width, height = height
    )
}

#' @export
quad_alignh.uneval <- function(data, ...) {
    cli::cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn quad_alignh}"
    ))
}

#########################################################################
#' @export
#' @rdname quad_free
quad_alignv <- function(data = NULL, mapping = aes(),
                        ...,
                        action = NULL, theme = NULL, context = NULL,
                        width = NA, height = NA) {
    UseMethod("quad_alignv")
}

#' @export
quad_alignv.default <- function(data = NULL, mapping = aes(),
                                ...,
                                action = NULL, theme = NULL, context = NULL,
                                width = NA, height = NA) {
    data <- data %|w|% NULL
    # we need a matrix to melted into long formated data frame
    data <- fortify_matrix(data = data, ...)
    if (!is.null(data) && !is.function(data)) {
        ncols <- ncol(data)
    } else {
        ncols <- NULL
    }
    new_quad_layout(
        name = "quad_alignv",
        data = data,
        horizontal = NULL,
        vertical = new_layout_params(nobs = ncols),
        mapping = mapping, action = action, context = context, theme = theme,
        width = width, height = height
    )
}

#' @export
quad_alignv.uneval <- function(data, ...) {
    cli::cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn quad_alignv}"
    ))
}

#############################################################
#' @export
#' @rdname quad_free
quad_alignb <- function(data = NULL, mapping = aes(),
                        ...,
                        action = NULL, theme = NULL, context = NULL,
                        width = NA, height = NA) {
    UseMethod("quad_alignb")
}

#' @export
quad_alignb.default <- function(data = NULL, mapping = aes(),
                                ...,
                                action = NULL, theme = NULL, context = NULL,
                                width = NA, height = NA) {
    data <- data %|w|% NULL
    # we need a matrix to melted into long formated data frame
    data <- fortify_matrix(data = data, ...)
    if (!is.null(data) && !is.function(data)) {
        nrows <- NROW(data)
        ncols <- ncol(data)
    } else {
        nrows <- NULL
        ncols <- NULL
    }
    new_quad_layout(
        name = "quad_alignb",
        data = data,
        horizontal = new_layout_params(nobs = nrows),
        vertical = new_layout_params(nobs = ncols),
        mapping = mapping, action = action, context = context, theme = theme,
        width = width, height = height
    )
}

#' @export
quad_alignb.uneval <- function(data, ...) {
    cli::cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn quad_alignb}"
    ))
}

#####################################################
new_quad_layout <- function(name, data, horizontal, vertical,
                            mapping = aes(),
                            action = NULL, theme = NULL, context = NULL,
                            width = NA, height = NA, class = "QuadLayout",
                            call = caller_call()) {
    plot <- ggplot2::ggplot(mapping = mapping)
    # for `QuadLayout`, we use `NULL` to inherit data
    # since `QuadLayout` must have data, and won't be waiver()
    # if inherit from the parent layout data, we'll inherit
    # the action data function
    action <- check_action(
        action,
        if (is.null(data)) waiver() else NULL,
        call = call
    )
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    # check arguments -----------------------------------
    width <- check_size(width, call = call)
    height <- check_size(height, call = call)
    assert_s3_class(context, "plot_context", null_ok = TRUE)

    # Here we use S4 object to override the double dispatch of `+.gg` method
    methods::new(
        class,
        # used by the layout
        data = data, theme = theme, action = action,
        context = update_context(context, new_context(
            order = NA_integer_, active = TRUE, name = NA_character_
        )), name = name,
        # used by the main body
        body_action = default_action(waiver()),
        # following parameters can be controlled by `quad_switch`
        width = width, height = height,
        # following parameters are used internally
        plot = plot, horizontal = horizontal, vertical = vertical
    )
}

# Used to create the QuadLayout
methods::setClass(
    "QuadLayout",
    contains = "Layout",
    list(
        data = "ANY", plot = "ANY", body_action = "ANY", name = "character",
        # parameters for main body
        width = "ANY", height = "ANY",
        # If we regard QuadLayout as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        context = "ANY",
        # Used by the layout itself:
        horizontal = "ANY", vertical = "ANY",
        # top, left, bottom, right must be a StackLayout object.
        top = "ANY", left = "ANY", bottom = "ANY", right = "ANY"
    ),
    prototype = list(
        horizontal = NULL, vertical = NULL,
        # used by QuadLayout
        top = NULL, left = NULL, bottom = NULL, right = NULL
    )
)

# used to create the heatmap layout
#' @keywords internal
methods::setClass(
    "HeatmapLayout",
    contains = "QuadLayout",
    list(filling = "ANY") # parameters for heatmap body
)
