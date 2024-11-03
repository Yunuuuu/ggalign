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
                        theme = NULL, active = NULL,
                        width = NA, height = NA) {
    if (!is.null(align)) {
        align <- match.arg(align, c("horizontal", "vertical", "both"))
    }
    if (is.null(align)) {
        quad_free(
            data = data, mapping = mapping,
            ..., active = active, theme = theme,
            width = width, height = height
        )
    } else {
        switch(align,
            both = quad_alignb(
                data = data, mapping = mapping,
                ..., active = active, theme = theme,
                width = width, height = height
            ),
            horizontal = quad_alignh(
                data = data, mapping = mapping,
                ..., active = active, theme = theme,
                width = width, height = height
            ),
            vertical = quad_alignv(
                data = data, mapping = mapping,
                ..., active = active, theme = theme,
                width = width, height = height
            )
        )
    }
}

##########################################################
#' Arrange Plots in the Quad-Side of a main plot
#'
#' These functions arrange plots around a main plot, allowing for flexible
#' alignment of observations in different directions. `ggside` is an alias for
#' `quad_free`.
#'
#' @description
#' - `quad_free`/`ggside`: Never align observations.
#' - `quad_alignh`: Align observations in the horizontal direction.
#' - `quad_alignv`: Align observations in the vertical direction.
#' - `quad_alignb`: Align observations in both horizontal and vertical
#'   directions.
#' @param data `r rd_layout_data()`.
#' - For `quad_free`/`ggside`, the function uses [`fortify_data_frame()`] to
#'   convert the data into a data frame.
#' - For all other functions, it employs [`fortify_matrix()`] to convert
#'   the data into a matrix.
#' @inheritParams ggplot2::ggplot
#' @param ... Additional arguments passed to [`fortify_matrix()`] or
#' [`fortify_data_frame()`].
#' @inheritParams align_plots
#' @inheritParams align
#' @param width,height The relative width/height of the main plot, can be a
#' [`unit`][grid::unit] object.
#' @export
quad_free <- function(data = NULL, mapping = aes(),
                      ...,
                      theme = NULL, active = NULL,
                      width = NA, height = NA) {
    UseMethod("quad_free")
}

#' @usage NULL
#' @export
#' @rdname quad_free
ggside <- quad_free

#' @export
quad_free.default <- function(data = NULL, mapping = aes(),
                              ...,
                              theme = NULL, active = NULL,
                              width = NA, height = NA) {
    data <- data %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    new_quad_layout(
        name = "quad_free",
        data = data, horizontal = NULL, vertical = NULL,
        mapping = mapping, active = active, theme = theme,
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
                        theme = NULL, active = NULL,
                        width = NA, height = NA) {
    UseMethod("quad_alignh")
}

#' @export
quad_alignh.default <- function(data = NULL, mapping = aes(),
                                ...,
                                theme = NULL, active = NULL,
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
        mapping = mapping, active = active, theme = theme,
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
                        theme = NULL, active = NULL,
                        width = NA, height = NA) {
    UseMethod("quad_alignv")
}

#' @export
quad_alignv.default <- function(data = NULL, mapping = aes(),
                                ...,
                                theme = NULL, active = NULL,
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
        mapping = mapping, active = active, theme = theme,
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
                        theme = NULL, active = NULL,
                        width = NA, height = NA) {
    UseMethod("quad_alignb")
}

#' @export
quad_alignb.default <- function(data = NULL, mapping = aes(),
                                ...,
                                theme = NULL, active = NULL,
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
        mapping = mapping, active = active, theme = theme,
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
#' @importFrom ggplot2 ggplot
new_quad_layout <- function(name, data, horizontal, vertical,
                            mapping = aes(), theme = NULL, active = NULL,
                            width = NA, height = NA, class = "QuadLayout",
                            call = caller_call()) {
    plot <- ggplot(mapping = mapping)
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    # for `QuadLayout`, we use `NULL` to inherit data from parent layout
    # since `QuadLayout` must have data, and won't be waiver()
    # if inherit from the parent layout data, we'll inherit
    # the action data function
    controls <- new_controls(
        new_plot_data(if (is.null(data)) waiver() else NULL)
    )

    # check arguments -----------------------------------
    width <- check_size(width, call = call)
    height <- check_size(height, call = call)
    assert_active(active, call = call)

    # Here we use S4 object to override the double dispatch of `+.gg` method
    methods::new(
        class,
        # used by the layout
        data = data, theme = theme,
        controls = controls,
        plot_active = update_active(active, new_active(
            order = NA_integer_, use = TRUE, name = NA_character_
        )),
        name = name,
        # used by the main body
        body_controls = new_controls(new_plot_data(waiver())),
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
        data = "ANY", plot = "ANY", body_controls = "list", name = "character",
        # parameters for main body
        width = "ANY", height = "ANY",
        # If we regard QuadLayout as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        plot_active = "ANY",
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

#' @aliases +.QuadLayout &.QuadLayout -.QuadLayout
#' @aliases +.HeatmapLayout &.HeatmapLayout -.HeatmapLayout
#' @aliases +.ggheatmap &.ggheatmap -.ggheatmap
#' @aliases +.ggside &.ggside -.ggside
#' @importFrom methods Ops
#' @export
#' @rdname layout-operator
methods::setMethod("Ops", c("QuadLayout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code {.Generic}} with a single argument.",
            "i" = "Did you accidentally put {.code {.Generic}} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    switch(.Generic, # nolint
        `+` = quad_layout_add(e2, e1, e2name),
        `-` = quad_layout_subtract(e2, e1, e2name),
        `&` = quad_layout_and_add(e2, e1, e2name),
        stop_incompatible_op(.Generic, e1, e2)
    )
})

# used to create the heatmap layout
#' @keywords internal
methods::setClass(
    "HeatmapLayout",
    contains = "QuadLayout",
    list(filling = "ANY") # parameters for heatmap body
)
