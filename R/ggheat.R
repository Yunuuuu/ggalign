#' Heatmap with ggplot2
#'
#' @param data A matrix, if it is a simple vector, it will be converted to a
#' one-column matrix. Data.frame will also be coerced into matrix.
#' @param mapping Default list of aesthetic mappings to use for plot. If `NULL`,
#' will using `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments.
#' @param width,height Heatmap width/height, can be a [unit][grid::unit] object.
#' @param xlabels,ylabels Labels for x/y, the default will use
#' the colnames/rownames of the `data`. One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels
#'   - A character vector giving labels (must be same length as the heatmap
#'     axis)
#'   - An expression vector (must be the same length as heatmap axis). See
#'     `?plotmath` for details.
#'   - A function that takes the default labels as the input and returns labels
#'     as output. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param xlabels_nudge,ylabels_nudge A single numeric or a numeric value of
#' length `ncol(data)/nrow(data)`, to nudge each text label away from the
#' center. If `waiver()`, it means `0`. If `NULL`, no breaks, in this way labels
#' will be removed too.
#' @inheritParams patchwork::plot_layout
#' @param filling A boolean value indicates whether filling the heatmap. If you
#' want to custom the filling style, you can set to `FALSE`.
#' @param environment Used by [ggplot_build][ggplot2::ggplot_build].
#' @return A `ggheatmap` object.
#' @importFrom ggplot2 aes
#' @export
ggheat <- function(data, mapping = aes(), ...) UseMethod("ggheat")

#' @importFrom ggplot2 waiver
#' @export
#' @rdname ggheat
ggheat.matrix <- function(data, mapping = NULL,
                          width = NULL, height = NULL,
                          xlabels = waiver(),
                          ylabels = waiver(),
                          xlabels_nudge = waiver(),
                          ylabels_nudge = waiver(),
                          guides = "collect",
                          axes = NULL, axis_titles = axes,
                          filling = TRUE, ...,
                          environment = parent.frame()) {
    assert_bool(filling)
    xlabels <- set_labels(xlabels, "column", colnames(data), ncol(data))
    xlabels_nudge <- set_nudge(xlabels_nudge, ncol(data), xlabels, "column")
    ylabels <- set_labels(ylabels, "row", rownames(data), nrow(data))
    ylabels_nudge <- set_nudge(ylabels_nudge, ncol(data), ylabels, "row")
    mapping <- mapping %||% aes(.data$.x, .data$.y)
    heatmap <- ggplot2::ggplot(mapping = mapping)
    if (ncol(data) > 10L) {
        heatmap <- heatmap + ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = -60, hjust = 0L)
        )
    }
    if (filling) {
        # add heatmap filling in the first layer
        heatmap <- heatmap + ggplot2::geom_tile(
            aes(.data$.x, .data$.y, fill = .data$value),
            width = 1L, height = 1L
        )
    }
    methods::new("ggheatmap",
        matrix = data,
        params = rlang::list2(
            width = set_size(width),
            height = set_size(height),
            xlabels = xlabels,
            xlabels_nudge = xlabels_nudge,
            ylabels = ylabels,
            ylabels_nudge = ylabels_nudge,
            guides = guides,
            axes = axes,
            axis_titles = axis_titles,
            filling = filling
        ),
        heatmap = heatmap,
        active = NULL,
        plot_env = environment
    )
}

set_nudge <- function(nudge, n, names, axis,
                      arg = rlang::caller_arg(nudge),
                      call = caller_call()) {
    # if no names, default to
    if (is.waiver(nudge) && is.null(names)) return(NULL) # styler: off
    if (is.numeric(nudge)) {
        if (!is_scalar(nudge) && length(nudge) != n) {
            cli::cli_abort(paste(
                "{.arg {arg}} must be of length 1 or",
                "the same length of heatmap {axis}"
            ), call = call)
        }
        nudge <- rep_len(nudge, n)
    } else if (!is.waiver(nudge) && !is.null(nudge)) {
        cli::cli_abort(
            "{.arg {arg}} must be `waiver()`, `NULL` or a numeric",
            call = call
        )
    }
    nudge
}

set_labels <- function(labels, axis, default, n,
                       arg = rlang::caller_arg(labels),
                       call = caller_call()) {
    labels <- allow_lambda(labels)
    if (is.waiver(labels)) {
        return(default)
    } else if (is.null(labels)) {
        return(NULL)
    } else if (identical(labels, NA)) {
        cli::cli_abort(c(
            "Invalid {.arg {arg}} specification.",
            i = "Use {.code NULL}, not {.code NA}"
        ), call = call)
    } else if (is.function(labels)) {
        labels <- labels(default)
    }
    if (is.atomic(labels) && n != length(labels)) {
        cli::cli_abort(
            "{.arg {arg}} must have the same length of heatmap {axis}.",
            call = call
        )
    }
    labels
}

#' @export
#' @rdname ggheat
ggheat.data.frame <- function(data, mapping = aes(), ...) {
    data <- as.matrix(data)
    ggheat(data = data, mapping = mapping, ...)
}

#' @export
#' @rdname ggheat
ggheat.numeric <- function(data, mapping = aes(), ...) {
    ans <- matrix(data, ncol = 1L)
    colnames(ans) <- "V1"
    if (rlang::is_named(data)) rownames(ans) <- names(data)
    ggheat(data = ans, mapping = mapping, ...)
}

#' @export
#' @rdname ggheat
ggheat.character <- ggheat.numeric

#' @export
#' @rdname ggheat
ggheat.default <- function(data, mapping = aes(), ...) {
    data <- as.matrix(data)
    ggheat(data = data, mapping = mapping, ...)
}

#' @export
ggheat.NULL <- function(data, mapping = aes(), ...) {
    cli::cli_abort("{.arg data} must be a matrix-like object instead of `NULL`")
}

#' @keywords internal
methods::setClass("ggheat")

# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#' @keywords internal
methods::setClass(
    "ggheatmap",
    contains = "ggheat",
    list(
        matrix = "matrix",
        params = "list",
        row_panels = "ANY",
        row_index = "ANY",
        column_panels = "ANY",
        column_index = "ANY",
        heatmap = "ANY", active = "ANY",
        facetted_pos_scales = "ANY",
        top = "ANY", left = "ANY",
        bottom = "ANY", right = "ANY",
        plot_env = "environment"
    ),
    prototype = list(
        row_index = NULL,
        row_panels = NULL,
        column_panels = NULL,
        column_index = NULL,
        top = NULL, left = NULL,
        bottom = NULL, right = NULL
    )
)

#' Subset a `ggheatmap` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A ggheatmap object
#' @param name A string of slot name in `ggheatmap` object.
#' @keywords internal
methods::setMethod("$", "ggheatmap", function(x, name) {
    # https://github.com/tidyverse/ggplot2/issues/6002
    if (name == "theme") {
        slot(x, "heatmap")$theme
    } else if (name == "plot_env") {
        slot(x, "plot_env")
    } else {
        cli::cli_abort(c(
            "`$` is just for internal usage for ggplot2 methods",
            i = "try to use `@` method instead"
        ))
    }
})

#' @importFrom ggplot2 is.ggplot
#' @importFrom rlang is_string
methods::setValidity("ggheatmap", function(object) {
    if (!is.ggplot(slot(object, "heatmap"))) {
        cli::cli_abort("@heatmap must be a {.cls ggplot} object")
    }
    active <- slot(object, "active")
    if (!is.null(active) &&
        (!is_string(active) || !any(active == GGHEAT_ELEMENTS))) {
        cli::cli_abort(sprintf(
            "@active must be a string of %s",
            oxford_comma(GGHEAT_ELEMENTS, final = "or")
        ))
    }
    TRUE
})

#' @param object A `ggheatmap` object.
#' @importFrom methods show
#' @export
#' @rdname ggheat
methods::setMethod("show", "ggheatmap", function(object) print(object))

#' Reports whether x is a ggheatmap object
#'
#' @param x An object to test
#' @export
is.ggheatmap <- function(x) methods::is(x, "ggheatmap")
