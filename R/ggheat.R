#' Heatmap with ggplot2
#'
#' @param data A numeric or character vector, a data frame, and any other data
#' which can be converted into a matrix. Simple vector will be converted into a
#' one column matrix.
#' @param mapping Default list of aesthetic mappings to use for plot. If `NULL`,
#' will using `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments passed to matrix method.
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
#' center. One of:
#'   - `NULL` for no breaks
#'   - `waiver()`: if `xlabels`/`ylabels` is `NULL`, then
#'     `xlabels_nudge`/`ylabels_nudge` will be `NULL`, otherwise `0`.
#'   - A numeric.
#' @inheritParams patchwork::plot_layout
#' @param filling A boolean value indicates whether filling the heatmap. If you
#' want to custom the filling style, you can set to `FALSE`.
#'
#' @section ggplot2 details:
#' The data input in `ggheat` will be converted into the long formated data
#' frame when drawing. The default mapping will use `aes(.data$.x, .data$.y)`,
#' you can use `mapping` argument to control it. The data in the underlying
#' ggplot object contains following columns:
#'
#'  - `.row_panel`: the row panel
#'
#'  - `.column_panel`: the column panel
#'
#'  - `.row_names` and `.column_names`: the row and column names of the original
#'    matrix (only applicable when names exist).
#'
#'  - `.row_index` and `.column_index`: the row and column index of the original
#'    matrix.
#'
#'  - `.x` and `.y`: the `x` and `y` coordinates
#'
#'  - `value`: the actual matrix value.
#'
#' @return A `ggheatmap` object.
#' @examples
#' ggheat(1:10)
#' ggheat(letters)
#' @importFrom ggplot2 aes
#' @export
ggheat <- function(data, mapping = aes(), ...) UseMethod("ggheat")

#' @importFrom ggplot2 waiver theme
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
                          filling = TRUE, ...) {
    assert_bool(filling)
    xlabels <- set_labels(xlabels, "column", colnames(data), ncol(data))
    xlabels_nudge <- set_nudge(xlabels_nudge, ncol(data), xlabels, "column")
    ylabels <- set_labels(ylabels, "row", rownames(data), nrow(data))
    ylabels_nudge <- set_nudge(ylabels_nudge, nrow(data), ylabels, "row")
    mapping <- mapping %||% aes(.data$.x, .data$.y)
    heatmap <- ggplot2::ggplot(mapping = mapping) +
        ggplot2::theme_bw() +
        theme(
            plot.background = element_blank(),
            panel.border = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
    if (ncol(data) > 10L) {
        heatmap <- heatmap + theme(
            axis.text.x = ggplot2::element_text(angle = -60, hjust = 0L)
        )
    }
    # add heatmap filling in the first layer
    if (filling) {
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
        active = NULL
    )
}

set_nudge <- function(nudge, n, labels, axis,
                      arg = rlang::caller_arg(nudge),
                      call = caller_call()) {
    if (is.numeric(nudge)) {
        if (!is_scalar(nudge) && length(nudge) != n) {
            cli::cli_abort(paste(
                "{.arg {arg}} must be of length 1 or",
                "the same length of heatmap {axis}"
            ), call = call)
        }
        nudge <- rep_len(nudge, n)
    } else if (is.waiver(nudge)) {
        if (is.null(labels)) nudge <- NULL else nudge <- rep_len(0, n)
    } else if (!is.null(nudge)) {
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
        bottom = "ANY", right = "ANY"
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
        slot(x, "heatmap")$plot_env
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
#' @return A boolean value
#' @examples
#' is.ggheatmap(ggheat(1:10))
#' @export
is.ggheatmap <- function(x) methods::is(x, "ggheatmap")
