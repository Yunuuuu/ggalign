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
#' The data input in `ggheatmap` will be converted into the long formated data
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
#' ggheatmap(1:10)
#' ggheatmap(letters)
#' @importFrom ggplot2 aes
#' @export
layout_heatmap <- function(data, mapping = aes(), ...) {
    UseMethod("layout_heatmap")
}

#' @export
#' @rdname layout_heatmap
ggheatmap <- layout_heatmap

#' @importFrom ggplot2 waiver theme
#' @export
#' @rdname layout_heatmap
layout_heatmap.matrix <- function(data, mapping = NULL,
                                  width = NULL, height = NULL,
                                  xlabels = waiver(),
                                  ylabels = waiver(),
                                  xlabels_nudge = waiver(),
                                  ylabels_nudge = waiver(),
                                  guides = "collect",
                                  axes = NULL, axis_titles = axes,
                                  filling = TRUE, ...) {
    assert_bool(filling)
    xlabels <- set_labels(xlabels, colnames(data), "x")
    xlabels_nudge <- set_nudge(xlabels_nudge, ncol(data), xlabels, "x")
    ylabels <- set_labels(ylabels, rownames(data), "y")
    ylabels_nudge <- set_nudge(ylabels_nudge, nrow(data), ylabels, "y")
    mapping <- mapping %||% aes(.data$.x, .data$.y)
    plot <- ggplot2::ggplot(mapping = mapping) +
        ggplot2::theme_bw() +
        theme(
            plot.background = element_blank(),
            panel.border = element_blank(),
            strip.text = element_blank(),
            strip.background = element_blank()
        )
    if (ncol(data) > 10L) {
        plot <- plot + theme(
            axis.text.x = ggplot2::element_text(angle = -60, hjust = 0L)
        )
    }
    # add heatmap filling in the first layer
    if (filling) {
        plot <- plot + ggplot2::geom_tile(
            aes(.data$.x, .data$.y, fill = .data$value),
            width = 1L, height = 1L
        )
    }
    methods::new(
        "LayoutHeatmap",
        data = data,
        params = rlang::list2(
            xlabels = xlabels,
            xlabels_nudge = xlabels_nudge,
            ylabels = ylabels,
            ylabels_nudge = ylabels_nudge,
            filling = filling,
            # following parameters are used by patchwork
            width = set_size(width),
            height = set_size(height),
            guides = guides,
            axes = axes,
            axis_titles = axis_titles
        ),
        plot = plot
    )
}

#' @export
#' @rdname layout_heatmap
layout_heatmap.data.frame <- function(data, mapping = aes(), ...) {
    data <- as.matrix(data)
    layout_heatmap(data = data, mapping = mapping, ...)
}

#' @export
#' @rdname layout_heatmap
layout_heatmap.numeric <- function(data, mapping = aes(), ...) {
    ans <- matrix(data, ncol = 1L)
    colnames(ans) <- "V1"
    if (rlang::is_named(data)) rownames(ans) <- names(data)
    layout_heatmap(data = ans, mapping = mapping, ...)
}

#' @export
#' @rdname layout_heatmap
layout_heatmap.character <- layout_heatmap.numeric

#' @export
#' @rdname layout_heatmap
layout_heatmap.default <- function(data, mapping = aes(), ...) {
    data <- as.matrix(data)
    layout_heatmap(data = data, mapping = mapping, ...)
}

#' @export
layout_heatmap.NULL <- function(data, mapping = aes(), ...) {
    cli::cli_abort("{.arg data} must be a matrix-like object instead of `NULL`")
}

#' Subset a `LayoutHeatmap` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A `LayoutHeatmap` object
#' @param name A string of slot name in `LayoutHeatmap` object.
#' @importFrom methods slot
#' @keywords internal
methods::setMethod("$", "LayoutHeatmap", function(x, name) {
    # https://github.com/tidyverse/ggplot2/issues/6002
    if (name == "theme") {
        slot(x, "plot")$theme
    } else if (name == "plot_env") {
        slot(x, "plot")$plot_env
    } else {
        cli::cli_abort(c(
            "`$` is just for internal ggplot2 methods",
            i = "try to use `@` method instead"
        ))
    }
})

# We can remove the validation
#' @importFrom methods slot
#' @importFrom ggplot2 is.ggplot
#' @importFrom rlang is_string
methods::setValidity("LayoutHeatmap", function(object) {
    if (!is.ggplot(slot(object, "plot"))) {
        cli::cli_abort("@plot must be a {.cls ggplot} object")
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

#' @param object A `LayoutHeatmap` object.
#' @importFrom methods show
#' @export
#' @rdname ggheat
methods::setMethod("show", "LayoutHeatmap", function(object) print(object))

#' Reports whether x is a `LayoutHeatmap` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggheatmap(ggheat(1:10))
#' @export
is.ggheatmap <- function(x) methods::is(x, "LayoutHeatmap")
