#' Arrange plots around a Heatmap
#'
#' `ggheatmap` is an alias of `layout_heatmap`.
#'
#' @param data A numeric or character vector, a data frame, and any other data
#' which can be converted into a matrix. Simple vector will be converted into a
#' one column matrix.
#' @param mapping Default list of aesthetic mappings to use for plot. If `NULL`,
#' will using `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments passed to [geom_tile][ggplot2::geom_tile].
#' Only used when `filling = TRUE`.
#' @param filling A boolean value indicates whether to fill the heatmap. If you
#' want to custom the filling style, you can set to `FALSE`.
#' @inheritParams align
#' @inheritParams ggplot2::ggplot
#' @section ggplot2 specification:
#' The data input in `ggheatmap` will be converted into the long formated data
#' frame when drawing. The default mapping will use `aes(.data$.x, .data$.y)`,
#' you can use `mapping` argument to control it. The data in the underlying
#' `ggplot` object contains following columns:
#'
#'  - `.xpanel` and `.ypanel`: the column and row panel
#'
#'  - `.x` and `.y`: the `x` and `y` coordinates
#'
#'  - `.row_names` and `.column_names`: the row and column names of the original
#'    matrix (only applicable when names exist).
#'
#'  - `.row_index` and `.column_index`: the row and column index of the original
#'    matrix.
#'
#'  - `value`: the actual matrix value.
#'
#' @return A `HeatmapLayout` object.
#' @examples
#' ggheatmap(1:10)
#' ggheatmap(letters)
#' @importFrom ggplot2 aes
#' @export
layout_heatmap <- function(data, mapping = aes(),
                           ...,
                           filling = TRUE,
                           set_context = TRUE, order = NULL, name = NULL,
                           environment = parent.frame()) {
    if (missing(data)) {
        .layout_heatmap(
            data = NULL, mapping = mapping,
            ..., filling = filling,
            environment = environment,
            set_context = set_context, order = order, name = name,
            nobs_list = list(), call = current_call()
        )
    } else {
        UseMethod("layout_heatmap")
    }
}

#' @export
print.HeatmapLayout <- function(x, ...) {
    p <- ggalign_build(x)
    print(p, ...)
    invisible(x)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.HeatmapLayout <- function(x, recording = TRUE) {
    grid.draw(ggalign_build(x), recording = recording)
}

# used to create the heatmap layout
#' @keywords internal
methods::setClass(
    "HeatmapLayout",
    contains = "Layout",
    list(
        data = "ANY",
        plot = "ANY",
        facetted_pos_scales = "ANY",
        params = "list",
        set_context = "logical",
        order = "integer",
        name = "character",
        # Used by the layout,
        # top, left, bottom, right must be a StackLayout object.
        top = "ANY", left = "ANY",
        bottom = "ANY", right = "ANY",
        panel_list = "list", index_list = "list", nobs_list = "list"
    ),
    prototype = list(
        top = NULL, left = NULL,
        bottom = NULL, right = NULL,
        panel_list = list(), index_list = list(),
        nobs_list = list()
    )
)

#' @export
#' @rdname layout_heatmap
ggheatmap <- layout_heatmap

#' @importFrom ggplot2 waiver theme
#' @export
layout_heatmap.matrix <- function(data, ...) {
    .layout_heatmap(
        data = data, ...,
        nobs_list = list(x = ncol(data), y = nrow(data)),
        call = current_call()
    )
}

#' @export
layout_heatmap.NULL <- function(data, ...) {
    .layout_heatmap(
        data = data, nobs_list = list(),
        ..., call = current_call()
    )
}

#' @export
layout_heatmap.formula <- function(data, ...) {
    .layout_heatmap(
        data = allow_lambda(data), ...,
        nobs_list = list(), call = current_call()
    )
}

#' @export
layout_heatmap.functon <- layout_heatmap.NULL

#' @export
layout_heatmap.default <- function(data, ...) {
    call <- current_call()
    data <- tryCatch(
        as.matrix(data),
        error = function(cnd) {
            cli::cli_abort(paste(
                "{.arg data} must be a matrix-like object but you provide",
                "{.obj_type_friendly {data}}"
            ), call = call)
        }
    )
    .layout_heatmap(
        data = data, ...,
        nobs_list = list(x = ncol(data), y = nrow(data)),
        call = call
    )
}

#' @importFrom ggplot2 aes
.layout_heatmap <- function(data, mapping = aes(),
                            ...,
                            filling = TRUE,
                            set_context = TRUE, order = NULL, name = NULL,
                            environment = parent.frame(),
                            # following parameters are used internally
                            nobs_list, call = caller_call()) {
    assert_bool(filling, call = call)
    assert_bool(set_context, call = call)
    if (is.null(order) || is.na(order)) {
        order <- NA_integer_
    } else if (!is_scalar(order)) {
        cli::cli_abort("{.arg order} must be a single number", call = call)
    } else if (is.double(order)) {
        order <- as.integer(order)
    } else if (!is.integer(order)) {
        cli::cli_abort("{.arg order} must be a single number", call = call)
    }
    plot <- ggplot2::ggplot(mapping = mapping) +
        heatmap_theme()
    plot <- add_default_mapping(plot, aes(.data$.x, .data$.y))

    # add heatmap filling in the first layer
    if (filling) {
        if (is.null(.subset2(plot$mapping, "fill"))) {
            tile_mapping <- aes(.data$.x, .data$.y, fill = .data$value)
        } else {
            tile_mapping <- aes(.data$.x, .data$.y)
        }
        plot <- plot + ggplot2::geom_tile(mapping = tile_mapping, ...)
    }

    # Here we use S4 object to override the double dispatch of `+.gg` method
    methods::new(
        "HeatmapLayout",
        data = data,
        params = list(
            # following parameters can be controlled by `active` object.
            width = unit(NA, "null"),
            height = unit(NA, "null"),
            guides = waiver(),
            free_labs = waiver(),
            free_sizes = waiver(),
            plot_data = waiver()
        ),
        set_context = set_context,
        order = order, name = name %||% NA_character_,
        plot = plot, nobs_list = nobs_list,
        # following parameters are used by ggplot methods
        # like `ggsave` and `ggplot_build`
        theme = default_theme(),
        plot_env = environment
    )
}

#' Reports whether `x` is a `HeatmapLayout` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggheatmap(ggheatmap(1:10))
#' @export
is.ggheatmap <- function(x) methods::is(x, "HeatmapLayout")
