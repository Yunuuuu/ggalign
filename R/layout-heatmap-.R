#' Arrange plots around a Heatmap
#'
#' `ggheatmap` is an alias of `layout_heatmap`.
#'
#' @param data A numeric or character vector, a data frame, and any other data
#' which can be converted into a matrix. Simple vector will be converted into a
#' one column matrix.
#' @param mapping Default list of aesthetic mappings to use for plot. If `NULL`,
#' will using `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments passed to matrix method.
#' @param width,height Heatmap body width/height, can be a [unit][grid::unit]
#' object.
#' @inheritParams plot_grid
#' @param free_labs A boolean value or a character of the axis position (`"t"`,
#' `"l"`, `"b"`, `"r"`) indicates which axis title should be free from
#' alignment. By default, all axis title won't be aligned.
#' @param free_sizes A character specifies the ggplot elements which won't
#' count space sizes when alignment.
#' @param filling A boolean value indicates whether to fill the heatmap. If you
#' want to custom the filling style, you can set to `FALSE`.
#' @param plot_data A function used to transform the plot data before rendering.
#' By default, it'll inherit from the parent layout. If no parent layout, the
#' default is to not modify the data. Use `NULL`, if you don't want to modify
#' anything.
#'
#' Used to modify the data after layout has been created, but before the data is
#' handled of to the ggplot2 for rendering. Use this hook if the you needs
#' change the default data for all `geoms`.
#' @param ... Additional arguments passed to [geom_tile][ggplot2::geom_tile].
#' Only used when `filling = TRUE`.
#' @inheritParams align
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
                           width = NULL, height = NULL,
                           guides = waiver(),
                           free_labs = waiver(), free_sizes = waiver(),
                           plot_data = waiver(), filling = TRUE,
                           ..., set_context = TRUE, order = NULL, name = NULL) {
    if (missing(data)) {
        .layout_heatmap(
            data = NULL, mapping = mapping,
            width = width, height = height,
            guides = guides, free_labs = free_labs,
            free_sizes = free_sizes, plot_data = plot_data,
            filling = filling, ..., set_context = set_context,
            order = order, name = name, nobs_list = list(),
            call = current_call()
        )
    } else {
        UseMethod("layout_heatmap")
    }
}

#' @export
print.HeatmapLayout <- function(x, ...) {
    p <- build_alignpatches(x)
    print(p, ...)
    invisible(x)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.HeatmapLayout <- function(x, ...) {
    print(x, ...)
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
    .layout_heatmap(data = data, ..., call = current_call())
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
        data = allow_lambda(data), nobs_list = list(),
        ..., call = current_call()
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
    .layout_heatmap(data = data, ..., call = call)
}

#' @importFrom ggplot2 aes
.layout_heatmap <- function(data,
                            mapping = aes(),
                            width = NULL, height = NULL,
                            guides = waiver(),
                            free_labs = waiver(), free_sizes = waiver(),
                            plot_data = waiver(), filling = TRUE,
                            ...,
                            set_context = TRUE, order = NULL, name = NULL,
                            nobs_list = list(x = ncol(data), y = nrow(data)),
                            call = caller_call()) {
    assert_bool(filling, call = call)

    if (!is.waive(free_labs)) {
        free_labs <- check_layout_labs(free_labs, call = call)
    }
    if (!is.waive(free_sizes)) {
        free_sizes <- check_ggelements(free_sizes, call = call)
    }
    if (is.null(width)) {
        width <- unit(NA, "null")
    } else {
        width <- check_size(width)
    }
    if (is.null(height)) {
        height <- unit(NA, "null")
    } else {
        height <- check_size(height)
    }
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
    assert_string(name,
        empty_ok = FALSE, na_ok = FALSE,
        null_ok = TRUE, call = call
    )
    plot_data <- check_plot_data(plot_data)
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
            width = width,
            height = height,
            guides = guides,
            free_labs = free_labs,
            free_sizes = free_sizes,
            plot_data = plot_data
        ),
        set_context = set_context,
        order = order, name = name %||% NA_character_,
        plot = plot,
        nobs_list = nobs_list
    )
}

#' Subset a `HeatmapLayout` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A `HeatmapLayout` object
#' @param name A string of slot name in `HeatmapLayout` object.
#' @importFrom methods slot
#' @keywords internal
methods::setMethod("$", "HeatmapLayout", function(x, name) {
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

#' Reports whether `x` is a `HeatmapLayout` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggheatmap(ggheatmap(1:10))
#' @export
is.ggheatmap <- function(x) methods::is(x, "HeatmapLayout")
