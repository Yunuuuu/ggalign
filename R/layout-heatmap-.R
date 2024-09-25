#' Arrange plots in a Heatmap
#'
#' `ggheatmap` is an alias of `heatmap_layout`.
#'
#' @param data A numeric or character vector, a data frame, and any other data
#' which can be converted into a matrix. Simple vector will be converted into a
#' one column matrix.
#' @param mapping Default list of aesthetic mappings to use for plot. If `NULL`,
#' will using `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments passed to [geom_tile][ggplot2::geom_tile].
#' Only used when `filling = TRUE`.
#' @param width,height Heatmap body width/height, can be a [unit][grid::unit]
#' object.
#' @param filling A boolean value indicates whether to fill the heatmap. If you
#' want to customize the filling style, you can set to `FALSE`.
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
#'  - `.row_names` and `.column_names`: A factor of the row and column names of
#'    the original matrix (only applicable when names exist).
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
#' ggheatmap(matrix(rnorm(81), nrow = 9L))
#' @importFrom ggplot2 aes
#' @export
heatmap_layout <- function(data, mapping = aes(),
                           ...,
                           width = NA, height = NA, filling = TRUE,
                           set_context = TRUE, order = NULL, name = NULL) {
    if (missing(data)) {
        .heatmap_layout(
            data = NULL, mapping = mapping,
            ..., width = width, height = height, filling = filling,
            set_context = set_context, order = order, name = name,
            nobs_list = list(), call = current_call()
        )
    } else {
        UseMethod("heatmap_layout")
    }
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
        # If we regard heatmap layout as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        set_context = "logical", order = "integer", name = "character",
        # Used by the layout itself,
        # top, left, bottom, right must be a StackLayout object.
        top = "ANY", left = "ANY", bottom = "ANY", right = "ANY",
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
#' @rdname heatmap_layout
ggheatmap <- heatmap_layout

#' @importFrom ggplot2 waiver
#' @export
heatmap_layout.matrix <- function(data, ...) {
    .heatmap_layout(
        data = data, ...,
        nobs_list = list(x = ncol(data), y = nrow(data)),
        call = current_call()
    )
}

#' @export
heatmap_layout.NULL <- function(data, ...) {
    .heatmap_layout(
        data = data, nobs_list = list(),
        ..., call = current_call()
    )
}

#' @export
heatmap_layout.formula <- function(data, ...) {
    .heatmap_layout(
        data = allow_lambda(data), ...,
        nobs_list = list(), call = current_call()
    )
}

#' @export
heatmap_layout.functon <- heatmap_layout.NULL

#' @importFrom rlang try_fetch
#' @export
heatmap_layout.default <- function(data, ...) {
    call <- current_call()
    data <- try_fetch(
        as.matrix(data),
        error = function(cnd) {
            cli::cli_abort(paste(
                "{.arg data} must be a matrix-like object but you provide",
                "{.obj_type_friendly {data}}"
            ), call = call)
        }
    )
    .heatmap_layout(
        data = data, ...,
        nobs_list = list(x = ncol(data), y = nrow(data)),
        call = call
    )
}

#' @importFrom vctrs vec_cast
#' @importFrom ggplot2 aes
.heatmap_layout <- function(data, mapping = aes(),
                            ...,
                            width = NA, height = NA, filling = TRUE,
                            set_context = TRUE, order = NULL, name = NULL,
                            # following parameters are used internally
                            nobs_list, call = caller_call()) {
    width <- check_size(width)
    height <- check_size(height)
    assert_bool(filling, call = call)
    assert_bool(set_context, call = call)
    if (is.null(order) || is.na(order)) {
        order <- NA_integer_
    } else if (!is_scalar(order)) {
        cli::cli_abort("{.arg order} must be a single number", call = call)
    } else {
        order <- vec_cast(order, integer(), call = call)
    }
    assert_string(name, empty_ok = FALSE, na_ok = TRUE, null_ok = TRUE)
    plot <- ggplot2::ggplot(mapping = mapping) +
        heatmap_theme()
    plot <- add_default_mapping(plot, aes(.data$.x, .data$.y)) +
        # always remove default axis titles -------------------
        # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
        # There are multiple ways to set labels in a plot, which take different
        # priorities. Here are the priorities from highest to lowest.
        # 1. The guide title.
        # 2. The scale name.
        # 3. The `labs()` function.
        # 4. The captured expression in aes().
        ggplot2::labs(x = NULL, y = NULL)

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
            width = width,
            height = height,
            # following parameters can be controlled by `active` object.
            guides = waiver(),
            free_labs = waiver(),
            free_spaces = waiver(),
            plot_data = waiver(),
            theme = waiver()
        ),
        set_context = set_context,
        order = order, name = name %||% NA_character_,
        plot = plot, nobs_list = nobs_list,
        # used by ggsave
        theme = NULL
    )
}

#' Reports whether `x` is a [heatmap_layout()] object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggheatmap(ggheatmap(1:10))
#' @export
is.ggheatmap <- function(x) methods::is(x, "HeatmapLayout")
