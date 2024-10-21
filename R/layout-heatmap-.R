#' Arrange plots in a Heatmap
#'
#' `ggheatmap` is an alias of `heatmap_layout`.
#'
#' @param data A numeric or character vector, a data frame, and any other data
#' which can be converted into a matrix. Simple vector will be converted into a
#' one column matrix. If `missing`, will inherit from the parent layout.
#' @param mapping Default list of aesthetic mappings to use for plot. In
#' addition, we will always add mapping `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments passed to
#' [geom_tile()][ggplot2::geom_tile]/[geom_raster()][ggplot2::geom_raster]. Only
#' used when `filling = TRUE`.
#' @param .width,.height `r rd_heatmap_size()`.
#' @param action A [plot_action()] object used to define the default plot action
#' in the layout.
#' @inheritParams align_plots
#' @param filling A boolean value indicating whether to fill the heatmap. If you
#' wish to customize the filling style, set this to `FALSE`.
#'
#' By default, the classic heatmap colour scheme
#' [`scale_fill_gradient2(low = "blue", high = "red")`][ggplot2::scale_fill_gradient2]
#' is utilized for continuous values.
#' You can use the option
#' `r rd_values(sprintf("%s.heatmap_continuous_fill", pkg_nm()))` or
#' `r rd_values(sprintf("%s.heatmap_discrete_fill", pkg_nm()))` to modify the
#' default heatmap body fill color scale. See
#' [`scale_fill_continuous()`][ggplot2::scale_fill_continuous] or
#' [`scale_fill_discrete()`][ggplot2::scale_fill_discrete] for option setting
#' details.
#'
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
#' @importFrom lifecycle deprecated
#' @export
heatmap_layout <- function(data, mapping = aes(),
                           ..., .width = NA, .height = NA,
                           action = NULL, theme = NULL, filling = TRUE,
                           set_context = TRUE, order = NULL, name = NULL,
                           guides = deprecated()) {
    if (missing(data)) {
        .heatmap_layout(
            data = NULL, mapping = mapping,
            ..., .width = .width, .height = .height,
            action = action, theme = theme, filling = filling,
            set_context = set_context, order = order, name = name,
            guides = guides,
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
        data = "ANY", plot = "ANY", body_action = "ANY",
        # parameters for heatmap body
        width = "ANY", height = "ANY", filling = "ANY",
        # If we regard heatmap layout as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        set_context = "logical", order = "integer", name = "character",
        # Used by the layout itself:
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

#' @export
heatmap_layout.NULL <- function(data, ...) {
    .heatmap_layout(
        data = data, nobs_list = list(),
        ..., call = current_call()
    )
}

#' @export
heatmap_layout.functon <- heatmap_layout.NULL

#' @export
heatmap_layout.formula <- function(data, ...) {
    .heatmap_layout(
        data = allow_lambda(data), ...,
        nobs_list = list(), call = current_call()
    )
}

#' @export
heatmap_layout.default <- function(data, ...) {
    data <- fortify_heatmap(data)
    .heatmap_layout(
        data = data, ...,
        nobs_list = list(x = ncol(data), y = nrow(data)),
        call = current_call()
    )
}

#' @importFrom lifecycle deprecated
#' @importFrom vctrs vec_cast
#' @importFrom ggplot2 aes
.heatmap_layout <- function(data, mapping = aes(),
                            ...,
                            .width = NA, .height = NA,
                            action = NULL, theme = NULL, filling = TRUE,
                            set_context = TRUE, order = NULL, name = NULL,
                            guides = deprecated(),
                            # following parameters are used internally
                            nobs_list, call = caller_call()) {
    width <- check_size(.width, call = call)
    height <- check_size(.height, call = call)
    action <- check_action(action, FALSE, call = call)
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    assert_bool(filling, call = call)
    assert_bool(set_context, call = call)
    order <- check_order(order, call = call)
    assert_string(name, empty_ok = FALSE, na_ok = TRUE, null_ok = TRUE)
    if (lifecycle::is_present(guides)) {
        lifecycle::deprecate_warn(
            "0.0.5", "ggheatmap(guides)", "ggheatmap(action)"
        )
        assert_layout_position(guides, call = call)
        action$guides <- guides
    }
    plot <- ggplot2::ggplot(
        mapping = add_default_mapping(mapping, aes(.data$.x, .data$.y))
    ) +
        # always remove default axis titles -------------------
        # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
        # There are multiple ways to set labels in a plot, which take different
        # priorities. Here are the priorities from highest to lowest.
        # 1. The guide title.
        # 2. The scale name.
        # 3. The `labs()` function.
        # 4. The captured expression in aes().
        ggplot2::labs(x = NULL, y = NULL)
    # save the `geom_tile()`/geom_raster() parameters
    if (filling) {
        filling_params <- rlang::list2(...)
    } else {
        filling_params <- NULL
    }

    # Here we use S4 object to override the double dispatch of `+.gg` method
    methods::new(
        "HeatmapLayout",
        data = data,
        theme = theme, action = action, # used by the layout
        body_action = default_action(TRUE), # used by heatmap body
        # following parameters can be controlled by `active` object.
        width = width, height = height,
        # following parameters used when adding ggheamtap to ggstack
        set_context = set_context,
        order = order, name = name %||% NA_character_,
        # following parameters are used internally
        plot = plot, nobs_list = nobs_list,
        filling = filling_params
    )
}

#' Reports whether `x` is a [heatmap_layout()] object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is_ggheatmap(ggheatmap(1:10))
#' @export
is_ggheatmap <- function(x) methods::is(x, "HeatmapLayout")
