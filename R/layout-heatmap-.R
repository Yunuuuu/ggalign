#' Arrange plots in a Heatmap
#'
#' `ggheatmap` is an alias of `heatmap_layout`.
#'
#' @param data A numeric or character vector, a data frame, and any other data
#' which can be converted into a matrix. Simple vector will be converted into a
#' one column matrix. If `missing`, will inherit from the parent layout.
#' @param mapping Default list of aesthetic mappings to use for plot. In
#' addition, we will always add mapping `aes(.data$.x, .data$.y)`.
#' @param ... Additional arguments passed to [`fortify_heatmap()`].
#' @param width,height `r rd_heatmap_size()`.
#' @param action A [plot_action()] object used to define the default plot action
#' in the layout.
#' @inheritParams align_plots
#' @param filling A single string of
#' `r rd_values(c("raster", "tile"), final = "or")` to indicate the filling
#' style. By default, `waiver()` is used, which means that if the input matrix
#' has more than 20,000 cells (`nrow * ncol > 20000`),
#' [`geom_raster()`][ggplot2::geom_raster] will be used for performance
#' efficiency; for smaller matrices, [`geom_tile()`][ggplot2::geom_tile] will be
#' used. To customize the filling style, set this to `NULL`.
#'
#' For backward compatibility, a single boolean value is acceptable: `TRUE`
#' means `waiver()`, and `FALSE` means `NULL`.
#'
#' By default, the classic heatmap color scheme
#' [`scale_fill_gradient2(low = "blue", high = "red")`][ggplot2::scale_fill_gradient2]
#' is utilized for continuous values.
#'
#' You can use the options
#' `r rd_values(sprintf("%s.heatmap_continuous_fill", pkg_nm()))` or
#' `r rd_values(sprintf("%s.heatmap_discrete_fill", pkg_nm()))` to modify the
#' default heatmap body filling color scale. See
#' [`scale_fill_continuous()`][ggplot2::scale_fill_continuous] or
#' [`scale_fill_discrete()`][ggplot2::scale_fill_discrete] for details on
#' option settings.
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
heatmap_layout <- function(data = NULL, mapping = aes(),
                           ...,
                           width = NA, height = NA,
                           action = NULL, theme = NULL, filling = waiver(),
                           set_context = TRUE, order = NULL, name = NULL,
                           guides = deprecated()) {
    UseMethod("heatmap_layout")
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

#' @importFrom lifecycle deprecated
#' @importFrom vctrs vec_cast
#' @importFrom ggplot2 aes
#' @importFrom rlang arg_match0
#' @export
heatmap_layout.default <- function(data = NULL, mapping = aes(),
                                   ...,
                                   width = NA, height = NA,
                                   action = NULL, theme = NULL,
                                   filling = waiver(),
                                   set_context = TRUE, order = NULL,
                                   name = NULL,
                                   guides = deprecated()) {
    # prepare data --------------------------------------
    data <- fortify_heatmap(data = data, ...)

    # check arguments -----------------------------------
    width <- check_size(width)
    height <- check_size(height)
    action <- check_action(action)
    if (!is.null(theme)) assert_s3_class(theme, "theme")
    # A single boolean value for compatible with version <= 0.0.4
    if (isTRUE(filling)) {
        filling <- waiver()
    } else if (isFALSE(filling)) {
        filling <- NULL
    } else if (!is.waive(filling) && !is.null(filling)) {
        filling <- arg_match0(filling, c("tile", "raster"))
    }
    assert_bool(set_context)
    order <- check_order(order)
    assert_string(name, empty_ok = FALSE, na_ok = TRUE, null_ok = TRUE)
    if (lifecycle::is_present(guides)) {
        lifecycle::deprecate_warn(
            "0.0.5", "ggheatmap(guides)", "ggheatmap(action)"
        )
        assert_layout_position(guides)
        action$guides <- guides
    }
    # initialize nobs --------------------------------
    if (is.null(data) || is.function(data)) {
        nobs_list <- list(x = NULL, y = NULL)
    } else {
        nobs_list <- list(x = ncol(data), y = nrow(data))
    }
    # initialize the heatmap body plot
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

    # Here we use S4 object to override the double dispatch of `+.gg` method
    methods::new(
        "HeatmapLayout",
        data = data,
        theme = theme, action = action, # used by the layout
        body_action = default_action(), # used by heatmap body
        # following parameters can be controlled by `active` object.
        width = width, height = height,
        # following parameters used when adding ggheamtap to ggstack
        set_context = set_context,
        order = order, name = name %||% NA_character_,
        # following parameters are used internally
        plot = plot, nobs_list = nobs_list,
        filling = filling
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
