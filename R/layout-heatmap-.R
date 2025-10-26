#' Create a heatmap
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `heatmap_layout` is a specialized version of [`quad_discrete()`], which
#' simplifies the creation of heatmap plots by integrating essential elements
#' for a standard heatmap layout, ensuring that the appropriate data mapping and
#' visualization layers are automatically applied. `ggheatmap` is an alias for
#' `heatmap_layout`.
#'
#' @param data `r rd_layout_data()`. By default, it will try to inherit from
#' parent layout. [`fortify_matrix()`] will be used to convert data to a
#' matrix.
#' @param ... Additional arguments passed to [`fortify_matrix()`].
#' @inheritParams quad_layout
#' @param filling A single string of `r oxford_or(c("raster", "tile"))` to
#' indicate the filling style. By default, `waiver()` is used, which means that
#' if the input matrix has more than 20,000 cells (`nrow * ncol > 20000`),
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
#' `r code_quote(sprintf("%s.heatmap_continuous_fill", pkg_nm()))` or
#' `r code_quote(sprintf("%s.heatmap_discrete_fill", pkg_nm()))` to modify the
#' default heatmap body filling color scale. See
#' [`scale_fill_continuous()`][ggplot2::scale_fill_continuous] or
#' [`scale_fill_discrete()`][ggplot2::scale_fill_discrete] for details on
#' option settings.
#'
#' @section ggplot2 specification:
#' The data input will be converted to a matrix using [`fortify_matrix()`], and
#' the data in the underlying main plot will contain the following columns:
#'
#'  - `.panel_x` and `.panel_y`: the column and row panel groups.
#'
#'  - `.x` and `.y`: an integer index of `x` and `y` coordinates
#'
#'  - `.discrete_x` and `.discrete_y`: a factor of the data labels (only
#'    applicable when `.row_names` and `.column_names` exists).
#'
#'  - `.row_names` and `.column_names`: A character of the row and column names
#'    of the original matrix (only applicable when names exist).
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
heatmap_layout <- function(data = NULL, mapping = aes(),
                           ...,
                           width = deprecated(), height = deprecated(),
                           filling = waiver(),
                           theme = NULL, active = deprecated()) {
    UseMethod("heatmap_layout")
}

#' @usage NULL
#' @export
#' @rdname heatmap_layout
ggheatmap <- heatmap_layout

#' @importFrom ggplot2 aes
#' @importFrom rlang arg_match0
#' @export
heatmap_layout.default <- function(data = NULL, mapping = aes(),
                                   ...,
                                   width = deprecated(), height = deprecated(),
                                   filling = waiver(),
                                   theme = NULL, active = deprecated()) {
    HeatmapLayout(
        name = "ggheatmap",
        data = data, mapping = mapping, ...,
        width = width, height = height,
        filling = filling,
        theme = theme,
        active = active
    )
}

# used to create the heatmap layout
#' @include layout-quad-.R
HeatmapLayout <- S7::new_class(
    "HeatmapLayout", QuadLayout,
    properties = list(filling = S7::class_any),
    constructor = function(..., filling = waiver()) {
        # A single boolean value for compatible with `version <= 0.0.4`
        if (isTRUE(filling)) {
            filling <- waiver()
        } else if (isFALSE(filling)) {
            filling <- NULL
        } else if (!is_waiver(filling) && !is.null(filling)) {
            filling <- arg_match0(filling, c("tile", "raster"))
        }
        ans <- new_object(quad_discrete(...), filling = filling)
        # add default mapping
        prop(prop(ans, "graph"), "plot", check = FALSE) <- ggadd_default(
            prop(prop(ans, "graph"), "plot"),
            mapping = aes(.data$.x, .data$.y)
        )
        ans
    }
)
