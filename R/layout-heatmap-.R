#' Arrange Plots in a Heatmap
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `heatmap_layout` is a specialized version of [`quad_alignb()`], which
#' simplifies the creation of heatmap plots by integrating essential elements
#' for a standard heatmap layout, ensuring that the appropriate data mapping and
#' visualization layers are automatically applied. `ggheatmap` is an alias for
#' `heatmap_layout`.
#'
#' @param data `r rd_layout_data()`. By default, it will try to inherit from
#' parent layout. If not already a matrix, will be converted to one by
#' [`fortify_matrix()`].
#' @param ... Additional arguments passed to [`fortify_matrix()`].
#' @inheritParams quad_free
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
#' @inheritParams align_gg
#' @param guides `r lifecycle::badge("deprecated")` Please use
#' [`scheme_align()`] function instead.
#' @section ggplot2 specification:
#' The default mapping will use `aes(.data$.x, .data$.y)`, you can use `mapping`
#' argument to control it. The data in the underlying `ggplot` object contains
#' following columns:
#'
#'  - `.xpanel` and `.ypanel`: the column and row panel
#'
#'  - `.x` and `.y`: an integer index of `x` and `y` coordinates
#'
#'  - `.discrete_x` and `.discrete_y`: a factor of the data labels (only
#'    applicable when `.row_names`` and `.column_names` exists).
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
                           width = NA, height = NA, filling = waiver(),
                           theme = NULL, active = NULL,
                           set_context = deprecated(),
                           order = deprecated(), name = deprecated(),
                           guides = deprecated()) {
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
                                   width = NA, height = NA, filling = waiver(),
                                   theme = NULL, active = NULL,
                                   set_context = deprecated(),
                                   order = deprecated(), name = deprecated(),
                                   guides = deprecated()) {
    # A single boolean value for compatible with `version <= 0.0.4`
    if (isTRUE(filling)) {
        filling <- waiver()
    } else if (isFALSE(filling)) {
        filling <- NULL
    } else if (!is.waive(filling) && !is.null(filling)) {
        filling <- arg_match0(filling, c("tile", "raster"))
    }
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
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    active <- deprecate_active(active, "ggheatmap",
        set_context = set_context, order = order, name = name
    )
    ans <- new_quad_layout(
        name = "ggheatmap",
        data = data,
        horizontal = new_layout_coords(nobs = nrows),
        vertical = new_layout_coords(nobs = ncols),
        mapping = mapping,
        theme = theme, active = active,
        width = width, height = height,
        class = "HeatmapLayout"
    )
    if (lifecycle::is_present(guides)) {
        lifecycle::deprecate_warn(
            "0.0.5", "ggheatmap(guides)", "scheme_align()"
        )
        assert_layout_position(guides)
        ans@schemes$scheme_align["guides"] <- list(guides)
    }
    # add default mapping
    ans@plot <- ggadd_default(ans@plot, mapping = aes(.data$.x, .data$.y)) +
        ggplot2::labs(x = NULL, y = NULL)
    ans@filling <- filling
    ans
}

# used to create the heatmap layout
#' @keywords internal
#' @include layout-quad-.R
methods::setClass(
    "HeatmapLayout",
    contains = "QuadLayout",
    list(filling = "ANY") # parameters for heatmap body
)
