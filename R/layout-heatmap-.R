#' Arrange Plots in a Heatmap
#'
#' `r lifecycle::badge('stable')` `heatmap_layout` is a specialized version of
#' [`quad_alignb()`], which simplifies the creation of heatmap plots by
#' integrating essential elements for a standard heatmap layout, ensuring that
#' the appropriate data mapping and visualization layers are automatically
#' applied. `ggheatmap` is an alias for `heatmap_layout`.
#'
#' @param data `r rd_layout_data()`. If not already a matrix, will be converted
#' to one by [`fortify_matrix()`].
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
#' [`plot_align()`] function instead.
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
heatmap_layout <- function(data = NULL, mapping = aes(),
                           ...,
                           width = NA, height = NA, filling = waiver(),
                           theme = NULL, context = NULL,
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
                                   theme = NULL, context = NULL,
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
    assert_context(context)
    context <- update_context(context, new_context(
        active = TRUE, order = NA_integer_, name = NA_character_
    ))
    context <- deprecate_context(context, "ggheatmap",
        set_context = set_context, order = order, name = name
    )
    ans <- new_quad_layout(
        name = "ggheatmap",
        data = data,
        horizontal = new_layout_params(nobs = nrows),
        vertical = new_layout_params(nobs = ncols),
        mapping = mapping, theme = theme, context = context,
        width = width, height = height,
        class = "HeatmapLayout"
    )
    if (lifecycle::is_present(guides)) {
        lifecycle::deprecate_warn(
            "0.0.5", "ggheatmap(guides)", "plot_align()"
        )
        assert_layout_position(guides)
        ans@controls$plot_align["guides"] <- list(guides)
    }
    # always remove default axis titles
    # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
    # There are multiple ways to set labels in a plot, which take different
    # priorities. Here are the priorities from highest to lowest.
    # 1. The guide title.
    # 2. The scale name.
    # 3. The `labs()` function.
    # 4. The captured expression in aes().
    ans@plot <- ans@plot + ggplot2::labs(x = NULL, y = NULL)
    # add default mapping
    ans@plot$mapping <- add_default_mapping(
        ans@plot$mapping, aes(.data$.x, .data$.y)
    )
    ans@filling <- filling
    ans
}
