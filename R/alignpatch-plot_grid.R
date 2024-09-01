#' Arrange multiple plots into a grid
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of plots, ususally the
#' ggplot object.
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byrow If `FALSE` the plots will be filled in in column-major order.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid. The
#' special value of `NA⁠` will behave as `⁠1null⁠` unless a fixed aspect plot is
#' inserted in which case it will allow the dimension to expand or contract to
#' match the aspect ratio of the content.
#' @param design Specification of the location of areas in the layout. Can
#' either be specified as a text string or by concatenating calls to
#' [area()][patchwork::area] together.
#' @param guides A single boolean value indicates whether to collect the guides,
#' or you can specify the string of the guide position to collect. Allowed
#' strings are: `r rd_values(BORDERS)`.
#' @inheritParams ggplot2::labs
#' @param theme `r rd_theme()`
#' @return A `alignpatches` object.
#' @export
plot_grid <- function(..., ncol = NULL, nrow = NULL, byrow = TRUE, widths = NA,
                      heights = NA, design = NULL, guides = NULL,
                      title = NULL, subtitle = NULL, caption = NULL,
                      theme = NULL) {
    plots <- rlang::list2(...)
    assert_bool(byrow)
    guides <- check_guides(guides)
    assert_s3_class(theme, "theme", null_ok = TRUE)
    if (!is.null(design)) design <- as_areas(design)
    nms <- names(plots)
    if (!is.null(nms) && !is.null(design) && is.character(design)) {
        area_names <- unique(trimws(.subset2(strsplit(design, ""), 1L)))
        area_names <- sort(setdiff(area_names, c("", "#")))
        if (all(nms %in% area_names)) {
            plot_list <- vector("list", length(area_names))
            names(plot_list) <- area_names
            plot_list[nms] <- plots
            plots <- plot_list
        }
    }
    plots <- lapply(plots, as_patch)
    new_alignpatches(plots, list(
        ncol = ncol,
        nrow = nrow,
        byrow = byrow,
        widths = widths,
        heights = heights,
        design = design, guides = guides,
        theme = theme,
        title = title, subtitle = subtitle, caption = caption
    ))
}

new_alignpatches <- function(plots, layout) {
    structure(
        list(
            plots = plots, layout = layout,
            # Will ensure serialisation includes a link to the `ggalign`
            # namespace
            `_namespace` = ggalign_namespace_link
        ),
        class = "alignpatches"
    )
}
