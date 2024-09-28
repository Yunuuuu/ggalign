#' Arrange multiple plots into a grid
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of plots, ususally the
#' ggplot object. Use `NULL` to indicate an empty spacer.
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [facet_wrap()][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byrow If `FALSE` the plots will be filled in in column-major order.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid. The
#' special value of `NA` will behave as `1null` unit unless a fixed aspect plot
#' is inserted in which case it will allow the dimension to expand or contract
#' to match the aspect ratio of the content.
#' @param design Specification of the location of areas in the layout. Can
#' either be specified as a text string or by concatenating calls to
#' [area()] together.
#' @param guides `r rd_guides()`. If `waiver()`, will inherit from the upper
#' level.
#' @inheritParams ggplot2::labs
#' @param theme `r rd_layout_theme()`
#' @return A `alignpatches` object.
#' @examples
#' # directly copied from patchwork
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' p4 <- ggplot(mtcars) +
#'     geom_bar(aes(carb))
#' p5 <- ggplot(mtcars) +
#'     geom_violin(aes(cyl, mpg, group = cyl))
#'
#' # Either add the plots as single arguments
#' align_plots(p1, p2, p3, p4, p5)
#'
#' # Or use bang-bang-bang to add a list
#' align_plots(!!!list(p1, p2, p3), p4, p5)
#'
#' # Match plots to areas by name
#' design <- "#BB
#'            AA#"
#' align_plots(B = p1, A = p2, design = design)
#'
#' # Compare to not using named plot arguments
#' align_plots(p1, p2, design = design)
#' @export
align_plots <- function(..., ncol = NULL, nrow = NULL, byrow = TRUE,
                        widths = NA, heights = NA,
                        design = NULL, guides = waiver(),
                        title = NULL, subtitle = NULL, caption = NULL,
                        theme = NULL) {
    plots <- rlang::dots_list(..., .ignore_empty = "all")
    assert_s3_class(theme, "theme", null_ok = TRUE)
    nms <- names(plots)
    if (!is.null(nms) && is.character(design)) {
        area_names <- unique(trimws(.subset2(strsplit(design, ""), 1L)))
        area_names <- sort(setdiff(area_names, c("", "#")))
        if (all(nms %in% area_names)) {
            plot_list <- vector("list", length(area_names))
            names(plot_list) <- area_names
            plot_list[nms] <- plots
            plots <- plot_list
        }
    }
    design <- as_areas(design)
    patches <- lapply(plots, alignpatch)

    # setup layout
    if (!is.waive(byrow)) assert_bool(byrow)
    if (!is.waive(design)) design <- as_areas(design)
    if (!is.waive(guides) && !is.null(guides)) {
        assert_position(guides)
        guides <- setup_pos(guides)
    }
    layout <- list(
        ncol = ncol, nrow = nrow, byrow = byrow,
        widths = widths, heights = heights, design = design,
        guides = guides
    )
    # setup annotation
    annotation <- layout_annotation(
        title = title,
        subtitle = subtitle,
        caption = caption
    )
    annotation <- annotation[
        !vapply(annotation, is.waive, logical(1L), USE.NAMES = FALSE)
    ]
    new_alignpatches(patches,
        layout = layout, annotation = annotation,
        theme = theme
    )
}

new_alignpatches <- function(patches, layout, annotation, theme) {
    structure(
        list(
            patches = patches, layout = layout,
            annotation = annotation, theme = theme
        ),
        # Will ensure serialisation includes a link to the `ggalign`
        # namespace
        `_namespace` = namespace_link,
        class = "alignpatches"
    )
}

#############################################################
#' @inherit patchwork::plot_layout
#' @inheritParams patchwork::plot_layout
#' @importFrom ggplot2 waiver
#' @noRd
layout_design <- function(ncol = waiver(), nrow = waiver(), byrow = waiver(),
                          widths = waiver(), heights = waiver(),
                          design = waiver(), guides = NA) {
    if (!is.waive(byrow)) assert_bool(byrow)
    if (!is.waive(design)) design <- as_areas(design)
    if (!identical(guides, NA) && !is.waive(guides) && !is.null(guides)) {
        assert_position(guides)
        guides <- setup_pos(guides)
    }
    structure(list(
        ncol = ncol,
        nrow = nrow,
        byrow = byrow,
        widths = widths,
        heights = heights,
        design = design,
        guides = guides
    ), class = c("layout_design", "plot_layout"))
}

#' Annotate the whole layout
#'
#' @inheritParams ggplot2::labs
#' @return A `layout_annotation` object to be added into `r rd_layout()`.
#' @importFrom ggplot2 waiver
#' @export
layout_annotation <- function(title = waiver(), subtitle = waiver(),
                              caption = waiver()) {
    structure(
        list(title = title, subtitle = subtitle, caption = caption),
        class = c("layout_annotation", "plot_annotation")
    )
}

#' Modify components of the layout theme
#'
#' Used to render the `guides`, `title`, `subtitle`, `caption`,
#' `margins` and `background`.
#' @inherit ggplot2::theme description sections references author source note format
#' @param ... Additional arguments passed to [theme()][ggplot2::theme].
#' @return A `layout_theme` object to be added to `r rd_layout()`.
#' @examples
#' layout_theme(plot.background = element_rect(fill = "green"))
#' @importFrom ggplot2 theme
#' @export
layout_theme <- function(...) add_class(ggplot2::theme(...), "layout_theme")
