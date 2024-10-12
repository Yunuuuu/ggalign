#' Arrange multiple plots into a grid
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of plots, ususally the
#' ggplot object. Use `NULL` to indicate an empty spacer.
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [`facet_wrap()`][ggplot2::facet_wrap] to set the
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
#' @param guides `r rd_guides()`
#' @return An `alignpatches` object.
#' @seealso
#'  - [layout_design()]
#'  - [layout_title()]
#'  - [layout_annotation()]
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
                        design = NULL, guides = waiver()) {
    plots <- rlang::dots_list(..., .ignore_empty = "all")
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
    for (plot in plots) {
        if (!has_method(plot, "alignpatch", default = FALSE)) {
            cli::cli_abort("Cannot align {.obj_type_friendly {plot}}")
        }
    }

    # setup layout parameters
    assert_bool(byrow)
    design <- as_areas(design)
    if (!is.waive(guides) && !is.null(guides)) {
        assert_position(guides)
        guides <- setup_pos(guides)
    }
    layout <- list(
        ncol = ncol, nrow = nrow, byrow = byrow,
        widths = widths, heights = heights, design = design,
        guides = guides
    )
    new_alignpatches(plots, layout = layout)
}

new_alignpatches <- function(patches, layout = NULL,
                             titles = NULL, annotation = NULL,
                             theme = NULL) {
    layout <- layout %||% list(
        ncol = NULL, nrow = NULL, byrow = TRUE,
        widths = NA, heights = NA,
        design = NULL, guides = waiver()
    )
    titles <- titles %||% list(title = NULL, subtitle = NULL, caption = NULL)
    annotation <- annotation %||% list(
        top = NULL, left = NULL, bottom = NULL, right = NULL
    )
    structure(
        list(
            patches = patches, # the input plot list
            layout = layout, # the layout design
            annotation = annotation, #
            titles = titles,
            theme = theme
        ),
        # Will ensure serialisation includes a link to the `ggalign`
        # namespace
        `_namespace` = namespace_link,
        class = "alignpatches"
    )
}

#' @export
`+.alignpatches` <- function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    alignpatches_add(e2, e1, e2name)
}

alignpatches_add <- function(object, plot, object_name) {
    UseMethod("alignpatches_add")
}

#############################################################
#' Define the grid to compose plots in
#'
#' To control how different plots are laid out, you need to add a layout design
#' specification. If you are nesting grids, the layout is scoped to the current
#' nesting level.
#' @inheritParams align_plots
#' @return A `layout_design` object.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(p1, p2, p3) +
#'     layout_design(nrow = 1L)
#' align_plots(p1, p2, p3) +
#'     layout_design(ncol = 1L)
#' @importFrom ggplot2 waiver
#' @export
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

#' @importFrom utils modifyList
alignpatches_update <- function(old, new) {
    modifyList(old, new[!vapply(new, is.waive, logical(1L))], keep.null = TRUE)
}

update_layout_design <- function(old, new) {
    guides <- .subset2(new, "guides")
    new <- .subset(new, setdiff(names(new), "guides"))
    old <- alignpatches_update(old, new)
    if (!identical(guides, NA)) old$guides <- guides
    old
}

#' @export
alignpatches_add.layout_design <- function(object, plot, object_name) {
    plot$layout <- update_layout_design(.subset2(plot, "layout"), object)
    plot
}

#' @export
alignpatches_add.plot_layout <- function(object, plot, object_name) {
    object <- .subset(object, names(layout_design()))
    if (is.waive(object$guides)) {
        object$guides <- NA
    } else if (identical(object$guides, "auto")) {
        object$guides <- waiver()
    } else if (identical(object$guides, "collect")) {
        object$guides <- .TLBR
    } else if (identical(object$guides, "keep")) {
        object["guides"] <- list(NULL)
    }
    plot$layout <- update_layout_design(.subset2(plot, "layout"), object)
    plot
}

##############################################################
#' Annotate the whole layout
#'
#' @inheritParams ggplot2::labs
#' @return A `layout_title` object.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(p1, p2, p3) +
#'     layout_title(title = "I'm title")
#' @importFrom ggplot2 waiver
#' @export
layout_title <- function(title = waiver(), subtitle = waiver(),
                         caption = waiver()) {
    structure(
        list(title = title, subtitle = subtitle, caption = caption),
        class = c("layout_title", "plot_annotation")
    )
}

#' @export
alignpatches_add.layout_title <- function(object, plot, object_name) {
    plot$titles <- alignpatches_update(.subset2(plot, "titles"), object)
    plot
}

##############################################################
#' Modify components of the layout
#'
#' - modify the theme of the layout
#'
#' @param theme A [`theme()`][ggplot2::theme] used to render the `guides`,
#' `title`, `subtitle`, `caption`, `margins`, `patch.title`, `panel.border`, and
#' `background`.
#' @inheritParams rlang::args_dots_empty
#'
#' @details
#' - `guides`, `patch.title`, `panel.border`, and `background` will always be
#' added even for the nested `alignpatches` object.
#'
#' - `title`, `subtitle`, `caption`, and `margins` will be added for the
#' top-level `alignpatches` object only.
#'
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#' align_plots(
#'     p1 + theme(plot.background = element_blank()),
#'     p2 + theme(plot.background = element_blank()),
#'     p3 + theme(plot.background = element_blank())
#' ) +
#'     layout_annotation(
#'         theme = theme(plot.background = element_rect(fill = "red"))
#'     )
#' @importFrom ggplot2 waiver
#' @export
layout_annotation <- function(theme = waiver(), ...) {
    rlang::check_dots_empty()
    if (!is.null(theme) && !is.waive(theme)) assert_s3_class(theme, "theme")
    structure(
        list(annotation = list(), theme = theme),
        class = c("layout_annotation", "plot_annotation")
    )
}

update_layout_theme <- function(old, new) {
    if (is.waive(new)) return(old) # styler: off
    if (is.null(old) || is.null(new)) return(new) # styler: off
    ggfun("add_theme")(old, new)
}

#' @export
alignpatches_add.layout_annotation <- function(object, plot, object_name) {
    plot$annotation <- alignpatches_update(
        .subset2(plot, "annotation"),
        .subset2(object, "annotation")
    )
    plot$theme <- update_layout_theme(
        .subset2(plot, "theme"),
        .subset2(object, "theme")
    )
    plot
}

#' @export
alignpatches_add.plot_annotation <- function(object, plot, object_name) {
    plot$titles <- alignpatches_update(
        .subset2(plot, "titles"),
        .subset(object, names(layout_title()))
    )
    plot$theme <- update_layout_theme(.subset2(plot, "theme"), theme)
    plot
}
