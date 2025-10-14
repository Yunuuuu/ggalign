#' Modify theme of the layout
#'
#' @inherit ggplot2::theme
#' @param ... A [`theme()`][ggplot2::theme] object or additional element
#' specifications not part of base ggplot2. In general, these should also be
#' defined in the `element tree` argument. [`Splicing`][rlang::splice] a list
#' is also supported.
#'
#' @details
#' A [`theme()`][ggplot2::theme] object used to customize various elements of
#' the layout, including `guides`, `title`, `subtitle`, `caption`, `margins`,
#' `panel.border`, and `background`. By default, the theme will inherit from the
#' parent `layout`.
#'
#' - `guides`, `panel.border`, and `background` will always be used even for the
#' nested `alignpatches` object.
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
#'     layout_theme(plot.background = element_rect(fill = "red"))
#' @importFrom ggplot2 theme
#' @include ggplot-theme.R
#' @export
layout_theme <- new_theme_class("layout_theme")

S3_layout_theme <- S7::new_S3_class("layout_theme")

# nocov start
#' @importFrom S7 prop prop<-
#' @importFrom ggplot2 is_theme
prop_layout_theme <- function(...) {
    S7::new_property(
        S7::class_any,
        validator = function(value) {
            if (!is.null(value) && !is_theme(value)) {
                return("must be a 'theme()' object'")
            }
        },
        ...,
        default = NULL
    )
}
# nocov end

##############################################################
#' Add layout annotation
#'
#' This function is a placeholder for future extensions.
#' If you're trying to apply a theme, use [layout_theme()] instead.
#'
#' @param ... Currently unused. May accept a theme in the future.
#' @param theme A theme object. If not `waiver()`, an error will be raised.
#'
#' @return None. This function is used for input validation.
#' @importFrom ggplot2 is_theme is_waiver
#' @export
#' @keywords internal
layout_annotation <- function(..., theme = waiver()) {
    if (is_theme(...elt(1L)) || !is_waiver(theme)) {
        cli_abort("Please use {.fn layout_theme} instead; {.fn layout_annotation} is reserved for future extensions.")
    }
}

##############################################################
#' Arrange multiple plots into a grid
#'
#' An internal S7 class that represents a collection of aligned plots
#' along with their layout configuration, titles, tags, and theme.
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> A list of plots, ususally the
#' ggplot object. Use `NULL` to indicate an empty spacer. Each input must
#' implement the [`alignpatch()`] method.
#' @param ncol,nrow The dimensions of the grid to create - if both are `NULL` it
#' will use the same logic as [`facet_wrap()`][ggplot2::facet_wrap] to set the
#' dimensions
#' @param byrow If `FALSE` the plots will be filled in in column-major order.
#' @param widths,heights The relative widths and heights of each column and row
#' in the grid. Will get repeated to match the dimensions of the grid. The
#' special value of `NA` will behave as `1null` unit unless a fixed aspect plot
#' is inserted in which case it will allow the dimension to expand or contract
#' to match the aspect ratio of the content.
#' @param area Specification of the location of areas in the layout. Can
#' either be specified as a text string or by concatenating calls to
#' [`area()`] together.
#' @param guides A string with one or more of `r oxford_and(c(.tlbr, "i"))`
#' indicating which side of guide legends should be collected. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If
#' there is no parent layout, or if `NULL` is provided, no guides will be
#' collected.
#' @param theme A [`theme()`][ggplot2::theme] object used to customize various
#' elements of the layout. By default, the theme will inherit from the parent
#' `layout`.
#' @param design An alias for `area`, retained for backward compatibility.
#' @return An `alignpatches` object.
#' @seealso
#'  - [layout_design()]
#'  - [layout_title()]
#'  - [layout_theme()]
#'  - [layout_tags()]
#'
#' @section Properties:
#' - **plots**: A list of plot objects.
#' - **layout**: A list specifying layout options, including:
#'   - `ncol`, `nrow`, `byrow`: grid layout parameters.
#'   - `widths`, `heights`: relative dimensions of rows/columns.
#'   - `area`: custom area specification.
#'   - `guides`: guide handling.
#' - **titles**: A list specifying title options (`title`, `subtitle`,
#'   `caption`).
#' - **tags**: A list specifying tag options (`tags`, `sep`, `prefix`,
#'   `suffix`).
#' - **theme**: A theme configuration object.
#'
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
#' area <- "#BB
#'           AA#"
#' align_plots(B = p1, A = p2, area = area)
#'
#' # Compare to not using named plot arguments
#' align_plots(p1, p2, area = area)
#'
#' @importFrom S7 new_object S7_object prop prop<-
#' @importFrom ggplot2 waiver
#' @include alignpatch-design.R
#' @include alignpatch-title.R
#' @include alignpatch-tags.R
#' @export
alignpatches <- S7::new_class(
    "alignpatches",
    properties = list(
        plots = S7::class_list,
        layout = layout_design,
        titles = layout_title,
        tags = layout_tags,
        theme = prop_layout_theme()
    ),
    constructor = function(..., ncol = NULL, nrow = NULL, byrow = TRUE,
                           widths = NA, heights = NA, area = NULL,
                           guides = waiver(), theme = NULL, design = NULL) {
        plots <- rlang::dots_list(..., .ignore_empty = "all", .named = NULL)
        nms <- names(plots)
        area <- area %||% design
        if (!is.null(nms) && is.character(area)) { # nocov start
            area_names <- unique(trimws(.subset2(strsplit(area, ""), 1L)))
            area_names <- sort(vec_set_difference(area_names, c("", "#")))
            if (all(nms %in% area_names)) {
                plot_list <- vector("list", length(area_names))
                names(plot_list) <- area_names
                plot_list[nms] <- plots
                plots <- plot_list
            }
        } # nocov end

        # setup layout parameters
        layout <- layout_design(
            ncol = ncol, nrow = nrow, byrow = byrow,
            widths = widths, heights = heights, area = area,
            guides = guides
        )
        if (is.null(theme)) {
            theme <- layout_theme()
        } else {
            theme <- layout_theme(theme)
        }
        new_object(
            S7_object(),
            plots = plots, layout = layout,
            titles = layout_title(), tags = layout_tags(),
            theme = theme
        )
    }
)

#' @export
#' @rdname alignpatches
align_plots <- alignpatches

#' @importFrom S7 prop
local(S7::method(`$`, alignpatches) <- function(x, i) prop(x, i))

#' @importFrom S7 prop
local(S7::method(`[[`, alignpatches) <- function(x, i) prop(x, i))

#' @importFrom S7 props
local(S7::method(`[`, alignpatches) <- function(x, i) {
    .subset(props(x), i)
})
