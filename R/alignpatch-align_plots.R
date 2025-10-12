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
                          area = waiver(), guides = NA, design = waiver()) {
    if (!is.waive(ncol)) {
        assert_number_whole(ncol, min = 1, allow_na = TRUE, allow_null = TRUE)
    }
    if (!is.waive(nrow)) {
        assert_number_whole(nrow, min = 1, allow_na = TRUE, allow_null = TRUE)
    }
    if (!is.waive(byrow)) assert_bool(byrow)
    area <- area %|w|% design
    if (!is.waive(area)) area <- as_areas(area)
    if (!identical(guides, NA) && !is.waive(guides) && !is.null(guides)) {
        assert_guides(guides)
    }
    structure(
        list(
            ncol = ncol,
            nrow = nrow,
            byrow = byrow,
            widths = widths,
            heights = heights,
            area = area,
            guides = guides
        ),
        class = c("layout_design", "plot_layout")
    )
}

S3_layout_design <- S7::new_S3_class("layout_design")

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
    if (!is.waive(title)) assert_string(title, allow_null = TRUE)
    if (!is.waive(subtitle)) assert_string(subtitle, allow_null = TRUE)
    if (!is.waive(caption)) assert_string(caption, allow_null = TRUE)
    structure(
        list(title = title, subtitle = subtitle, caption = caption),
        class = c("layout_title", "plot_annotation")
    )
}

S3_layout_title <- S7::new_S3_class("layout_title")

prop_layout_title <- function(...) {
    S7::new_property(
        S7::class_list,
        ...,
        default = quote(list(title = NULL, subtitle = NULL, caption = NULL))
    )
}

##############################################################
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
#' Control Plot Tagging in Layouts
#'
#' These arguments control how tags (labels) are assigned to plots within a
#' layout, including nested layouts. Tags can be inherited from a parent
#' layout, suppressed entirely, or generated automatically in various
#' sequences. Formatting can be customized with separators, prefixes, and
#' suffixes.
#'
#' The appearance of tags is controlled by the `plot.tag`, `plot.tag.position`,
#' and `plot.tag.location` theme elements. Tag styling is first retrieved from
#' the plot's theme; if not found there, the layout's theme is used.
#'
#' @param tags Tag templates for plots in the layout.
#'   If `waiver()` (default), tags are inherited from the parent layout.
#'   If there is no parent layout, no tags are applied.
#'
#'   If `NULL`, tags are suppressed for this layout.
#'   In a nested layout, the parent layout's tag is applied to the
#'   the entire layout as a single unit.
#'
#'   If not `NULL`, must be one of:
#'   - A character vector specifying explicit tags for each plot, or
#'   - A single character indicating an auto-generated sequence:
#'       * `'a'`: lowercase letters
#'       * `'A'`: uppercase letters
#'       * `'1'`: numbers
#'       * `'i'`: lowercase Roman numerals
#'       * `'I'`: uppercase Roman numerals
#'
#'   When a parent layout exists, each plot's tag is prefixed with the parent
#'   tag and separated by `sep`.
#'
#' @param sep Separator between the parent tag (without its own `prefix` and
#'     `suffix`) and the current tag.
#' @param prefix String prepended to the tag.
#' @param suffix String appended to the tag.
#' @examples
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p3 <- ggplot(mtcars) +
#'     geom_bar(aes(gear)) +
#'     facet_wrap(~cyl)
#'
#' # Add tags to plots, by default the plot in nested layout will get own tag
#' align_plots(p1, align_plots(p2, p3), ncol = 1) + layout_tags("A")
#'
#' # Treat a nested layout as a single plot by disabling its internal tags
#' align_plots(p1, align_plots(p2, p3) + layout_tags(NULL), ncol = 1) +
#'     layout_tags("A")
#'
#' # Apply multilevel tagging — outer layout uses letters, inner layout uses
#' # numbers
#' align_plots(
#'     p1,
#'     align_plots(p2, p3) + layout_tags(1),
#'     ncol = 1
#' ) +
#'     layout_tags("A")
#'
#' # Use a custom tag sequence, possibly mixed with standard sequences
#' align_plots(
#'     p1,
#'     align_plots(p2, p3) + layout_tags(1),
#'     ncol = 1
#' ) +
#'     layout_tags(c("&", "%"))
#' @importFrom rlang is_na
#' @export
layout_tags <- function(tags = NA, sep = waiver(),
                        prefix = waiver(), suffix = waiver()) {
    if (!is_na(tags) && !is.waive(tags) && !is.null(tags)) {
        tags <- as.character(tags)
        if (length(tags) == 1L) {
            tags <- switch(tags,
                a = letters,
                A = LETTERS,
                "1" = 1:100,
                i = tolower(utils::as.roman(1:100)),
                I = utils::as.roman(1:100),
                cli_abort("Unknown tag type: {.val {x}}")
            )
        }
    }
    if (!is.waive(sep)) assert_string(sep, allow_null = TRUE)
    if (!is.waive(prefix)) assert_string(prefix, allow_null = TRUE)
    if (!is.waive(suffix)) assert_string(suffix, allow_null = TRUE)
    structure(
        list(
            tags = tags, sep = sep,
            prefix = prefix, suffix = suffix
        ),
        class = "layout_tags"
    )
}

S3_layout_tags <- S7::new_S3_class("layout_tags")

create_layout_tagger <- function(tags, parent) {
    # If no parent and no tags, return NULL
    if (is.null(parent) &&
        (is.null(.subset2(tags, "tags")) || is.waive(.subset2(tags, "tags")))) {
        return(NULL)
    }

    # If has parent and tags are waived, inherit parent directly
    if (!is.null(parent) && is.waive(.subset2(tags, "tags"))) {
        return(parent)
    }

    # If has parent and tags are `NULL`, take parent's resolved tag as whole
    if (!is.null(parent) && is.null(.subset2(tags, "tags"))) {
        return(parent$tag())
    }

    # Compose combined prefix, suffix, and separator
    prefix <- .subset2(tags, "prefix")
    suffix <- .subset2(tags, "suffix")
    if (!is.null(parent)) {
        prefix <- paste0(parent$prefix, prefix)
        suffix <- paste0(suffix, parent$suffix)
        prefix <- paste0(prefix, parent$resolve_tag(), .subset2(tags, "sep"))
    }
    ggproto(
        "LayoutTagger", NULL,
        tags = .subset2(tags, "tags"),
        prefix = prefix,
        suffix = suffix,
        index = 1L,
        n = length(.subset2(tags, "tags")),
        tag = function(self) {
            paste0(self$prefix, self$resolve_tag(), self$suffix)
        },
        resolve_tag = function(self) {
            if (self$index > self$n) {
                cli_warn("Not enough {.field tags} supplied; recycling {.field tags}.")
                self$index <- 1L
            }
            out <- .subset(self$tags, self$index)
            self$index <- self$index + 1L
            out
        },
        tag_table = function(self, table, theme) {
            label <- self$tag()
            table_add_tag(table, label, theme)
        }
    )
}

inherit_tag_theme <- function(theme, parent) {
    for (el in c("plot.tag", "plot.tag.position", "plot.tag.location")) {
        # This is like doing t1[[item]] <- x, except that it preserves NULLs.
        # The other form will simply drop NULL values
        theme[el] <- list(theme[[el]] %||% parent[[el]])
    }
    theme
}

tag_theme <- function(th) {
    if (is.null(th)) {
        theme(
            plot.tag = NULL,
            plot.tag.position = NULL,
            plot.tag.location = NULL
        )
    } else {
        theme(
            plot.tag = th$plot.tag,
            plot.tag.position = th$plot.tag.position,
            plot.tag.location = th$plot.tag.location
        )
    }
}

#' @importFrom ggplot2 is_theme_element calc_element theme element_grob
#' @importFrom gtable gtable_add_grob
#' @importFrom rlang arg_match0
table_add_tag <- function(table, label, theme) {
    # Early exit when label is absent or element is blank
    if (length(label) < 1L) {
        return(table)
    }
    element <- calc_element("plot.tag", theme)
    if (is_theme_element(element, "blank")) {
        return(table)
    }

    # Resolve position
    position <- calc_element("plot.tag.position", theme) %||% "topleft"
    location <- calc_element("plot.tag.location", theme) %||%
        (if (is.numeric(position)) "plot" else "margin")

    if (is.numeric(position)) {
        if (location == "margin") {
            cli_abort(
                paste0(
                    "A {.cls numeric} {.arg plot.tag.position} cannot be used",
                    " with `{.val margin}` as {.arg plot.tag.location}."
                ),
                call = quote(theme())
            )
        }
        if (length(position) != 2L) {
            cli_abort("A {.cls numeric} {.arg plot.tag.position} must be of length 2", call = quote(theme()))
        }
        top <- left <- right <- bottom <- FALSE
    } else {
        # Break position into top/left/right/bottom
        position <- arg_match0(
            position[1],
            c(
                "topleft", "top", "topright", "left",
                "right", "bottomleft", "bottom", "bottomright"
            ),
            arg_nm = "plot.tag.position",
            error_call = quote(theme())
        )
        top <- position %in% c("topleft", "top", "topright")
        left <- position %in% c("topleft", "left", "bottomleft")
        right <- position %in% c("topright", "right", "bottomright")
        bottom <- position %in% c("bottomleft", "bottom", "bottomright")
    }

    # Resolve tag and sizes
    tag <- element_grob(
        element,
        label = label,
        margin_y = TRUE,
        margin_x = TRUE
    )
    height <- grobHeight(tag)
    width <- grobWidth(tag)

    if (location %in% c("plot", "panel")) {
        if (!is.numeric(position)) {
            hjust <- ggfun("try_prop")(element, "hjust", default = 0.5)
            if (right || left) {
                x <- (1 - hjust) * width
                if (right) {
                    x <- unit(1, "npc") - x
                }
            } else {
                x <- unit(hjust, "npc")
            }
            if (top || bottom) {
                vjust <- ggfun("try_prop")(element, "vjust", default = 0.5)
                y <- (1 - vjust) * height
                if (top) {
                    y <- unit(1, "npc") - y
                }
            } else {
                y <- unit(vjust, "npc")
            }
        } else {
            x <- unit(position[1], "npc")
            y <- unit(position[2], "npc")
        }
        # Re-render with manual positions, why?
        tag <- element_grob(
            element,
            x = x, y = y, label = label,
            margin_y = TRUE, margin_x = TRUE
        )
        if (location == "plot") {
            # without margin
            table <- gtable_add_grob(
                table, tag,
                name = "tag", clip = "off",
                t = 2L, b = nrow(table) - 1L,
                l = 2L, r = ncol(table) - 1L
            )
            return(table)
        }
    }

    if (location == "panel") {
        place <- find_panel(table)
    } else {
        n_col <- ncol(table)
        n_row <- nrow(table)
        # Actually fill margin with relevant units
        if (top) table$heights[2L] <- max(height, table$heights[2L])
        if (left) table$widths[2L] <- max(width, table$widths[2L])
        if (right) {
            table$widths[n_col - 1L] <- max(table$widths[n_col - 1L], width)
        }
        if (bottom) {
            table$heights[n_row - 1L] <- max(table$heights[n_row - 1L], height)
        }
        place <- data_frame0(t = 2L, r = n_col - 1L, b = n_row - 1L, l = 2L)
    }

    # Shrink placement to position
    if (top) place$b <- place$t
    if (left) place$r <- place$l
    if (right) place$l <- place$r
    if (bottom) place$t <- place$b

    gtable_add_grob(
        table, tag,
        name = "tag", clip = "off",
        t = place$t, l = place$l, b = place$b, r = place$r
    )
}

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
#' @importFrom ggplot2 is_theme
#' @export
#' @keywords internal
layout_annotation <- function(..., theme = waiver()) {
    if (is_theme(...elt(1L)) || !is.waive(theme)) {
        cli_abort("Please use {.fn layout_theme} instead; {.fn layout_annotation} is reserved for future extensions.")
    }
}

##############################################################
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
#' @return An [alignpatches][class_alignpatches] object.
#' @seealso
#'  - [layout_design()]
#'  - [layout_title()]
#'  - [layout_theme()]
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
#' @importFrom ggplot2 update_ggplot
#' @export
align_plots <- function(..., ncol = NULL, nrow = NULL, byrow = TRUE,
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
    for (plot in plots) {
        if (!has_s3_method(plot, "alignpatch", default = FALSE)) {
            cli_abort(paste(
                "Each plot to be aligned must implement an {.fn alignpatch}",
                "method. Object of {.obj_type_friendly {plot}} does not."
            ))
        }
    }
    out <- class_alignpatches(plots = plots)

    # setup layout parameters
    layout <- layout_design(
        ncol = ncol, nrow = nrow, byrow = byrow,
        widths = widths, heights = heights, area = area,
        guides = guides
    )
    out <- update_ggplot(layout, out)
    objectname <- deparse(substitute(theme))
    if (!is.null(theme)) {
        out <- update_ggplot(layout_theme(theme), out, objectname)
    }
    out
}

#' The `alignpatches` class
#'
#' An internal S7 class that represents a collection of aligned plots
#' along with their layout configuration, titles, tags, and theme.
#' It is primarily used within the `ggalign` system for plot composition
#' and layout management.
#'
#' @section Properties:
#' - **plots**: A list of plot objects.
#' - **layout**: A list specifying layout options, including:
#'   - `ncol`, `nrow`, `byrow`: grid layout parameters.
#'   - `widths`, `heights`: relative dimensions of rows/columns.
#'   - `area`: custom area specification.
#'   - `guides`: guide handling (default: [waiver()]).
#' - **titles**: A list specifying title options (`title`, `subtitle`,
#'   `caption`).
#' - **tags**: A list specifying tag options (`tags`, `sep`, `prefix`,
#'   `suffix`).
#' - **theme**: A theme configuration object.
#'
#' @importFrom S7 new_object S7_object prop prop<-
#' @keywords internal
#' @export
class_alignpatches <- S7::new_class(
    "alignpatches",
    properties = list(
        plots = S7::class_list,
        layout = S7::new_property(
            S7::class_list,
            default = quote(list(
                ncol = NULL, nrow = NULL, byrow = TRUE,
                widths = NA, heights = NA, area = NULL,
                guides = waiver()
            ))
        ),
        titles = prop_layout_title(),
        tags = S7::new_property(
            S7::class_list,
            default = quote(list(
                tags = waiver(), sep = NULL, prefix = NULL, suffix = NULL
            ))
        ),
        theme = prop_layout_theme()
    )
)

local(S7::method(`$`, class_alignpatches) <- function(x, name) prop(x, name))
local(S7::method(`[[`, class_alignpatches) <- function(x, name) prop(x, name))
