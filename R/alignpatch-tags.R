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
#' @include utils-ggplot.R
#' @export
layout_tags <- S7::new_class("layout_tags",
    properties = list(
        tags = S7::new_property(
            S7::new_union(S7::class_character, NULL, S3_waiver),
            setter = function(self, value) {
                if (is_na(value)) {
                    value <- NA_character_
                } else if (!is_waiver(value) && !is.null(value)) {
                    value <- as.character(value)
                }
                prop(self, "tags", check = FALSE) <- value
                self
            },
            default = NA_character_
        ),
        sep = S7::new_property(
            S7::new_union(S3_waiver, S7::class_character, NULL),
            setter = function(self, value) {
                if (!is_waiver(value)) {
                    assert_string(value, allow_null = TRUE, arg = "@sep")
                }
                prop(self, "sep", check = FALSE) <- value
                self
            },
            default = quote(waiver())
        ),
        prefix = S7::new_property(
            S7::new_union(S3_waiver, S7::class_character, NULL),
            setter = function(self, value) {
                if (!is_waiver(value)) {
                    assert_string(value, allow_null = TRUE, arg = "@prefix")
                }
                prop(self, "prefix", check = FALSE) <- value
                self
            },
            default = quote(waiver())
        ),
        suffix = S7::new_property(
            S7::new_union(S3_waiver, S7::class_character, NULL),
            setter = function(self, value) {
                if (!is_waiver(value)) {
                    assert_string(value, allow_null = TRUE, arg = "@suffix")
                }
                prop(self, "suffix", check = FALSE) <- value
                self
            },
            default = quote(waiver())
        )
    )
)

#' @importFrom S7 prop prop<-
S7::method(on_init, layout_tags) <- function(input) {
    if (identical(prop(input, "tags"), NA_character_)) {
        prop(input, "tags", check = FALSE) <- waiver()
    }
    prop(input, "sep", check = FALSE) <- prop(input, "sep") %|w|% NULL
    prop(input, "prefix", check = FALSE) <- prop(input, "prefix") %|w|% NULL
    prop(input, "suffix", check = FALSE) <- prop(input, "suffix") %|w|% NULL
    input
}

#' @importFrom ggplot2 is_waiver
#' @importFrom S7 props
local(
    S7::method(`+`, list(layout_tags, layout_tags)) <-
        function(e1, e2) {
            if (!identical(prop(e2, "tags"), NA_character_)) {
                prop(e1, "tags", check = FALSE) <- prop(e2, "tags")
            }
            if (!is_waiver(prop(e2, "sep"))) {
                prop(e1, "sep", check = FALSE) <- prop(e2, "sep")
            }
            if (!is_waiver(prop(e2, "prefix"))) {
                prop(e1, "prefix", check = FALSE) <- prop(e2, "prefix")
            }
            if (!is_waiver(prop(e2, "suffix"))) {
                prop(e1, "suffix", check = FALSE) <- prop(e2, "suffix")
            }
            e1
        }
)

#' @importFrom ggplot2 ggproto is_waiver
#' @importFrom S7 prop
create_layout_tagger <- function(tags, parent) {
    # initialize the tags
    tags <- on_init(tags)

    # If no parent and no tags, return NULL
    if (is.null(parent) &&
        (is.null(prop(tags, "tags")) || is_waiver(prop(tags, "tags")))) {
        return(NULL)
    }

    # If has parent and tags are waived, inherit parent directly
    if (!is.null(parent) && is_waiver(prop(tags, "tags"))) {
        return(parent)
    }

    # If has parent and tags are `NULL`, take parent's resolved tag as whole
    if (!is.null(parent) && is.null(prop(tags, "tags"))) {
        return(parent$tag())
    }

    # Compose combined prefix, suffix, and separator
    prefix <- prop(tags, "prefix")
    suffix <- prop(tags, "suffix")
    if (!is.null(parent)) {
        prefix <- paste0(parent$prefix, prefix)
        suffix <- paste0(suffix, parent$suffix)
        prefix <- paste0(prefix, parent$resolve_tag(), prop(tags, "sep"))
    }
    tags <- prop(tags, "tags")
    if (length(tags) == 1L) {
        tags <- switch(tags,
            a = letters,
            A = LETTERS,
            "1" = 1:100,
            i = tolower(utils::as.roman(1:100)),
            I = utils::as.roman(1:100),
            tags
        )
    }
    ggproto(
        "LayoutTagger", NULL,
        tags = tags,
        prefix = prefix,
        suffix = suffix,
        index = 1L,
        n = length(tags),
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
