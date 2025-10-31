#' Control Plot Tagging in Layouts
#'
#' These arguments control how tags (labels) are assigned to plots within a
#' layout, including nested layouts. Tags can be inherited from a parent
#' layout, suppressed entirely, or generated automatically in various
#' sequences. Formatting can be customized with separators, prefixes, and
#' suffixes.
#'
#' The appearance of tags is controlled by the `plot.tag`, `plot.tag.placement`,
#' `plot.tag.position`, and `plot.tag.location` theme elements. Tag styling is
#' first retrieved from the plot's theme; if not found there, the layout's theme
#' is used.
#'
#' `plot.tag.placement` (new in `ggalign`) determines where the tag is
#' positioned—either within the plot itself (`"plot"`) or on the canvas where
#' all plots are placed (`"canvas"`). When set to `"canvas"`, the tag will be
#' fixed and does not move with the plots, even when using `free_vp()` or other
#' helper functions. This ensures that all tags are consistently aligned across
#' the canvas.
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
    properties = c(
        list(tags = S7::new_property(
            S7::new_union(S7::class_character, NULL, S3_waiver),
            setter = function(self, value) {
                if (!is_waiver(value) && !is.null(value)) {
                    value <- as.character(value)
                }
                prop(self, "tags", check = FALSE) <- value
                self
            },
            default = character()
        )),
        lapply(
            rlang::set_names(c("sep", "prefix", "suffix")),
            function(x) {
                S7::new_property(
                    S7::new_union(S3_waiver, NULL, S7::class_character),
                    validator = function(value) {
                        if (!is_waiver(value) && !is.null(value) &&
                            length(value) != 1L) {
                            return("must be a single character string")
                        }
                    },
                    default = quote(waiver())
                )
            }
        )
    )
)

#' @importFrom S7 prop prop<-
#' @include generics.R
S7::method(ggalign_init, layout_tags) <- function(x) {
    if (is.character(prop(x, "tags")) && length(prop(x, "tags")) == 0L) {
        prop(x, "tags", check = FALSE) <- waiver()
    }
    prop(x, "sep", check = FALSE) <- prop(x, "sep") %|w|% NULL
    prop(x, "prefix", check = FALSE) <- prop(x, "prefix") %|w|% NULL
    prop(x, "suffix", check = FALSE) <- prop(x, "suffix") %|w|% NULL
    x
}

#' @importFrom ggplot2 is_waiver
#' @importFrom S7 prop prop<-
#' @include generics.R
S7::method(ggalign_update, list(layout_tags, layout_tags)) <-
    function(x, object) {
        if (!(is.character(prop(object, "tags")) &&  # styler: off
              length(prop(object, "tags")) == 0L)) { # styler: off
            prop(x, "tags", check = FALSE) <- prop(object, "tags")
        }
        if (!is_waiver(prop(object, "sep"))) {
            prop(x, "sep", check = FALSE) <- prop(object, "sep")
        }
        if (!is_waiver(prop(object, "prefix"))) {
            prop(x, "prefix", check = FALSE) <- prop(object, "prefix")
        }
        if (!is_waiver(prop(object, "suffix"))) {
            prop(x, "suffix", check = FALSE) <- prop(object, "suffix")
        }
        x
    }

local(
    S7::method(`+`, list(layout_tags, layout_tags)) <-
        function(e1, e2) ggalign_update(e1, e2)
)

#' @importFrom ggplot2 ggproto is_waiver
#' @importFrom S7 prop
create_layout_tagger <- function(tags, tagger) {
    # initialize the tags
    tags <- ggalign_init(tags)

    # If no parent and no tags, return NULL
    if (!is_tagger(tagger) &&
        (is.null(prop(tags, "tags")) || is_waiver(prop(tags, "tags")))) {
        return(NULL)
    }

    # If has parent and tags are waived, inherit parent directly
    if (is_tagger(tagger) && is_waiver(prop(tags, "tags"))) {
        return(tagger)
    }

    # If has parent and tags are `NULL`, take parent's resolved tag as whole
    if (is_tagger(tagger) && is.null(prop(tags, "tags"))) {
        return(tagger$tag())
    }

    # Compose combined prefix, suffix, and separator
    prefix <- prop(tags, "prefix")
    suffix <- prop(tags, "suffix")
    if (is_tagger(tagger)) {
        prefix <- paste0(tagger$prefix, prefix)
        suffix <- paste0(suffix, tagger$suffix)
        prefix <- paste0(prefix, tagger$resolve_tag(), prop(tags, "sep"))
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
        "ggalign::LayoutTagger", NULL,
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
        }
    )
}

is_tagger <- function(x) inherits(x, "ggalign::LayoutTagger")

#' @importFrom S7 S7_data
#' @importFrom ggplot2 theme
tag_theme <- function(th) {
    if (is.null(th)) {
        theme()
    } else {
        th <- S7_data(th)
        th <- .subset(th, intersect(c(
            "plot.tag", "plot.tag.placement",
            "plot.tag.position", "plot.tag.location"
        ), names(th)))
        theme(!!!th)
    }
}

#' @importFrom ggplot2 is_theme_element calc_element theme element_grob
#' @importFrom gtable gtable_add_grob
#' @importFrom rlang arg_match0
table_add_tag <- function(table, label, theme, t, l, b, r, z) {
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
                t = t + 1L, b = b - 1L,
                l = l + 1L, r = r - 1L,
                z = z
            )
            return(table)
        }
    }

    if (location == "panel") {
        place <- data_frame0(
            t = t + TOP_BORDER, b = b - BOTTOM_BORDER,
            l = l + LEFT_BORDER, r = r - RIGHT_BORDER
        )
    } else {
        place <- data_frame0(
            t = t + 1L, b = b - 1L,
            l = l + 1L, r = r - 1L
        )
        # Actually fill margin with relevant units
        if (top) table$heights[place$t] <- max(height, table$heights[place$t])
        if (left) table$widths[place$l] <- max(width, table$widths[place$l])
        if (right) {
            table$widths[place$b] <- max(table$widths[place$b], width)
        }
        if (bottom) {
            table$heights[place$r] <- max(table$heights[place$r], height)
        }
    }

    # Shrink placement to position
    if (top) place$b <- place$t
    if (left) place$r <- place$l
    if (right) place$l <- place$r
    if (bottom) place$t <- place$b

    gtable_add_grob(
        table, tag,
        name = "tag", clip = "off",
        t = place$t, l = place$l, b = place$b, r = place$r,
        z = z
    )
}
