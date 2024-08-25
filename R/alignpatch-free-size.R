##########################################################
#' Removing the sizes when aligning
#'
#' @inheritParams free_align
#' @param ... What ggplot2 elements to remove?
#' @export
free_size <- function(plot, ..., free_border = FALSE) {
    UseMethod("free_size")
}

#' @export
free_size.default <- function(plot, ..., free_border = FALSE) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_size.ggplot <- function(plot, ..., free_border = FALSE) {
    if (...length() == 0L) return(plot) # styler: off
    assert_bool(free_border)
    attr(plot, "free_sizes") <- check_ggelements(c(...), arg = "...")
    attr(plot, "free_border") <- free_border
    add_class(plot, "free_size")
}

#' @export
free_size.free_border <- free_size.ggplot

#' @export
free_size.free_size <- function(plot, ..., free_border = FALSE) {
    if (...length() == 0L) return(plot) # styler: off
    assert_bool(free_border)
    elements <- check_ggelements(c(...), arg = "...")
    attr(plot, "free_sizes") <- union(attr(plot, "free_sizes"), elements)
    attr(plot, "free_border") <- free_border
    plot
}

#' @export
free_size.align_wrapped <- free_size.default

##########################################################
#' @export
patch_gtable.free_size <- function(patch) {
    class(patch) <- setdiff(class(patch), "free_size")
    gt <- NextMethod()
    attr(gt, "free_sizes") <- attr(patch, "free_sizes")
    attr(gt, "free_border") <- attr(patch, "free_border")
    add_class(gt, "gtable_free_size")
}

#' @export
patch_align.gtable_free_size <- function(gt, guides) {
    class(gt) <- setdiff(class(gt), "gtable_free_size")
    ggelements <- attr(gt, "free_sizes")
    ans <- NextMethod()
    if (attr(gt, "free_border")) {
        free_borders <- names(GGELEMENTS)[
            lengths(lapply(GGELEMENTS, intersect, ggelements)) > 0L
        ]
        if (length(free_borders)) {
            # here, we attach the borders into the panel
            ans <- make_free_border(ans, guides, borders = free_borders)
        }
    }
    remove_border_sizes(ans, ggelements)
}

#' @importFrom ggplot2 find_panel
#' @export
remove_border_sizes <- function(gt, ggelements) {
    ggelements <- lapply(GGELEMENTS, intersect, ggelements)
    panel_pos <- find_panel(gt)
    for (border in names(ggelements)) {
        elements <- .subset2(ggelements, border)
        if (length(elements) == 0L) next
        pos <- .subset2(panel_pos, border) +
            .subset(
                .subset2(GGELEMENTS_RELATIVE_TO_PANEL, border),
                elements
            )
        if (border %in% c("t", "b")) {
            gt$heights[pos] <- unit(0, "mm")
        } else {
            gt$widths[pos] <- unit(0, "mm")
        }
    }
    gt
}

GGELEMENTS <- list(
    t = c("title", "subtitle", "xlab-t", "axis-t", "strip-t"),
    l = c("ylab-l", "axis-l", "strip-l"),
    b = c("caption", "xlab-b", "axis-b", "strip-b"),
    r = c("ylab-r", "axis-r", "strip-r")
)

GGELEMENTS_RELATIVE_TO_PANEL <- list(
    t = c(`axis-t` = -2L, `xlab-t` = -3L, subtitle = -6, title = -7),
    l = c(`ylab-l` = -3L, `axis-l` = -2L),
    b = c(`axis-b` = 2L, `xlab-b` = 3L, caption = 6L),
    r = c(`axis-r` = 2L, `ylab-r` = 3L)
)

# top-bottom
#
# 3: title
# 4: subtitle
# 5: guide-box-top
# 7: xlab-t
# 8: axis-t
# 10: panel
# 12: axis-b
# 13: xlab-b
# 15: guide-box-bottom
# 16: caption

# left-right
#
# 3: guide-box-left
# 5: ylab-l
# 6: axis-l
# 8: panel
# 10: axis-r
# 11: ylab-r
# 13: guide-box-right
