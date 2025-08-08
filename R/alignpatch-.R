# here is copied from patchwork
# we modified the `patchwork` package for following reasons:
# 1. collect guides for each side (should be merged into patchwork, not allowed
#    to be merged: https://github.com/thomasp85/patchwork/issues/379).
# 2. `free_*()` functions: see https://github.com/thomasp85/patchwork/issues/379
#     - `free_align()`: added
#     - `free_border()`: not added
#     - `free_lab()`: added
#     - `free_space()`: added
#     - `free_vp()`: not added
# 3. Added titles around the plot top, left, bottom, and right
#    (`patch_titles()`)
TABLE_ROWS <- 18L + 2L
TABLE_COLS <- 15L + 2L

TOP_BORDER <- 9L + 1L
LEFT_BORDER <- 7L + 1L
BOTTOM_BORDER <- 8L + 1L
RIGHT_BORDER <- 7L + 1L

# top-bottom
# 1: margin
# 2: tag
# 3: title
# 4: subtitle
# 5: guide-box-top
# 6: legend.box.spacing
# feature: insert patch title
# 7: xlab-t
# strip.placement = "inside"/"outside"
# 8: axis-t/strip-t
# 9: strip-t/axis-t
# 10: panel
# 11: strip-b
# 12: axis-b
# 13: xlab-b
# feature: insert patch title
# 14: legend.box.spacing
# 15: guide-box-bottom
# 16: caption
# 17: tag
# 18: margin

# left-right
#
# 1: margin
# 2: tag
# 3: guide-box-left
# 4: legend.box.spacing
# feature: insert patch title
# 5: ylab-l
# 6: axis-l
# 8: panel
# 10: axis-r
# 11: ylab-r
# feature: insert patch title
# 12: legend.box.spacing
# 13: guide-box-right
# 14: tag
# 15: margin

.TLBR <- c("top", "left", "bottom", "right")
.tlbr <- c("t", "l", "b", "r")

# position is a single string contains `.tlbr`
setdiff_position <- function(x, y) gsub(sprintf("[%s]", y), "", x)
union_position <- function(x, y) paste0(x, gsub(sprintf("[%s]", x), "", y))
split_position <- function(x) {
    vec_unique(.subset2(strsplit(x, "", fixed = TRUE), 1L))
}

# pos is an atomic character
setup_pos <- function(x) unname(complete_pos(split_position(x)))

setup_guides <- function(x) {
    .subset(
        c(t = "top", l = "left", b = "bottom", r = "right", i = "inside"),
        split_position(x)
    )
}

complete_pos <- function(x) {
    .subset(c(t = "top", l = "left", b = "bottom", r = "right"), x)
}

opposite_pos <- function(pos) {
    switch(pos,
        top = "bottom",
        bottom = "top",
        left = "right",
        right = "left"
    )
}

#' @importFrom ggplot2 zeroGrob
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom grid unit
make_patch_table <- function() {
    widths <- unit(rep(0L, TABLE_COLS), "mm")
    widths[LEFT_BORDER + 1L] <- unit(1L, "null")
    heights <- unit(rep(0L, TABLE_ROWS), "mm")
    heights[TOP_BORDER + 1L] <- unit(1L, "null")
    ans <- gtable(widths, heights)
    gtable_add_grob(ans,
        list(zeroGrob()), TOP_BORDER + 1L, LEFT_BORDER + 1L,
        z = -Inf, name = "panel-area"
    )
}

#' Generate a plot grob.
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @return A [`grob()`][grid::grob] object.
#' @examples
#' ggalignGrob(ggplot())
#' @export
ggalignGrob <- function(x) {
    ggalign_gtable(ggalign_build(x))
}

# Now, we only define `ggalign_gtable` method for `alignpatches` and `ggplot`
# `ggalign_build` must return these objects
ggalign_build <- function(x) UseMethod("ggalign_build")

ggalign_gtable <- function(x) UseMethod("ggalign_gtable")

#' @export
ggalign_gtable.gtable <- function(x) x

#' Prepare plots to be aligned with `align_plots`
#'
#' @param x A plot object to be prepared for alignment.
#' @details
#' `ggalign` has implement `alignpatch` method for following objects:
#'   - [`ggplot`][ggplot2::ggplot]
#'   - [`alignpatches`][align_plots]
#'   - [`wrapped_plot`][ggwrap]
#'   - [`patch`][patchwork::patchGrob]
#'   - [`wrapped_patch`][patchwork::wrap_elements]
#'   - [`spacer`][patchwork::plot_spacer]
#'
#' @return A `Patch` object.
#' @examples
#' alignpatch(ggplot())
#' @seealso [`align_plots()`]
#' @export
#' @keywords internal
alignpatch <- function(x) UseMethod("alignpatch")

#' @export
alignpatch.default <- function(x) {
    cli_abort("Cannot align {.obj_type_friendly {x}}")
}

#' @export
alignpatch.NULL <- function(x) NULL

patch_no_method <- function(plot, method) {
    cli_abort("no {.fn {method}} method for {.obj_type_friendly {plot}}")
}

#' @importFrom ggplot2 ggproto
#' @importFrom grid unit.c
Patch <- ggproto(
    "Patch", NULL,
    # following fields will be added by `alignpatch()`
    plot = NULL,
    # following fields will be added in `alignpatches$patch_gtable()`
    # borders = NULL, gt = NULL,

    #' @param guides `guides` argument from the parent alignpatches
    #' @return Which side of guide legends should be collected by the parent
    #' `alignpatches` object?
    #' @noRd
    set_guides = function(self, guides) {
        patch_no_method(self$plot, "set_guides")
    },
    patch_gtable = function(self, theme, guides, tagger = NULL, 
                            plot = self$plot) {
        patch_no_method(self$plot, "patch_gtable")
    },
    collect_guides = function(self, guides, gt = self$gt) {
        if (is.null(guides)) return(list()) # styler: off
        layout <- .subset2(gt, "layout")
        grobs <- .subset2(gt, "grobs")
        guides_ind <- grep("guide-box", .subset2(layout, "name"))
        guides_loc <- vec_slice(layout, guides_ind)
        collected_guides <- vector("list", length(guides))
        names(collected_guides) <- guides
        panel_loc <- find_panel(gt)
        remove_grobs <- NULL
        for (guide_pos in guides) {
            guide_ind <- switch(guide_pos,
                top = .subset2(guides_loc, "b") < .subset2(panel_loc, "t"),
                left = .subset2(guides_loc, "r") < .subset2(panel_loc, "l"),
                bottom = .subset2(guides_loc, "t") > .subset2(panel_loc, "b"),
                right = .subset2(guides_loc, "l") > .subset2(panel_loc, "r"),
                inside = .subset2(guides_loc, "t") >= .subset2(panel_loc, "t") &
                    .subset2(guides_loc, "b") <= .subset2(panel_loc, "b") &
                    .subset2(guides_loc, "l") >= .subset2(panel_loc, "l") &
                    .subset2(guides_loc, "r") <= .subset2(panel_loc, "r")
            )
            if (!any(guide_ind)) next
            guide_loc <- vec_slice(guides_loc, guide_ind)
            guide_ind <- .subset(guides_ind, guide_ind)
            remove_grobs <- c(guide_ind, remove_grobs)
            collected_guides[[guide_pos]] <- .subset2(grobs, guide_ind)

            # remove the guide spaces from the original gtable
            # for inside guide, no need to remove the spaces
            if (guide_pos == "inside") next

            space_pos <- switch(guide_pos,
                top = ,
                left = 1L,
                bottom = ,
                right = -1L
            )
            if (guide_pos %in% c("right", "left")) {
                gt$widths[c(guide_loc$l, guide_loc$l + space_pos)] <- unit(
                    c(0L, 0L), "mm"
                )
            } else if (guide_pos %in% c("bottom", "top")) {
                gt$heights[c(guide_loc$t, guide_loc$t + space_pos)] <- unit(
                    c(0L, 0L), "mm"
                )
            }
        }
        if (length(remove_grobs)) {
            gt <- subset_gt(gt, -remove_grobs, trim = FALSE)
        }
        self$gt <- gt
        collected_guides
    },
    respect = function(self, gt = self$gt) isTRUE(.subset2(gt, "respect")),
    align_panel_sizes = function(self, panel_width, panel_height,
                                 gt = self$gt) {
        list(width = panel_width, height = panel_height, respect = FALSE)
    },
    get_sizes = function(self, free = NULL, gt = self$gt) {
        ans <- .subset2(gt, "heights")
        if (any(free == "t")) {
            top <- unit(rep_len(0, TOP_BORDER), "mm")
        } else {
            top <- ans[seq_len(TOP_BORDER)]
        }
        if (any(free == "b")) {
            bottom <- unit(rep_len(0, BOTTOM_BORDER), "mm")
        } else {
            bottom <- ans[seq(length(ans) - BOTTOM_BORDER + 1L, length(ans))]
        }
        ans <- .subset2(gt, "widths")
        if (any(free == "l")) {
            left <- unit(rep_len(0, LEFT_BORDER), "mm")
        } else {
            left <- ans[seq_len(LEFT_BORDER)]
        }
        if (any(free == "r")) {
            right <- unit(rep_len(0, RIGHT_BORDER), "mm")
        } else {
            right <- ans[seq(length(ans) - RIGHT_BORDER + 1L, length(ans))]
        }
        list(
            widths = unit.c(left, unit(0, "mm"), right),
            heights = unit.c(top, unit(0, "mm"), bottom)
        )
    },
    align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                            gt = self$gt) {
        if (!is.null(t)) gt$heights[seq_along(t)] <- t
        if (!is.null(l)) gt$widths[seq_along(l)] <- l
        if (!is.null(b)) {
            n_row <- nrow(gt)
            gt$heights[seq(n_row - length(b) + 1L, n_row)] <- b
        }
        if (!is.null(r)) {
            n_col <- ncol(gt)
            gt$widths[seq(n_col - length(r) + 1L, n_col)] <- r
        }
        gt
    },
    split_gt = function(self, gt = self$gt) {
        isbg <- .subset2(.subset2(gt, "layout"), "name") == "background"
        if (any(isbg)) {
            bg <- .subset(.subset2(gt, "grobs"), isbg) # a list of background
            plot <- subset_gt(gt, !isbg, trim = FALSE)
        } else {
            bg <- NULL
            plot <- gt
        }
        list(bg = bg, plot = plot)
    },
    add_plot = function(self, gt, plot, t, l, b, r, name, z = 2L) {
        gtable_add_grob(
            gt,
            grobs = plot,
            t = t, l = l, b = b, r = r,
            name = name, z = z
        )
    },
    add_background = function(self, gt, bg, t, l, b, r, name, z = 1L) {
        gtable_add_grob(
            gt,
            grobs = bg,
            t = t, l = l, b = b, r = r,
            name = name, z = z
        )
    },
    free_border = function(self, borders, gt = self$gt) {
        patch_no_method(self$plot, "free_border")
    },
    align_free_border = function(self, borders,
                                 t = NULL, l = NULL, b = NULL, r = NULL,
                                 gt = self$gt) {
        patch_no_method(self$plot, "align_free_border")
    },
    free_lab = function(self, labs, gt = self$gt) {
        patch_no_method(self$plot, "free_lab")
    }
)
