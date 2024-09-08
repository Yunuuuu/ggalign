# here is copied from patchwork
# we modified the `patchwork` package for following reasons:
# 1. we don't want to align some axes for some columns or rows, this is the main
#    reason and this is not approriate to push into patchwork.
# 2. collect guides for each side (should be merged into patchwork).
# 3. allow collapse axis title and labels (should be merged into patchwork).
#    see https://github.com/thomasp85/patchwork/pull/373
# 4. Added titles around the plot top, left, bottom, and right
TABLE_ROWS <- 18L + 2L
TABLE_COLS <- 15L + 2L
PANEL_ROW <- 10L + 1L
PANEL_COL <- 8L + 1L
PLOT_TOP <- 7L + 1L
PLOT_BOTTOM <- 13L + 1L
PLOT_LEFT <- 5L + 1L
PLOT_RIGHT <- 11L + 1L
TITLE_ROW <- 3L
SUBTITLE_ROW <- 4L
CAPTION_ROW <- 16L + 2L

GUIDE_TOP <- 5L
GUIDE_LEFT <- 3L
GUIDE_BOTTOM <- 15L + 2L
GUIDE_RIGHT <- 13L + 2L

# top-bottom
# 1: margin
# 2: tag
# 3: title
# 4: subtitle
# 5: guide-box-top
# 6: legend.box.spacing
# feature: insert patch title
# 7: xlab-t
# strip.placement = "inside"
# 8: axis-t
# 9: strip-t
# strip.placement = "outside"
# 8.strip-t 
# 9. axis-t 
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

setdiff_position <- function(x, y) gsub(sprintf("[%s]", y), "", x)
union_position <- function(x, y) paste0(x, gsub(sprintf("[%s]", x), "", y))
split_position <- function(x) .subset2(strsplit(x, "", fixed = TRUE), 1L)
setup_position <- function(x) {
    .subset(
        c(t = "top", l = "left", b = "bottom", r = "right"),
        split_position(x)
    )
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
make_patch <- function() {
    widths <- unit(rep(0L, TABLE_COLS), "mm")
    widths[PANEL_COL] <- unit(1L, "null")
    heights <- unit(rep(0L, TABLE_ROWS), "mm")
    heights[PANEL_ROW] <- unit(1L, "null")
    ans <- gtable(widths, heights)
    gtable_add_grob(ans,
        list(zeroGrob()), PANEL_ROW, PANEL_COL,
        z = -Inf, name = "panel-area"
    )
}

# copied from patchwork
#' @param borders Which borders should be aligned?
#' @importFrom ggplot2 find_panel zeroGrob
#' @importFrom gtable gtable gtable_add_grob
#' @noRd
make_full_patch <- function(gt, ..., borders = .TLBR) {
    panel_pos <- find_panel(gt)
    heights <- .subset2(gt, "heights")
    widths <- .subset2(gt, "widths")
    if (any(borders == "top")) {
        t <- heights[seq_len(.subset2(panel_pos, "t") - 1L)]
    } else {
        t <- unit(rep(0L, PANEL_ROW - 1L), "mm")
    }
    if (any(borders == "left")) {
        l <- widths[seq_len(.subset2(panel_pos, "l") - 1L)]
    } else {
        l <- unit(rep(0L, PANEL_COL - 1L), "mm")
    }
    if (any(borders == "bottom")) {
        b <- heights[seq(.subset2(panel_pos, "b") + 1L, nrow(gt))]
    } else {
        b <- unit(rep(0L, TABLE_ROWS - PANEL_ROW), "mm")
    }
    if (any(borders == "right")) {
        r <- widths[seq(.subset2(panel_pos, "r") + 1L, ncol(gt))]
    } else {
        r <- unit(rep(0L, TABLE_COLS - PANEL_COL), "mm")
    }
    widths <- unit.c(l, unit(1L, "null"), r)
    heights <- unit.c(t, unit(1L, "null"), b)
    ans <- gtable(widths = widths, heights = heights)
    ans <- gtable_add_grob(
        ans, zeroGrob(),
        PANEL_ROW, PANEL_COL,
        name = "panel-area"
    )
    ans <- gtable_add_grob(ans, list(gt),
        t = 1L, l = 1L, b = nrow(ans), r = ncol(ans), ...
    )
    add_class(ans, "full_patch")
}

#' Generate a plot grob.
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @return A [grob()][grid::grob] object.
#' @export
ggalignGrob <- function(x) UseMethod("ggalignGrob")

#' @export
#' @rdname ggalignGrob
ggalignGrob.default <- function(x) patch_gtable(alignpatch(x))

#' Building `alignpatches` object
#'
#' @description
#' Prepare plots to be aligned with `align_plots`
#'
#' - `alignpatch`: Prepare a plot object to be aligned, the output must
#' implement proper `patch_gtable` method.
#'
#' Extend object to be aligned with `align_plots`
#'
#' - `patch_table`: Convert the plot into a [gtable][gtable::gtable].
#' - `patch_align`: Build a standard [gtable][gtable::gtable] object and set the
#'   panel width and height.
#'
#' @details
#' `ggalign` has implement `patch_gtable` method for following objects:
#'   - [ggplot][ggplot2::ggplot]
#'   - [alignpatches][align_plots]
#'   - [wrapped_plot][alignwrap]
#'   - [patch][patchwork::patchGrob]
#'   - [wrapped_patch][patchwork::wrap_elements]
#' @param x A plot object to be prepared for alignment.
#' @return
#' - `alignpatch`: An object that implements `patch_gtable` method.
#' @export
#' @examples
#' alignpatch(ggplot())
#' @order 1
#' @keywords internal
alignpatch <- function(x) UseMethod("alignpatch")

#' @export
alignpatch.default <- function(x) {
    cli::cli_abort("Cannot align {.obj_type_friendly {x}}")
}

#' @export
alignpatch.ggplot <- function(x) x

#' @export
alignpatch.alignpatches <- function(x) x

#' @export
alignpatch.wrapped_plot <- function(x) x

#########################################
#' @param patch A patch to be aligned.
#' @param guides Input guides argument to [align_plots()]
#' @return
#' - `patch_gtable`: A [gtable][gtable::gtable] object.
#' @examples
#' patch_gtable(ggplot())
#' @export
#' @rdname alignpatch
#' @order 2
patch_gtable <- function(patch, guides) UseMethod("patch_gtable")

# we always build a standard gtable layout for the `patch`
#' @param gt A [gtable][gtable::gtable] object from `patch_gtable`.
#' @param panel_width,panel_height Size of the panel, if the size is `NA`, we
#' should guess the size from the aspect ratio of the `gt`.
#' @return
#' - `patch_align`: A list with following elements:
#'    - `gt`: A standard [gtable][gtable::gtable] object
#'    - `width`/`height`: the panel width and height.
#'    - `respect`: A boolean value indicates whether to fix this panel area.
#' @export
#' @rdname alignpatch
patch_align <- function(gt, guides, panel_width, panel_height) {
    UseMethod("patch_align")
}

#' @export
patch_align.default <- function(gt, guides, panel_width, panel_height) {
    list(gt = gt, width = panel_width, height = panel_height, respect = FALSE)
}

#' Extract the added classes when building alignpatches
#' @noRd
alignpatch_class <- function(x) {
    cls <- class(x)
    cls[seq_len(which(cls == "gtable") - 1L)]
}
