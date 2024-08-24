# here is copied from patchwork
# we modified the `patchwork` package for following reasons:
# 1. we don't want to align some axes for some columns or rows, this is the main
#    reason and this is not approriate to push into patchwork.
# 2. collect guides for each side (should be merged into patchwork).
# 3. allow collapse axis title and labels (should be merged into patchwork).
#    see https://github.com/thomasp85/patchwork/pull/373
TABLE_ROWS <- 18L
TABLE_COLS <- 15L
PANEL_ROW <- 10L
PANEL_COL <- 8L
PLOT_TOP <- 7L
PLOT_BOTTOM <- 13L
PLOT_LEFT <- 5L
PLOT_RIGHT <- 11L
TITLE_ROW <- 3L
SUBTITLE_ROW <- 4L
CAPTION_ROW <- 16L

GUIDE_RIGHT <- 13L
GUIDE_LEFT <- 3L
GUIDE_TOP <- 5L
GUIDE_BOTTOM <- 15L

BORDERS <- c("t", "l", "b", "r")

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
make_full_patch <- function(gt, ..., borders = c("t", "l", "b", "r")) {
    panel_pos <- find_panel(gt)
    heights <- .subset2(gt, "heights")
    widths <- .subset2(gt, "widths")
    if (any(borders == "t")) {
        t <- heights[seq_len(.subset2(panel_pos, "t") - 1L)]
    } else {
        t <- unit(rep(0L, PANEL_ROW - 1L), "mm")
    }
    if (any(borders == "l")) {
        l <- widths[seq_len(.subset2(panel_pos, "l") - 1L)]
    } else {
        l <- unit(rep(0L, PANEL_COL - 1L), "mm")
    }
    if (any(borders == "b")) {
        b <- heights[seq(.subset2(panel_pos, "b") + 1L, nrow(gt))]
    } else {
        b <- unit(rep(0L, TABLE_ROWS - PANEL_ROW), "mm")
    }
    if (any(borders == "r")) {
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
    add_class(ans, "full_patch", "alignpatch")
}

# convert a plot into a gtable
#' @return A `gtable_*` object
#' @noRd
patch_gtable <- function(patch) UseMethod("patch_gtable")

# we always build a standard gtable layout for the `patch`
# and we extract the size for axis, lab, strip
#' @return A `align_*` object
#' @noRd
patch_align <- function(gt, guides) UseMethod("patch_align")

#' Extract the alignpatch class and it's Child classes
#' @noRd
alignpatch_class <- function(x) {
    cls <- class(x)
    cls[seq_len(which(cls == "alignpatch"))]
}

#' @export
patch_gtable.alignpatch <- function(patch) patch

#' @export
patch_align.alignpatch <- function(gt, guides) gt

# For grob ---------------------------
#' @importFrom gtable gtable_add_grob
#' @export
patch_gtable.grob <- function(patch) {
    ans <- make_patch()
    ans <- gtable_add_grob(ans,
        list(patch), PANEL_ROW, PANEL_COL,
        z = -Inf, name = "panel-grob"
    )
    add_class(ans, "align_grob", "alignpatch")
}

# `patch` from `patchwork`: patchwork::plot_spacer
#' @export
patch_gtable.patch <- function(patch) {
    add_class(patch, "gtable_patch")
}

#' @export
patch_align.gtable_patch <- function(gt, guides) {
    class(gt) <- setdiff(class(gt), "gtable_patch")
    guides <- if (length(guides)) "collect" else "keep"
    add_class(
        patchwork::patchGrob(gt, guides = guides),
        "align_patch", "alignpatch"
    )
}
