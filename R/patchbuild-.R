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
        z = -Inf, name = "panel_patch"
    )
}

# convert a plot into a gtable
patch_gtable <- function(patch) UseMethod("patch_gtable")

# we always build a standard gtable layout for the `patch`
# and we extract the size for axis, lab, strip
patch_build <- function(gt) UseMethod("patch_build")

#' @export
patch_gtable.alignpatch <- function(patch) patch

#' @export
patch_build.alignpatch <- function(gt) gt

# For grob ---------------------------
#' @importFrom gtable gtable_add_grob
#' @export
patch_gtable.grob <- function(patch) {
    ans <- make_patch()
    ans <- gtable_add_grob(ans,
        list(patch), PANEL_ROW, PANEL_COL,
        z = -Inf, name = "panel_grob"
    )
    add_class(ans, "alignpatch")
}

# `patch` from `patchwork`: patchwork::plot_spacer
#' @export
patch_gtable.patch <- function(patch) {
    patchwork::patchGrob(patch, guides = "auto")
}

#' @export
patch_build.patchgrob <- function(gt) gt
