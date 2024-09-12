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

TOP_BORDER <- 9L + 1L
LEFT_BORDER <- 7L + 1L
BOTTOM_BORDER <- 8L + 1L
RIGHT_BORDER <- 7L + 1L

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
split_position <- function(x) {
    unique(.subset2(strsplit(x, "", fixed = TRUE), 1L))
}
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
#' @return A [grob()][grid::grob] object.
#' @examples
#' ggalignGrob(ggplot())
#' @export
ggalignGrob <- function(x) {
    ggalign_gtable(ggalign_build(x))
}

ggalign_build <- function(x) UseMethod("ggalign_build")

ggalign_gtable <- function(x) UseMethod("ggalign_gtable")

#' Prepare plots to be aligned with `align_plots`
#'
#' @param x A plot object to be prepared for alignment.
#' `ggalign` has implement `alignpatch` method for following objects:
#'   - [ggplot][ggplot2::ggplot]
#'   - [alignpatches][align_plots]
#'   - [wrapped_plot][wrap]
#'   - [patch][patchwork::patchGrob]
#'   - [wrapped_patch][patchwork::wrap_elements]
#'
#' @return An object `Patch` object.
#' @examples
#' alignpatch(ggplot())
#' @seealso [align_plots]
#' @export
#' @keywords internal
alignpatch <- function(x) UseMethod("alignpatch")

#' @export
alignpatch.default <- function(x) {
    cli::cli_abort("Cannot align {.obj_type_friendly {x}}")
}

#' @export
alignpatch.NULL <- function(x) NULL

#' @importFrom ggplot2 ggproto
Patch <- ggproto("Patch", NULL,
    plot = NULL, gt = NULL,
    patch_gtable = function(self, guides, plot = self$plot) {
        cli::cli_abort(
            "Cannot convert {.obj_type_friendly {plot}} into a {.cls grob}"
        )
    },
    collect_guides = function(self, guides, gt = self$gt) list(),
    align_panel_sizes = function(self, guides, panel_width, panel_height,
                                 gt = self$gt) {
        list(width = panel_width, height = panel_height, respect = FALSE)
    },
    align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                            gt = self$gt) {
        if (!is.null(t)) gt$heights[seq_along(t)] <- t
        if (!is.null(l)) gt$widths[seq_along(l)] <- l
        if (!is.null(b)) {
            gt$heights[seq(nrow(gt) - length(b) + 1L, nrow(gt))] <- b
        }
        if (!is.null(r)) {
            gt$widths[seq(ncol(gt) - length(r) + 1L, ncol(gt))] <- r
        }
        gt
    },
    respect = function(self, gt = self$gt) isTRUE(.subset2(gt, "respect"))
)
