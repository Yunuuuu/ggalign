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
# 8: axis-t/strip-t strip.placement = "inside"
# 9: strip-t/axis-t strip.placement = "outside"
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
ggalignGrob.default <- function(x) patch_gtable(ggalign_build(x))

#' Building alignpatches object
#'
#' @description
#' Prepare plots to be aligned with `plot_grid`
#'
#' - `ggalign_build`: Prepare a plot object to be aligned, the output must
#' implement proper `patch_gtable` method.
#'
#' Extend object to be aligned with `plot_grid`
#'
#' - `patch_table`: Convert the plot into a [gtable][gtable::gtable].
#' - `patch_align`: Build a standard [gtable][gtable::gtable] object.
#'
#' @details
#' `ggalign` has implement `patch_gtable` method for following objects:
#'   - [ggplot][ggplot2::ggplot]
#'   - [alignpatches][plot_grid]
#'   - [patch][patchwork::patchGrob]
#' @param x A plot object to be prepared for alignment.
#' @return
#' - `ggalign_build`: An object that implements `patch_gtable` method.
#' @export
#' @order 1
ggalign_build <- function(x) UseMethod("ggalign_build")

#' @export
ggalign_build.default <- function(x) {
    cli::cli_abort("Cannot deal with {.obj_type_friendly {x}}",
        class = "missing_ggalign_build_class"
    )
}

#' @export
ggalign_build.ggplot <- function(x) x

#' @export
ggalign_build.alignpatches <- function(x) x

#' @export
ggalign_build.wrapped_plot <- function(x) x

#' @export
ggalign_build.grob <- function(x) wrap(x)

#' @export
ggalign_build.formula <- function(x) wrap(x)

#########################################
#' @param patch A patch to be aligned.
#' @param guides Input guides argument to [plot_grid()]
#' @return
#' - `patch_gtable`: A [gtable][gtable::gtable] object.
#' @export
#' @rdname ggalign_build
#' @order 2
patch_gtable <- function(patch, guides) UseMethod("patch_gtable")

# we always build a standard gtable layout for the `patch`
#' @param gt A [gtable][gtable::gtable] object from `patch_gtable`.
#' @param panel_width,panel_height Size of the panel, if the size is `NA`, we
#' should guess the size from the aspect ratio of the `gt`.
#' @return
#' - `patch_align`: A list with following elements:
#'    - `gt`: the standard [gtable][gtable::gtable] object
#'    - `width`/`height`: the panel width and height.
#'    - `respect`: A boolean value indicates whether to fix this panel area.
#' @export
#' @rdname ggalign_build
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
