#' Normalized gtable Representation
#'
#' @description
#' The normalized gtable representation ensures that all plots share a
#' consistent row–column structure, facilitating flexible composition,
#' patch insertion, and alignment across multiple plots.
#'
#' This layout provides a fixed-size gtable grid for predictable positioning
#' of plot components such as titles, axes, legends, captions, and margins.
#' It is used internally for layout normalization and patch-based alignment.
#'
#' @usage NULL
#' @section Row structure (top -> bottom):
#' | Index | Component | Description |
#' |:------|:-----------|:-------------|
#' | 1 | `margin-top` | External top spacing |
#' | 2 | `tag-top` | Top tag (e.g., "A", "B") |
#' | 3 | `title` | Main title |
#' | 4 | `subtitle` | Subtitle |
#' | 5 | `guide-box-top` | Legend box at top |
#' | 6 | `legend.box.spacing` | Space between legend and main area |
#' | 7 | `patch-title-top`| Top patch title |
#' | 8 | `xlab-top` | Top x-axis label (rare) |
#' | 9 | `axis-top` | Top axis ticks and labels |
#' | 10 | `strip-top` | Top strip (facet label) |
#' | 11 | `panel` | Main plotting panel |
#' | 12 | `strip-bottom` | Bottom strip (facet label) |
#' | 13 | `axis-bottom` | Bottom axis ticks and labels |
#' | 14 | `xlab-bottom` | Bottom x-axis label |
#' | 15 | `patch-title-bottom`| Top patch title |
#' | 16 | `legend.box.spacing` | Space before bottom legend box |
#' | 17 | `guide-box-bottom` | Bottom legend box |
#' | 18 | `caption` | Caption or footnote text |
#' | 19 | `tag-bottom` | Bottom tag (optional) |
#' | 20 | `margin-bottom` | External bottom spacing |
#'
#' @section Column structure (left -> right):
#' | Index | Component | Description |
#' |:------|:-----------|:-------------|
#' | 1 | `margin-left` | External left spacing |
#' | 2 | `tag-left` | Optional side tag |
#' | 3 | `guide-box-left` | Left legend box |
#' | 4 | `legend.box.spacing` | Space between legend and panel |
#' | 5 | `patch-title-left`| Left patch title |
#' | 6 | `ylab-left` | Left y-axis label |
#' | 7 | `axis-left` | Left axis ticks and labels |
#' | 8 | `strip-left` | Left strip (facet label) |
#' | 9 | `panel` | Main panel area |
#' | 10 | `strip-right` | Right strip (facet label) |
#' | 11 | `axis-right` | Right axis ticks and labels |
#' | 12 | `ylab-right` | Right y-axis label |
#' | 13 | `patch-title-right`| Right patch title |
#' | 14 | `legend.box.spacing` | Space before right legend box |
#' | 15 | `guide-box-right` | Right legend box |
#' | 16 | `tag-right` | Optional tag on right side |
#' | 17 | `margin-right` | External right spacing |
#'
#' @name normalized_gtable
#' @keywords internal
NULL

TABLE_ROWS <- 18L + 2L
TABLE_COLS <- 15L + 2L

TOP_BORDER <- 9L + 1L
LEFT_BORDER <- 7L + 1L
BOTTOM_BORDER <- 8L + 1L
RIGHT_BORDER <- 7L + 1L

.TLBR <- c("top", "left", "bottom", "right")
.tlbr <- c("t", "l", "b", "r")

# position is a single string contains `.tlbr`
setdiff_position <- function(x, y) gsub(sprintf("[%s]", y), "", x)
union_position <- function(x, y) paste0(x, gsub(sprintf("[%s]", x), "", y))
split_position <- function(x) {
    vec_unique(.subset2(strsplit(x, "", fixed = TRUE), 1L))
}

setup_position <- function(x) setup_guides(x)

setup_guides <- function(x) {
    out <- .subset(
        c(t = "top", l = "left", b = "bottom", r = "right", i = "inside"),
        split_position(x)
    )
    names(out) <- NULL
    out
}

# pos is an atomic character
opposite_pos <- function(pos) {
    out <- .subset(
        c(top = "bottom", left = "right", bottom = "top", right = "left"),
        pos
    )
    names(out) <- NULL
    out
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

init_hook <- S7::new_generic("init_hook", "input")

#' Generate a plot grob.
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @return A [`grob()`][grid::grob] object.
#' @examples
#' ggalignGrob(ggplot())
#' @export
ggalignGrob <- function(x) ggalign_gtable(ggalign_build(x))

# Now, we only define `ggalign_gtable` method for `alignpatches` and `ggplot`
# `ggalign_build` must return these objects
ggalign_build <- function(x) UseMethod("ggalign_build")

ggalign_gtable <- function(x) UseMethod("ggalign_gtable")

#' @export
ggalign_gtable.gtable <- function(x) x

#' Get Patch representation
#'
#' @param x Any objects has a Patch representation
#' @details
#' `ggalign` has implement `alignpatch` method for following objects:
#'   - [`ggplot`][ggplot2::ggplot]
#'   - [`alignpatches`][align_plots]
#'   - [`wrapped_plot`][ggwrap]
#'   - [`patchwork::patchGrob`]
#'   - [`patchwork::wrap_elements`]
#'   - [`patchwork::plot_spacer`]
#'
#' @return A `r code_quote(sprintf("%s::Patch", pkg_nm()), quote = FALSE)`
#' object.
#' @examples
#' alignpatch(ggplot())
#' @seealso [`align_plots()`]
#' @export
#' @keywords internal
alignpatch <- function(x) UseMethod("alignpatch")

#' @export
alignpatch.default <- function(x) {
    cli_abort(paste(
        "Each plot to be aligned must implement an {.fn alignpatch}",
        "method. Object of {.obj_type_friendly {x}} does not."
    ))
}

#' @export
alignpatch.NULL <- function(x) NULL

#' @importFrom ggplot2 ggproto find_panel
#' @importFrom grid unit unit.c
#' @importFrom gtable is.gtable
Patch <- ggproto(
    "ggalign::Patch", NULL,

    # Fields added later in `alignpatches$gtable()`
    borders = NULL, # Border specifications for the patch
    gt = NULL, # The patch's gtable representation

    #' @param guides `guides` argument from the parent `alignpatches` object.
    #' @return Which side of guide legends should be collected by the parent
    #' `alignpatches` object?
    #' @noRd
    # we add a single method to let each plot determine which side of guide
    # legends will be collected before passing to `gtable()` or
    # `collect_guides()` instead of just one method `collect_guides()`. So that
    # the internal can modify the guides passed to `gtable()` or
    # `collect_guides()` and we can easily ensure that plots placed in a border
    # collect their guides by modifying the `guides` returned by $guides()
    # method.
    guides = function(self, guides) guides,
    #' @param guides Which side of guide legends should be collected by the
    #' parent `alignpatches` object?
    gtable = function(self, theme = NULL, guides = NULL, tagger = NULL) {
        cli_abort("{.fn gtable} method is not defined")
    },
    #' @param guides Which side of guide legends should be collected by the
    #' parent `alignpatches` object?
    collect_guides = function(self, guides) {
        if (is.null(guides)) return(list()) # styler: off
        gt <- self$gt

        # By default we only consider gtable has guide legend box
        if (!is.gtable(gt)) return(list()) # styler: off

        layout <- .subset2(gt, "layout")
        grobs <- .subset2(gt, "grobs")
        guides_ind <- grep("guide-box", .subset2(layout, "name"))

        # no guide legends to be collected
        if (length(guides_ind) == 0L) return(list()) # styler: off

        # collect guide legends from the `gt`
        guides_loc <- vec_slice(layout, guides_ind)
        collected_guides <- vector("list", length(guides))
        names(collected_guides) <- guides
        panel_loc <- find_panel(gt)
        removed <- NULL # guide legend grobs which will be removed
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
            removed <- c(guide_ind, removed)
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
        if (length(removed)) gt <- subset_gt(gt, -removed, trim = FALSE)
        self$gt <- gt
        collected_guides
    },
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
        cli_abort("{.fn free_border} method is not defined")
    },
    align_free_border = function(self, borders,
                                 t = NULL, l = NULL, b = NULL, r = NULL,
                                 gt = self$gt) {
        cli_abort("{.fn align_free_border} method is not defined")
    },
    free_lab = function(self, labs, gt = self$gt) {
        cli_abort("{.fn free_lab} method is not defined")
    }
)
