#' Standardized gtable Representation
#'
#' @description
#' The standardized gtable representation ensures that all plots share a
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
#' @name standardized_gtable
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

#' Initialize an S7 Object
#'
#' Helper generic to initialize an object before use. This may include
#' setting properties or other elements to their default values, particularly
#' if the object's internal defaults differ from the defaults required for
#' normal usage.
#'
#' @param input An S7 object to initialize.
#'
#' @return The initialized object, ready for use.
#'
#' @noRd
init_object <- S7::new_generic("init_object", "input")

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
#' @description
#' `r code_quote(sprintf("%s::Patch", pkg_nm()), quote = FALSE)` represents the
#' layout manager for a single subplot within a composite plot. The `Patch`
#' object provides the interface for aligning the subplot, managing panel sizes,
#' and handling guide legends.
#'
#' @param x Any objects has a Patch representation
#' @return A `r code_quote(sprintf("%s::Patch", pkg_nm()), quote = FALSE)`
#' object.
#' @examples
#' patch(ggplot())
#' @seealso [`alignpatches()`]/[`align_plots()`]
#' @export
#' @keywords internal
patch <- function(x) UseMethod("patch")

#' @export
patch.default <- function(x) {
    cli_abort(paste(
        "Each plot to be aligned must implement an {.fn patch}",
        "method. Object of {.obj_type_friendly {x}} does not."
    ))
}

#' @export
patch.NULL <- function(x) NULL

#' Patch object
#'
#'
#' @usage NULL
#' @format NULL
#'
#' @details
#' In `alignpatches()`, each subplot is regarded as a `patch`, and **a
#' corresponding `Patch` object is required** for proper alignment and layout
#' operations. `Patch` is a [`ggproto()`][ggplot2::ggproto] object that provides
#' the core methods for arranging and aligning subplots.
#'
#' @importFrom ggplot2 ggproto find_panel
#' @importFrom grid unit unit.c
#' @importFrom gtable is.gtable
#' @export
#' @rdname patch
Patch <- ggproto(
    "ggalign::Patch", NULL,

    # @field plot
    #
    # The input subplot, stored in the `plot` field by convention.
    plot = NULL,

    #' @field guides
    #' **Description**
    #'
    #' (Optional method) Determines which sides of the guide legends should be
    #' collected by the parent [`alignpatches()`] object.
    #'
    #' This per-plot method allows each subplot to modify the `guides` passed to
    #' the `self$decompose_guides()` method, ensuring that plots along the
    #' border collect their guide legends correctly. Such fine-grained control
    #' cannot be achieved when relying on only a single
    #' `self$decompose_guides()` method.
    #'
    #' **Arguments**
    #' - `guides`: The `guides` argument passed from the parent
    #'   [`alignpatches()`] object, specifying how legends should be combined or
    #'   positioned. Possible values include `r oxford_and(.TLBR)`.
    #'
    #' **Value**
    #' A modified `guides` object indicating which sides of the guide legends
    #' should be collected by the parent [`alignpatches()`] object.
    guides = function(self, guides) guides,

    #' @field gtable
    #'
    #' **Description**
    #'
    #' (Required method) Constructs a
    #' [`standardized gtable`][standardized_gtable] object.
    #'
    #' **Arguments**
    #' - `theme`: The global [`theme`][ggplot2::theme] of the parent
    #'   [`alignpatches()`] object.
    #' - `guides`: Specifies which sides of guide legends should be collected by
    #'   the parent [`alignpatches()`] object. In most cases, this is the value
    #'   returned by the subplot's `self$guides()` method. For plots along the
    #'   border, any guide legends on that side will always be collected if any
    #'   legends on that side of any subplot are being collected.
    #' - `tagger`: Either `NULL` (no tagging) or a `LayoutTagger` object that
    #'   provides a `$tag_table` method (accepting the `gtable` and `theme`)
    #'   used to add tag.
    #'
    #' **Value**
    #' A standardized [`gtable`][gtable::gtable] object representing the plot
    #' layout.
    gtable = function(self, theme = NULL, guides = NULL, tagger = NULL) {
        cli_abort("{.fn gtable} method is not defined")
    },

    #' @field decompose_guides
    #' **Description**
    #'
    #' (Optional method) Collects guide legends and optionally removes the space
    #' they occupy.
    #'
    #' This method extracts guide legends based on the sides specified in the
    #' `guides` argument. After collecting the guides, the corresponding space
    #' in the the `gt` is removed to free up space, except for guides
    #' placed `inside` the panel.
    #'
    #' **Arguments**
    #' - `gt`: A [`gtable`][gtable::gtable] object, usually returned by
    #'   `self$gtable()`.
    #' - `guides`: Specifies which sides of guide legends should be collected by
    #'   the parent [`alignpatches()`] object. In most cases, this is the value
    #'   returned by the subplot's `self$guides()` method. For plots along the
    #'   border, any guide legends on that side will always be collected if any
    #'   legends on that side of any subplot are being collected.
    #'
    #' **Value**
    #' A list with:
    #' - `gt`: The updated gtable with guide legends removed (if applicable).
    #' - `guides`: A named list of collected guide grobs corresponding to the
    #'   sides specified in `guides` (or `NULL` if absent).
    decompose_guides = function(self, gt, guides) {
        # By default we only consider gtable has guide legend box
        if (is.null(guides) || !is.gtable(gt)) {
            return(list(gt = gt, guides = list()))
        }

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
        list(gt = gt, guides = collected_guides)
    },

    #' @field align_panel
    #' **Description**
    #'
    #' (Optional method) In most cases, panel sizes do not need to be manually
    #' adjusted when aligning panels, as long as their border sizes are
    #' consistent. However, for gtables with a fixed aspect ratio, this method
    #' adjusts the panel width and height based on user input and the dimensions
    #' of the underlying gtable (`gt`) to ensure proper alignment.
    #'
    #' When the internal *numeric value* of either `panel_width` or
    #' `panel_height` is `NA` (i.e., `is.na(as.numeric(...))`), that dimension
    #' is inferred from the gtable while maintaining the aspect ratio for
    #' single-panel layouts when `respect = TRUE`.
    #'
    #' **Arguments**
    #' - `gt`: A [`gtable`][gtable::gtable] object, usually returned by
    #'   `self$decompose_guides()`.
    #' - `panel_width`/`panel_height`: Unit objects specifying the desired panel
    #'   size. If the internal numeric value of either is `NA`, the size is
    #'   computed from the gtable (`gt`).
    #'
    #' **Value**
    #' A list with components:
    #' - `width`: Final panel width as a unit object
    #' - `height`: Final panel height as a unit object
    #' - `respect`: If `TRUE`, the aspect ratio was enforced
    #'
    #' @importFrom ggplot2 find_panel
    #' @importFrom gtable is.gtable
    align_panel = function(self, gt, panel_width, panel_height) {
        # By default we only consider gtable has panel area
        if (!is.gtable(gt)) {
            return(list(width = panel_width, height = panel_height))
        }
        panel_pos <- find_panel(gt)
        rows <- c(.subset2(panel_pos, "t"), .subset2(panel_pos, "b"))
        cols <- c(.subset2(panel_pos, "l"), .subset2(panel_pos, "r"))
        # Only apply aspect-ratio respect when there is a single facet panel
        if (rows[1L] == rows[2L] && cols[1L] == cols[2L]) {
            respect <- is_respect(gt)
            # Continue only if 'respect' is enabled
            if (respect) {
                # Determine which dimension(s) can be inferred
                can_set_width <- is.na(as.numeric(panel_width))
                can_set_height <- is.na(as.numeric(panel_height))

                # Extract intrinsic panel dimensions from the gtable
                w <- .subset2(gt, "widths")[cols[1L]]
                h <- .subset2(gt, "heights")[rows[1L]]

                # Infer dimensions while maintaining aspect ratio
                if (can_set_width && can_set_height) {
                    panel_width <- w
                    panel_height <- h
                } else if (can_set_width) {
                    panel_width <- as.numeric(w) / as.numeric(h) * panel_height
                } else if (can_set_height) {
                    panel_height <- as.numeric(h) / as.numeric(w) * panel_width
                } else {
                    respect <- FALSE
                }
            }
        } else {
            respect <- FALSE
        }
        list(width = panel_width, height = panel_height, respect = respect)
    },

    #' @field border_sizes
    #' **Description**
    #'
    #' (Optional method) retrieve the border sizes of a gtable.
    #'
    #' **Arguments**
    #' - `gt`: A [`gtable`][gtable::gtable] object, usually returned by
    #'   `self$decompose_guides()`.
    #' - `free`: Optional. Borders to exclude when calculating sizes. Possible
    #'   values include `r oxford_and(.TLBR)`.
    #'
    #' **Value**
    #' A list with components:
    #' - `top`: `unit` values for the top borders.
    #' - `left`: `unit` values for the left borders.
    #' - `bottom`: `unit` values for the bottom borders.
    #' - `right`: `unit` values for the right borders.
    #'
    #' @importFrom gtable is.gtable
    border_sizes = function(self, gt = NULL, free = NULL) {
        if (is.gtable(gt)) {
            ans <- .subset2(gt, "heights")
            if (any(free == "top")) {
                top <- unit(rep_len(0, TOP_BORDER), "mm")
            } else {
                top <- ans[seq_len(TOP_BORDER)]
            }
            if (any(free == "bottom")) {
                bottom <- unit(rep_len(0, BOTTOM_BORDER), "mm")
            } else {
                bottom <- ans[seq(length(ans) - BOTTOM_BORDER + 1L, length(ans))]
            }
            ans <- .subset2(gt, "widths")
            if (any(free == "left")) {
                left <- unit(rep_len(0, LEFT_BORDER), "mm")
            } else {
                left <- ans[seq_len(LEFT_BORDER)]
            }
            if (any(free == "right")) {
                right <- unit(rep_len(0, RIGHT_BORDER), "mm")
            } else {
                right <- ans[seq(length(ans) - RIGHT_BORDER + 1L, length(ans))]
            }
            list(top = top, left = left, bottom = bottom, right = right)
        } else {
            NULL
        }
    },

    #' @field align_border
    #' **Description**
    #'
    #' (Optional method) This method modifies the top, left, bottom, and right
    #' border sizes of the underlying gtable (`gt`) by replacing corresponding
    #' entries in its `heights` and `widths` vectors..
    #'
    #' **Arguments**
    #' - `gt`: A [`gtable`][gtable::gtable] object, usually returned by
    #'   `self$decompose_guides()`.
    #' - `t`, `l`, `b`, `r`: Optional numeric vectors specifying new sizes for
    #'   the top, left, bottom, and right borders, respectively. Each vector
    #'   replaces the corresponding entries in `gt$heights` or `gt$widths`.
    #'
    #' **Value**
    #' A modified [`gtable`][gtable::gtable] object.
    #' @importFrom gtable is.gtable
    align_border = function(self, gt, t = NULL, l = NULL, b = NULL, r = NULL) {
        if (!is.gtable(gt)) return(gt) # styler: off
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

    #' @field place
    #'
    #' **Description**
    #' (Optional method) Inserts the patch's gtable (including optional
    #' background) into the target canvas gtable.
    #'
    #' This method places the patch's gtable into a specified location of
    #' another gtable, preserving the background and plot layers separately if a
    #' background exists. The `t`, `l`, `b`, `r` arguments specify the position
    #' in the target gtable, and `bg_z` / `plot_z` define the stacking order
    #' (z-order) for background and plot.
    #'
    #' **Arguments**
    #' - `gtable`: the target canvas gtable into which the patch will be
    #'   inserted.
    #' - `gt`: A [`gtable`][gtable::gtable] object, usually returned by
    #'   `self$align_border()`.
    #' - `t`, `l`, `b`, `r`: Integer positions (top, left, bottom, right)
    #'   specifying where to insert the patch in the target gtable.
    #' - `i`: Index of the current patch, used to generate unique grob names.
    #' - `bg_z`: Z-order for the background grob (default `1L`).
    #' - `plot_z`: Z-order for the plot grob (default `2L`).
    #'
    #' **Details**
    #' - If the patch includes a grob named `"background"`, it is separated from
    #'   the main plot and inserted independently from the plot grob.
    #' - If no background is present, the entire gtable is inserted as the plot
    #'   grob.
    #'
    #' **Value**
    #' The modified target canvas gtable with the patch's gtable added.
    place = function(self, gtable, gt, t, l, b, r, i, bg_z, plot_z) {
        if (is.gtable(gt)) {
            components <- self$decompose_bg(gt)
            if (!is.null(.subset2(components, "bg"))) {
                gtable <- self$place_bg(
                    gtable, .subset2(components, "bg"),
                    t, l, b, r, i, bg_z
                )
            }
            gt <- .subset2(components, "gt")
        }
        self$place_gt(gtable, gt, t, l, b, r, i, plot_z)
    },

    #' @field decompose_bg
    #'
    #' **Description**
    #' Separates the background grob (if present) from the main gtable.
    #'
    #' **Value**
    #' A list with:
    #' - `bg`: The background grob (or `NULL` if absent)
    #' - `gt`: The gtable with background removed
    decompose_bg = function(self, gt) {
        layout <- .subset2(gt, "layout")
        background <- layout$name == "background"
        if (!any(background)) {
            return(list(bg = NULL, gt = gt))
        }
        bg <- .subset(.subset2(gt, "grobs"), background)
        gt <- subset_gt(gt, !background, trim = FALSE)
        list(bg = bg, gt = gt)
    },

    #' @field place_bg
    #'
    #' **Description**
    #' Adds the background grob into the target gtable.
    place_bg = function(self, gtable, bg, t, l, b, r, i, z = 1L) {
        gtable_add_grob(
            gtable,
            grobs = bg,
            t = t, l = l, b = b, r = r,
            name = paste("plot", i, "background", sep = "-"),
            z = z
        )
    },

    #' @field place_gt
    #'
    #' **Description**
    #' Adds the main plot gtable into the target gtable.
    place_gt = function(self, gtable, gt, t, l, b, r, i, z = 2L) {
        gtable_add_grob(
            gtable,
            grobs = gt,
            t = t, l = l, b = b, r = r,
            name = paste("plot", i, sep = "-"),
            z = z
        )
    },

    #' @field is_alignpatches
    #'
    #' **Description**
    #'
    #' Checks whether the object inherits from the [alignpatches()] `Patch`
    #' representation.
    #'
    #' If `TRUE`, the fields `self$patches`, `self$gt_list`, and
    #' `self$borders_list` are expected to exist in the `$align_border()` and
    #' `$place()` methods. See the `patch.ggalign_free_lab` function in the
    #' `alignpatch-free-lab.R` script for an example of usage.
    #'
    #' **Value**
    #' Logical value (`TRUE` or `FALSE`) indicating whether `self` is a
    #' `PatchAlignpatches` object.
    is_alignpatches = function(self) inherits(self, "PatchAlignpatches")
)
