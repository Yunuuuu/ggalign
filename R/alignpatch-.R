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
#' @param x Any objects has a Patch representation
#' @details
#' `ggalign` has implement `alignpatch` method for following objects:
#'   - [`ggplot`][ggplot2::ggplot]
#'   - [`alignpatches`]
#'   - [`wrapped_plot`][ggwrap]
#'   - [`patchwork::patchGrob`]
#'   - [`patchwork::wrap_elements`]
#'   - [`patchwork::plot_spacer`]
#'
#' @return A `r code_quote(sprintf("%s::Patch", pkg_nm()), quote = FALSE)`
#' object.
#' @examples
#' alignpatch(ggplot())
#' @seealso
#' - [`alignpatches()`]/[`align_plots()`]
#' - [`Patch`]
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

#' Patch object
#'
#' @description
#' Represents a single patch within an [`alignpatches()`] layout. This object
#' defines operations for aligning plots, managing panel sizes, and handling
#' guide legends.
#'
#' @usage NULL
#' @format NULL
#'
#' @details
#' `Patch` is a [`ggproto()`][ggplot2::ggproto] object that provides the
#' core methods used by [`alignpatches()`] for arranging and aligning subplots.
#' Each patch can manage its own panel dimensions, guides, and gtable layout,
#' and it can be extended to support additional plot types for alignment within
#' [`alignpatches()`].
#'
#' @importFrom ggplot2 ggproto find_panel
#' @importFrom grid unit unit.c
#' @importFrom gtable is.gtable
#' @export
Patch <- ggproto(
    "ggalign::Patch", NULL,

    # Fields added later in `alignpatches$gtable()`

    #' @field gt
    #' The gtable representation of the plot
    gt = NULL,

    #' @field guides
    #'
    #' Border specifications of the plot
    borders = NULL,

    #' @field guides
    #' **Description**
    #'
    #' (Optional method) Determines which sides of the guide legends should be
    #' collected by the parent [`alignpatches()`] object.
    #'
    #' This per-plot method allows each subplot to modify the `guides` passed to
    #' the `self$$collect_guides()` method, ensuring that plots along the border
    #' collect their guide legends correctly. Such fine-grained control cannot
    #' be achieved when relying on only a single `self$$collect_guides()`
    #' method.
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
    #' - `guides`: Specifies which sides of guide legends should be collected by
    #'   the parent [`alignpatches()`] object. In most cases, this is the value
    #'   returned by the subplot's `self$guides()` method. For plots along the
    #'   border, any guide legends on that side will always be collected if any
    #'   legends on that side of any subplot are being collected.
    #' - `theme`: The global [`theme`][ggplot2::theme] of the parent
    #'   [`alignpatches()`] object.
    #' - `tagger`: Either `NULL` (no tagging) or a `LayoutTagger` object that
    #'   provides a `$tag_table` method (accepting the `gtable` and `theme`)
    #'   used to add tag.
    #'
    #' **Value**
    #' A standardized `gtable` object representing the plot layout.
    gtable = function(self, theme = NULL, guides = NULL, tagger = NULL) {
        cli_abort("{.fn gtable} method is not defined")
    },

    #' @field collect_guides
    #' **Description**
    #'
    #' (Optional method) Collects guide legends from the `self$gt` and
    #' optionally removes the space they occupy.
    #'
    #' This method extracts guide legends based on the sides specified in the
    #' `guides` argument. After collecting the guides, the corresponding space
    #' in the the `self$gt` is removed to free up space, except for guides
    #' placed `inside` the panel.
    #'
    #' **Arguments**
    #' - `guides`: Specifies which sides of guide legends should be collected by
    #'   the parent [`alignpatches()`] object. In most cases, this is the value
    #'   returned by the subplot's `self$guides()` method. For plots along the
    #'   border, any guide legends on that side will always be collected if any
    #'   legends on that side of any subplot are being collected.
    #'
    #' **Value**
    #' A named list of collected guide grobs, with names corresponding to the
    #' sides specified in `guides`.
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

    #' @field align_panel
    #' **Description**
    #'
    #' (Optional method) In most cases, panel sizes do not need to be manually
    #' adjusted when aligning panels, as long as their border sizes are
    #' consistent. However, for gtables with a fixed aspect ratio, this method
    #' adjusts the panel width and height based on user input and the dimensions
    #' of the underlying gtable (`self$gt`) to ensure proper alignment.
    #'
    #' When the internal *numeric value* of either `panel_width` or
    #' `panel_height` is `NA` (i.e., `is.na(as.numeric(...))`), that dimension
    #' is inferred from the gtable while maintaining the aspect ratio for
    #' single-panel layouts when `respect = TRUE`.
    #'
    #' **Arguments**
    #' - `panel_width`/`panel_height`: Unit objects specifying the desired panel
    #'   size. If the internal numeric value of either is `NA`, the size is
    #'   computed from the gtable (`self$gt`).
    #'
    #' **Value**
    #' A list with components:
    #' - `width`: Final panel width as a unit object
    #' - `height`: Final panel height as a unit object
    #' - `respect`: If `TRUE`, the aspect ratio was enforced
    #'
    #' @importFrom ggplot2 find_panel
    align_panel = function(self, panel_width, panel_height) {
        if (!is.gtable(self$gt)) {
            return(list(width = panel_width, height = panel_height))
        }
        panel_pos <- find_panel(self$gt)
        rows <- c(.subset2(panel_pos, "t"), .subset2(panel_pos, "b"))
        cols <- c(.subset2(panel_pos, "l"), .subset2(panel_pos, "r"))
        # Only apply aspect-ratio respect when there is a single facet panel
        if (rows[1L] == rows[2L] && cols[1L] == cols[2L]) {
            respect <- .subset2(self$gt, "respect")

            # Continue only if 'respect' is enabled
            if (isTRUE(respect) || (is.matrix(respect) && any(respect == 1L))) {
                # Determine which dimension(s) can be inferred
                can_set_width <- is.na(as.numeric(panel_width))
                can_set_height <- is.na(as.numeric(panel_height))

                # Extract intrinsic panel dimensions from the gtable
                w <- .subset2(self$gt, "widths")[LEFT_BORDER + 1L]
                h <- .subset2(self$gt, "heights")[TOP_BORDER + 1L]

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

    #' @field place_gt
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
    #' - `t`, `l`, `b`, `r`: Integer positions (top, left, bottom, right)
    #'   specifying where to insert the patch in the target gtable.
    #' - `bg_name`: Name to assign to the background grob, if present.
    #' - `plot_name`: Name to assign to the plot grob. It is important to use
    #'   this name consistently when inserting the plot gtable.
    #' - `bg_z`: Z-order for the background grob (default `1L`).
    #' - `plot_z`: Z-order for the plot grob (default `2L`).
    #'
    #' **Details**
    #' - If the patch contains a background grob named `"background"`, it is
    #'   separated from the main plot and inserted first, followed by the plot
    #'   grob.
    #' - If no background is present, the entire gtable is inserted as the plot
    #'   grob.
    #'
    #' **Value**
    #' - The modified target canvas gtable with the patch's gtable added.
    place_gt = function(self, gtable, t, l, b, r,
                        bg_name, plot_name, bg_z, plot_z) {
        gt <- self$gt
        background <- .subset2(.subset2(gt, "layout"), "name") == "background"
        if (any(background)) {
            bg <- .subset(.subset2(gt, "grobs"), background)
            gt <- subset_gt(gt, !background, trim = FALSE)
            gtable <- gtable_add_grob(
                gtable,
                grobs = bg,
                t = t, l = l, b = b, r = r,
                name = bg_name, z = bg_z
            )
        }
        gtable_add_grob(
            gtable,
            grobs = gt,
            t = t, l = l, b = b, r = r,
            name = plot_name, z = plot_z
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
