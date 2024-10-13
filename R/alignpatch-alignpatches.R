#' @importFrom ggplot2 ggproto
#' @export
alignpatch.alignpatches <- function(x) {
    ggproto(NULL, PatchAlignpatches, plot = x)
}

#' @importFrom vctrs new_data_frame
PatchAlignpatches <- ggproto("PatchAlignpatches", Patch,
    # We by default won't collect any guides
    guides = NULL,
    set_theme = function(theme) theme,
    #' @importFrom gtable gtable gtable_add_grob
    #' @importFrom grid unit
    #' @importFrom ggplot2 wrap_dims calc_element zeroGrob
    #' @importFrom vctrs vec_slice
    patch_gtable = function(self, top_level = FALSE, plot = self$plot) {
        patches <- lapply(.subset2(plot, "plots"), alignpatch)
        layout <- .subset2(plot, "layout")

        # get the design areas and design dims ------------------
        panel_widths <- .subset2(layout, "widths")
        panel_heights <- .subset2(layout, "heights")
        if (is.null(design <- .subset2(layout, "design"))) {
            if (is.null(layout$ncol) && length(panel_widths) > 1L) {
                layout$ncol <- length(panel_widths)
            }
            if (is.null(layout$nrow) && length(panel_heights) > 1L) {
                layout$nrow <- length(panel_heights)
            }
            dims <- wrap_dims(
                length(patches),
                .subset2(layout, "nrow"),
                .subset2(layout, "ncol")
            )
            design <- create_design(
                dims[2L], dims[1L],
                .subset2(layout, "byrow")
            )
        } else {
            dims <- c(max(.subset2(design, "b")), max(.subset2(design, "r")))
        }

        # filter `plots` based on the design areas --------------------
        if (nrow(design) < length(patches)) {
            cli::cli_warn(
                "Too few patch areas to hold all plots. Dropping plots"
            )
            plots <- patches[seq_len(nrow(design))]
        } else {
            design <- vec_slice(design, seq_along(patches))
        }

        # remove NULL patch -----------------------------------
        keep <- !vapply(patches, is.null, logical(1L), USE.NAMES = FALSE)
        patches <- .subset(patches, keep)

        # if no plots, we return empty gtable -----------------
        if (is_empty(patches)) return(make_patch_table()) # styler: off

        # add borders to patch --------------------------------
        design <- vec_slice(design, keep)
        for (i in seq_along(patches)) {
            patches[[i]]$borders <- c(
                if (.subset2(design, "t")[i] == 1L) "top" else NULL,
                if (.subset2(design, "l")[i] == 1L) "left" else NULL,
                if (.subset2(design, "b")[i] == .subset(dims, 1L)) {
                    "bottom"
                } else {
                    NULL
                },
                if (.subset2(design, "r")[i] == .subset(dims, 2L)) {
                    "right"
                } else {
                    NULL
                }
            )
        }

        # we inherit parameters from the parent --------------------
        # in `alignpatches` object, `self$guides` and `self$theme` will always
        # be the parent parameters, we always attach the parent `gudies` and
        # `theme` in `self`
        #
        # `self$guides` by default is `NULl`, if this is a
        # nested alignpatches, `self$guides` should be set by
        # `$set_guides()` method, which is the parent guides
        guides <- .subset2(layout, "guides") %|w|% self$guides
        # the same applies to theme but use `$set_theme()` method
        # the top-level alignpatches will always complete theme in
        # `ggalign_gtable.alignpatches`.
        if (is.null(theme <- .subset2(plot, "theme"))) {
            theme <- self$theme
        } else {
            theme <- complete_theme(theme)
        }

        for (patch in patches) {
            # Let each patch to determine whether to collect guides
            patch$guides <- patch$set_guides(guides)
            # inherit theme for nested `alignpatches` object only
            patch$theme <- patch$set_theme(theme)
        }

        # save patches, patches won't be copy -------------------
        self$patches <- patches

        #######################################################
        # 1. patch_gtable: create the gtable for the patch, will set internal
        #    `gt`
        # 2. collect_guides_list: call `collect_guides`, can change the internal
        #    `gt`
        # 3. set_sizes:
        #     - (TODO) align_panel_spaces: can change the internal `gt`
        #     - align_panel_sizes, can change the internal `gt`
        # 4. set_grobs: will call `align_border` and `split_gt`, return the
        #    final gtable
        # setup gtable list ----------------------------------
        for (patch in patches) patch$gt <- patch$patch_gtable()

        # collect guides  ---------------------------------------
        collected_guides <- self$collect_guides_list(patches)

        # prepare the output ----------------------------------
        gt <- gtable(
            unit(rep(0L, TABLE_COLS * dims[2L]), "null"),
            unit(rep(0L, TABLE_ROWS * dims[1L]), "null")
        )

        # setup sizes for each row/column -----------------------
        gt <- self$set_sizes(
            design, dims, panel_widths, panel_heights,
            patches = patches, gt = gt
        )

        # add the panel position --------------------------------
        panel_pos <- list(
            t = TOP_BORDER + 1L,
            l = LEFT_BORDER + 1L,
            b = TABLE_ROWS * dims[1L] - BOTTOM_BORDER,
            r = TABLE_COLS * dims[2L] - RIGHT_BORDER
        )

        # add guides into the final gtable ----------------------
        if (top_level) {
            gt <- self$attach_guide_list(
                guide_list = collected_guides,
                theme = theme,
                panel_pos = panel_pos,
                gt = gt
            )
        } else {
            # used by `$collect_guides() method`
            # if this is a nested alignpatches, theme will be automatically
            # attached by` $set_theme()`
            self$collected_guides <- collected_guides
            self$panel_pos <- panel_pos
        }

        # setup grobs -------------------------------------------
        # For z in the gtable layout
        # 0L: layout background
        # 1L: background of the plot
        # 2L: plot table
        # 3L: foreground of the panel area
        # 4L: legends
        gt <- self$set_grobs(design, patches = patches, gt = gt)

        # add panel area ---------------------------------------
        gt <- gtable_add_grob(
            gt, list(zeroGrob()),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = 0L,
            name = "panel-area"
        )
        gt <- gtable_add_grob(
            gt,
            # foreground
            list(element_render(theme, "panel.border", fill = NA)),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = 3L,
            name = "panel-foreground"
        )

        # add background -----------------------------------
        if (!top_level && inherits(theme$plot.background, "element")) {
            gt <- gtable_add_grob(gt,
                element_render(theme, "plot.background"),
                t = 1L, l = 1L, b = -1L, r = -1L,
                name = "background", z = 0L
            )
        }
        gt
    },
    align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                            gt = self$gt, patches = self$patches) {
        if (is.null(t) && is.null(l) && is.null(b) && is.null(r)) {
            return(gt)
        }
        gt <- Patch$align_border(t = t, l = l, b = b, r = r, gt = gt)
        self$recurse_lapply(function(patch, grob, t, l, b, r) {
            # For each plot grob, we reuse it's method to set the border
            # sizes we only set the border sizes for plot in the border
            patch$align_border(t = t, l = l, b = b, r = r, gt = grob)
        }, t = t, l = l, b = b, r = r, gt = gt, patches = patches)
    },
    #' @importFrom vctrs vec_set_difference
    collect_guides = function(self, guides = self$guides, gt = self$gt) {
        collected_guides <- self$collected_guides
        # for guides not collected by the top-level, we attach the guides
        self$gt <- self$attach_guide_list(
            .subset(
                collected_guides,
                vec_set_difference(names(collected_guides), guides)
            ),
            gt = gt
        )
        # return guides to be collected
        .subset(collected_guides, guides)
    },
    collect_guides_list = function(self, patches) {
        ans <- lapply(patches, function(patch) patch$collect_guides())
        # collapse the guides in the same guide position
        ans <- lapply(.TLBR, function(guide_pos) {
            unlist(lapply(ans, .subset2, guide_pos), FALSE, FALSE)
        })
        names(ans) <- .TLBR
        ans <- compact(ans)
        # remove duplicated guides
        ans <- lapply(ans, collapse_guides)
        compact(ans)
    },
    #' @importFrom grid is.unit unit
    set_sizes = function(self, design, dims,
                         panel_widths, panel_heights,
                         patches, gt = self$gt) {
        panel_widths <- rep(panel_widths, length.out = dims[2L])
        panel_heights <- rep(panel_heights, length.out = dims[1L])
        if (!is.unit(panel_widths)) panel_widths <- unit(panel_widths, "null")
        if (!is.unit(panel_heights)) {
            panel_heights <- unit(panel_heights, "null")
        }

        # For gtable with fixed aspect ratio -------------
        # if it cannot be fixed and aligned, the strip, axis and labs will be
        # attached into the panel
        # the plot to be fixed must in only one square of the area
        need_fix <- .subset2(design, "l") == .subset2(design, "r") &
            .subset2(design, "t") == .subset2(design, "b") &
            vapply(patches, function(patch) patch$respect(), logical(1L))

        # here we respect the aspect ratio when necessary -----
        # if the width or height is NA, we will guess the panel widths or
        # heights based on the fixed aspect ratio
        guess_widths <- which(is.na(as.numeric(panel_widths)))
        guess_heights <- which(is.na(as.numeric(panel_heights)))
        cols <- .subset2(design, "l")
        rows <- .subset2(design, "t")
        patch_index <- order(
            # we first set the widths for the fixed plot with heights set by
            # user
            cols %in% guess_widths & !rows %in% guess_heights,
            # we then set the heights for the fixed plot with widths set by user
            !cols %in% guess_widths & rows %in% guess_heights,
            # we set widths and heights for remaning plots
            # based on the number of plots in each row/column in the descending
            # order
            c(table(rows[need_fix]))[as.character(rows)],
            c(table(cols[need_fix]))[as.character(cols)],
            decreasing = TRUE
        )
        respect_dims <- vector("list", length(patches))

        # For plot cannot be fixed, we always attach strips, axes and labels
        # into the panel area
        for (i in patch_index) {
            row <- .subset(rows, i)
            col <- .subset(cols, i)
            # we always build a standard gtable layout from the gtable
            panel_sizes <- .subset2(patches, i)$align_panel_sizes(
                panel_width = panel_widths[col],
                panel_height = panel_heights[row]
            )
            panel_widths[col] <- .subset2(panel_sizes, "width")
            panel_heights[row] <- .subset2(panel_sizes, "height")
            if (.subset2(panel_sizes, "respect")) {
                respect_dims[[i]] <- matrix(c(
                    (row - 1L) * TABLE_ROWS + TOP_BORDER + 1L,
                    (col - 1L) * TABLE_COLS + LEFT_BORDER + 1L
                ), nrow = 1L)
            }
        }
        if (!is.null(respect_dims <- do.call(base::rbind, respect_dims))) {
            respect <- matrix(
                0L, TABLE_ROWS * dims[1L],
                TABLE_COLS * dims[2L]
            )
            respect[respect_dims] <- 1L
            gt$respect <- respect
        }

        # we set the widths/heights with no fixed plots to be 1 null
        if (any(guess_widths <- is.na(as.numeric(panel_widths)))) {
            panel_widths[guess_widths] <- unit(1L, "null")
        }
        if (any(guess_heights <- is.na(as.numeric(panel_heights)))) {
            panel_heights[guess_heights] <- unit(1L, "null")
        }

        # setup sizes for non-panel rows/columns --------------
        sizes <- table_sizes(
            lapply(patches, function(patch) patch$get_sizes()),
            design, dims[2L], dims[1L]
        )
        widths <- .subset2(sizes, "widths")
        heights <- .subset2(sizes, "heights")

        # restore the panel sizes ----------------------------
        width_ind <- seq(LEFT_BORDER + 1L,
            by = TABLE_COLS, length.out = dims[2L]
        )
        height_ind <- seq(TOP_BORDER + 1L,
            by = TABLE_ROWS, length.out = dims[1L]
        )
        widths[width_ind] <- panel_widths
        heights[height_ind] <- panel_heights

        # setup the widths and heights -----------------------
        gt$widths <- widths
        gt$heights <- heights
        gt
    },

    #' @importFrom gtable gtable_add_grob
    #' @importFrom vctrs vec_slice
    set_grobs = function(self, design, patches, gt = self$gt) {
        widths <- .subset2(gt, "widths")
        heights <- .subset2(gt, "heights")
        for (i in seq_along(patches)) {
            loc <- vec_slice(design, i)
            # We must align the borders for the gtable grob with the
            # final plot area sizes
            l <- (.subset2(loc, "l") - 1L) * TABLE_COLS + 1L
            l_widths <- widths[seq(l, l + LEFT_BORDER - 1L)]
            r <- .subset2(loc, "r") * TABLE_COLS
            r_widths <- widths[seq(r - RIGHT_BORDER + 1L, r)]
            t <- (.subset2(loc, "t") - 1L) * TABLE_ROWS + 1L
            t_heights <- heights[seq(t, t + TOP_BORDER - 1L)]
            b <- .subset2(loc, "b") * TABLE_ROWS
            b_heights <- heights[seq(b - BOTTOM_BORDER + 1L, b)]
            patch <- .subset2(patches, i)

            grobs <- patch$split_gt(patch$align_border(
                t = t_heights, l = l_widths,
                b = b_heights, r = r_widths
            ))

            # then we add the plot ---------------------------------
            gt <- gtable_add_grob(gt,
                list(.subset2(grobs, "plot")), t, l, b, r,
                name = paste("plot", i, sep = "-"), z = 2L
            )

            # add background grob ----------------------------------
            if (!is.null(bg <- .subset2(grobs, "bg"))) {
                # we always add background in the beginning --------
                gt <- gtable_add_grob(gt, bg, t, l, b, r,
                    name = paste("plot", i, "background", sep = "-"),
                    z = 1L
                )
            }

            # remove the grob from the patch, we wont' use it anymore
            patch$gt <- NULL
        }
        gt
    },
    attach_guide_list = function(self, guide_list, theme = self$theme,
                                 panel_pos = self$panel_pos,
                                 gt = self$gt) {
        if (length(guide_list)) {
            # https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/guides-.R#L512
            theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
            theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
            theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)
            theme$legend.box.spacing <- calc_element(
                "legend.box.spacing", theme
            ) %||% unit(0.2, "cm")
            for (guide_pos in names(guide_list)) {
                gt <- self$attach_guides(
                    guide_pos = guide_pos,
                    guides = .subset2(guide_list, guide_pos),
                    theme = theme, panel_pos = panel_pos,
                    name = sprintf("guide-box-collected-%s", guide_pos),
                    clip = "off", z = 4L, gt = gt
                )
            }
        }
        gt
    },
    #' @importFrom gtable gtable_width gtable_height
    #' @importFrom grid unit.c
    #' @importFrom ggplot2 find_panel
    attach_guides = function(self, guide_pos, guides, theme,
                             panel_pos = find_panel(gt), ...,
                             gt = self$gt) {
        guides <- assemble_guides(guides, guide_pos, theme = theme)
        spacing <- .subset2(theme, "legend.box.spacing")
        legend_width <- gtable_width(guides)
        legend_height <- gtable_height(guides)
        if (guide_pos == "left") {
            gt <- gtable_add_grob(
                x = gt,
                grobs = guides,
                t = panel_pos$t,
                l = panel_pos$l - 6L,
                b = panel_pos$b,
                ...
            )
            gt$widths[.subset2(panel_pos, "l") - 5:6] <- unit.c(
                spacing, legend_width
            )
        } else if (guide_pos == "right") {
            gt <- gtable_add_grob(
                x = gt, grobs = guides,
                t = panel_pos$t,
                l = panel_pos$r + 6L,
                b = panel_pos$b,
                ...
            )
            gt$widths[.subset2(panel_pos, "r") + 5:6] <- unit.c(
                spacing, legend_width
            )
        } else if (guide_pos == "bottom") {
            gt <- gtable_add_grob(
                x = gt,
                grobs = guides,
                t = panel_pos$b + 6L,
                l = panel_pos$l,
                r = panel_pos$r,
                ...
            )
            gt$heights[.subset2(panel_pos, "b") + 5:6] <- unit.c(
                spacing, legend_height
            )
        } else if (guide_pos == "top") {
            gt <- gtable_add_grob(
                x = gt,
                grobs = guides,
                t = panel_pos$t - 6L,
                l = panel_pos$l,
                r = panel_pos$r,
                ...
            )
            gt$heights[.subset2(panel_pos, "t") - 5:6] <- unit.c(
                spacing, legend_height
            )
        }
        gt
    },
    #' @importFrom rlang is_empty
    free_border = function(self, borders,
                           gt = self$gt, patches = self$patches) {
        self$recurse_lapply(
            function(patch, grob, t, l, b, r) {
                borders <- intersect(borders, c(t, l, b, r))
                if (is_empty(borders)) return(grob) # styler: off
                patch$free_border(borders = borders, gt = grob)
            },
            t = "top", l = "left", b = "bottom", r = "right",
            gt = gt, patches = patches
        )
    },
    #' @importFrom rlang is_empty
    free_lab = function(self, labs, gt = self$gt, patches = self$patches) {
        self$recurse_lapply(
            function(patch, grob, t, l, b, r) {
                labs <- intersect(labs, c(t, l, b, r))
                if (is_empty(labs)) return(grob) # styler: off
                patch$free_lab(labs = labs, gt = grob)
            },
            t = "top", l = "left", b = "bottom", r = "right",
            gt = gt, patches = patches
        )
    },
    # we apply function in each plot gtable in `gt`.
    #' @importFrom rlang is_empty
    recurse_lapply = function(self, .fn, t, l, b, r,
                              gt = self$gt, patches = self$patches) {
        # if no plot provided, we'll do nothing
        if (is_empty(patches)) return(gt) # styler: off
        patch_index <- seq_along(patches)
        grobs <- .subset2(gt, "grobs")
        layout_index <- match(
            paste0("plot-", patch_index),
            .subset2(.subset2(gt, "layout"), "name")
        )
        # For each grob, we reuse the method from the patch to apply with the
        # plot gtable. We always apply function to the plot border in the
        # alignpatches
        gt$grobs[layout_index] <- .mapply(function(layout_idx, patch_idx) {
            patch <- .subset2(patches, patch_idx)
            borders <- .subset2(patch, "borders")
            .fn(
                patch = patch,
                grob = .subset2(grobs, layout_idx),
                t = if (any(borders == "top")) t else NULL,
                l = if (any(borders == "left")) l else NULL,
                b = if (any(borders == "bottom")) b else NULL,
                r = if (any(borders == "right")) r else NULL
            )
        }, list(layout_idx = layout_index, patch_idx = patch_index), NULL)
        gt
    }
)

create_design <- function(ncol, nrow, byrow) {
    mat <- matrix(seq_len(ncol * nrow),
        nrow = nrow, ncol = ncol, byrow = byrow
    )
    ind <- as.vector(mat)
    ind <- match(seq_along(ind), ind)
    area(t = row(mat)[ind], l = col(mat)[ind])
}

#' @importFrom grid convertHeight convertWidth unit
table_sizes <- function(sizes, design, ncol, nrow) {
    # `null` unit of the panel area will be converted into 0
    # we'll set the panel width and height afterward
    widths <- lapply(sizes, function(size) {
        convertWidth(.subset2(size, "widths"), "mm", valueOnly = TRUE)
    })
    widths <- vapply(seq_len(ncol * TABLE_COLS), function(i) {
        area_col <- (i - 1L) %/% TABLE_COLS + 1L
        col_loc <- i %% TABLE_COLS
        if (col_loc == 0L) col_loc <- TABLE_COLS
        area_side <- if (col_loc <= LEFT_BORDER + 1L) "l" else "r"
        idx <- .subset2(design, area_side) == area_col
        if (any(idx)) {
            max(
                vapply(.subset(widths, idx), .subset, numeric(1L), col_loc),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L))
    heights <- lapply(sizes, function(size) {
        convertHeight(.subset2(size, "heights"), "mm", valueOnly = TRUE)
    })
    heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
        area_row <- (i - 1L) %/% TABLE_ROWS + 1L
        row_loc <- i %% TABLE_ROWS
        if (row_loc == 0L) row_loc <- TABLE_ROWS
        area_side <- if (row_loc <= TOP_BORDER + 1L) "t" else "b"
        idx <- .subset2(design, area_side) == area_row
        if (any(idx)) {
            max(
                vapply(.subset(heights, idx), .subset, numeric(1L), row_loc),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L))
    list(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
}
