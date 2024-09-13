#' @importFrom ggplot2 ggproto
#' @export
alignpatch.alignpatches <- function(x) {
    ggproto(NULL, PatchAlignpatches, plot = x)
}

PatchAlignpatches <- ggproto("PatchAlignpatches", Patch,
    patches = NULL,
    #' @importFrom gtable gtable gtable_add_grob
    #' @importFrom grid unit
    #' @importFrom ggplot2 wrap_dims calc_element zeroGrob
    patch_gtable = function(self, guides, plot = self$plot) {
        patches <- .subset2(plot, "patches")
        layout <- .subset2(plot, "layout")

        # complete the theme object
        theme <- complete_theme(.subset2(layout, "theme"))

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
        design <- quickdf(design)
        if (nrow(design) < length(patches)) {
            cli::cli_warn(
                "Too few patch areas to hold all plots. Dropping plots"
            )
            plots <- patches[seq_len(nrow(design))]
        } else {
            design <- design[seq_along(patches), , drop = FALSE]
        }
        guides <- .subset2(layout, "guides")

        # remove NULL patch -----------------------------------
        keep <- !vapply(patches, is.null, logical(1L), USE.NAMES = FALSE)
        design <- design[keep, , drop = FALSE]
        patches <- .subset(patches, keep)

        # save patches, patches won't be copy
        self$patches <- patches

        # prepare the output ----------------------------------
        gt <- gtable(
            unit(rep(0L, TABLE_COLS * dims[2L]), "null"),
            unit(rep(0L, TABLE_ROWS * dims[1L]), "null")
        )

        # 1. patch_gtable: create the gtable
        # 2. collect_guides_list: collect_guides, can change the internal `gt`
        # 3. set_sizes:
        #     - (TODO) align_panel_spaces: can change the internal `gt`
        #     - align_panel_sizes, can change the internal `gt`
        # 4. add_grobs: align_border, return the final gtable
        # setup gtable list ----------------------------------
        for (patch in patches) {
            # create gtable from the patch
            patch$gt <- patch$patch_gtable(guides)
        }

        # collect guides  ---------------------------------------
        if (is.null(guides)) {
            collected_guides <- NULL
        } else {
            collected_guides <- self$collect_guides_list(
                guides, patches = patches # styler: off
            )
        }

        # setup sizes for each row/column -----------------------
        gt <- self$set_sizes(
            design, guides,
            dims, panel_widths, panel_heights,
            patches = patches, gt = gt
        )

        # setup grobs -------------------------------------------
        gt <- self$add_grobs(design, patches = patches, gt = gt)

        # add guides into the final gtable ----------------------
        panel_pos <- list(
            t = TOP_BORDER + 1L,
            l = LEFT_BORDER + 1L,
            b = TABLE_ROWS * dims[1L] - BOTTOM_BORDER,
            r = TABLE_COLS * dims[2L] - RIGHT_BORDER
        )
        if (length(collected_guides)) {
            # https://github.com/tidyverse/ggplot2/blob/57ba97fa04dadc6fd73db1904e39a09d57a4fcbe/R/guides-.R#L512
            theme$legend.spacing <- theme$legend.spacing %||% unit(0.5, "lines")
            theme$legend.spacing.y <- calc_element("legend.spacing.y", theme)
            theme$legend.spacing.x <- calc_element("legend.spacing.x", theme)
            theme$legend.box.spacing <- calc_element(
                "legend.box.spacing", theme
            ) %||% unit(0.2, "cm")
            for (guide_pos in names(collected_guides)) {
                gt <- self$attach_guides(
                    guide_pos,
                    guides = .subset2(collected_guides, guide_pos),
                    theme = theme,
                    panel_pos = panel_pos,
                    gt = gt
                )
            }
        }
        # add panel area ---------------------------------------
        gtable_add_grob(
            gt, zeroGrob(),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = -1L,
            name = "panel-area"
        )
    },
    align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                            gt = self$gt, patches = self$patches) {
        if (is.null(t) && is.null(l) && is.null(b) && is.null(r)) {
            return(gt)
        }
        n_row <- nrow(gt)
        n_col <- ncol(gt)
        if (!is.null(t)) gt$heights[seq_along(t)] <- t
        if (!is.null(l)) gt$widths[seq_along(l)] <- l
        if (!is.null(b)) gt$heights[seq(n_row - length(b) + 1L, n_row)] <- b
        if (!is.null(r)) gt$widths[seq(n_col - length(r) + 1L, n_col)] <- r
        grobs <- .subset2(gt, "grobs")
        layout <- .subset2(gt, "layout")
        len <- length(patches)
        gt$grobs[seq_len(len)] <- lapply(seq_len(len), function(i) {
            # For each plot grob, we reuse it's method to set the border
            # sizes we only set the border sizes for plot in the border
            .subset2(patches, i)$align_border(
                t = if (.subset2(layout, "t")[i] == 1L) t else NULL,
                l = if (.subset2(layout, "l")[i] == 1L) l else NULL,
                b = if (.subset2(layout, "b")[i] == n_row) b else NULL,
                r = if (.subset2(layout, "r")[i] == n_col) r else NULL,
                gt = .subset2(grobs, i)
            )
        })
        gt
    },
    collect_guides_list = function(self, guides, patches) {
        ans <- lapply(patches, function(patch) patch$collect_guides(guides))
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
    set_sizes = function(self, design, guides, dims,
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
                guides = guides,
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
            lapply(patches, function(patch) patch$widths()),
            lapply(patches, function(patch) patch$heights()),
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
    add_grobs = function(self, design, patches, gt = self$gt) {
        widths <- .subset2(gt, "widths")
        heights <- .subset2(gt, "heights")
        for (i in seq_along(patches)) {
            loc <- design[i, , drop = FALSE]
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
            grob <- patch$align_border(
                t = t_heights, l = l_widths,
                b = b_heights, r = r_widths
            )
            # we always add background in the beginning
            index <- .subset2(.subset2(grob, "layout"), "name") == "background"
            bg <- .subset(.subset2(grob, "grobs"), index)
            if (length(bg) == 1L) {
                name <- paste("plot", i, "background", sep = "-")
            } else {
                name <- paste("plot", i, "background",
                    seq_along(bg),
                    sep = "-"
                )
            }
            gt <- gtable_add_grob(gt, bg, t, l, b, r,
                name = name, z = 0L
            )
            grob <- subset_gt(grob, !index, trim = FALSE)
            gt <- gtable_add_grob(gt, list(grob), t, l, b, r,
                name = paste("plot", i, sep = "-")
            )
            # remove the grob from the patch, we wont' use it anymore
            patch$gt <- NULL
        }
        # always move background grobs to the end
        layout <- .subset2(gt, "layout")
        bg <- grep("plot-\\d+-background", .subset2(layout, "name"))
        index <- seq_len(nrow(layout))
        index <- c(index[-bg], index[bg])
        gt$layout <- layout[index, , drop = FALSE]
        gt$grobs <- .subset(gt$grobs, index)
        gt
    },
    #' @importFrom gtable gtable_width gtable_height
    #' @importFrom grid unit.c unit
    #' @importFrom ggplot2 calc_element
    attach_guides = function(self, guide_pos, guides, theme,
                             panel_pos = find_panel(gt), gt = self$gt) {
        guides <- assemble_guides(guides, guide_pos, theme = theme)
        spacing <- .subset2(theme, "legend.box.spacing")
        legend_width <- gtable_width(guides)
        legend_height <- gtable_height(guides)
        if (guide_pos == "left") {
            gt <- gtable_add_grob(gt, guides,
                clip = "off",
                t = panel_pos$t,
                l = panel_pos$l - 5L,
                b = panel_pos$b,
                name = "guide-box-collected-left"
            )
            gt <- self$align_border(
                l = unit.c(
                    gt$widths[seq_len(panel_pos$l - 6L)],
                    legend_width, spacing
                ),
                gt = gt
            )
        } else if (guide_pos == "right") {
            gt <- gtable_add_grob(gt, guides,
                clip = "off", t = panel_pos$t,
                l = panel_pos$r + 5L, b = panel_pos$b,
                name = "guide-box-collected-right"
            )
            gt <- self$align_border(
                r = unit.c(
                    spacing, legend_width,
                    gt$widths[seq(panel_pos$r + 6L, ncol(gt))]
                ),
                gt = gt
            )
        } else if (guide_pos == "bottom") {
            gt <- gtable_add_grob(gt, guides,
                clip = "off", t = panel_pos$b + 5L,
                l = panel_pos$l, r = panel_pos$r,
                name = "guide-box-collected-bottom"
            )
            gt <- self$align_border(
                b = unit.c(
                    spacing, legend_height,
                    gt$heights[seq(panel_pos$b + 6L, nrow(gt))]
                ),
                gt = gt
            )
        } else if (guide_pos == "top") {
            gt <- gtable_add_grob(gt, guides,
                clip = "off", t = panel_pos$t - 5L,
                l = panel_pos$l, r = panel_pos$r,
                name = "guide-box-collected-top"
            )
            gt <- self$align_border(
                t = unit.c(
                    gt$heights[seq_len(panel_pos$t - 6L)],
                    legend_height, spacing
                ),
                gt = gt
            )
        }
        gt
    },
    free_border = function(self, guides, borders,
                           gt = self$gt, patches = self$patches) {
        len <- length(patches)
        grobs <- .subset2(gt, "grobs")
        layout <- .subset2(gt, "layout")
        n_row <- nrow(gt)
        n_col <- ncol(gt)
        gt$grobs[seq_len(len)] <- lapply(seq_len(len), function(i) {
            borders <- intersect(borders, c(
                if (.subset2(layout, "t")[i] == 1L) "top" else NULL,
                if (.subset2(layout, "l")[i] == 1L) "left" else NULL,
                if (.subset2(layout, "b")[i] == n_row) "bottom" else NULL,
                if (.subset2(layout, "r")[i] == n_col) "right" else NULL
            ))
            # the second is the actual
            grob <- .subset2(grobs, i)
            if (length(borders) == 0L) {
                return(grob)
            }
            # For each plot grob, we reuse it's method to attach the border
            # we only attach border for plot in the border
            .subset2(patches, i)$free_border(
                guides,
                borders = borders, gt = grob
            )
        })
        gt
    },
    free_lab = function(self, labs, gt = self$gt, patches = self$patches) {
        len <- length(patches)
        grobs <- .subset2(gt, "grobs")
        layout <- .subset2(gt, "layout")
        n_row <- nrow(gt)
        n_col <- ncol(gt)
        gt$grobs[seq_len(len)] <- lapply(seq_len(len), function(i) {
            labs <- intersect(labs, c(
                if (.subset2(layout, "t")[i] == 1L) "top" else NULL,
                if (.subset2(layout, "l")[i] == 1L) "left" else NULL,
                if (.subset2(layout, "b")[i] == n_row) "bottom" else NULL,
                if (.subset2(layout, "r")[i] == n_col) "right" else NULL
            ))
            grob <- .subset2(grobs, i)
            if (length(labs) == 0L) {
                return(grob)
            }
            # For each plot grob, we reuse it's method to attach the labs
            # we only attach labs for plot in the border
            .subset2(patches, i)$free_lab(
                labs = labs, gt = grob
            )
        })
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
table_sizes <- function(widths, heights, design, ncol, nrow) {
    # `null` unit of the panel area will be converted into 0
    # we'll set the panel width and height afterward
    widths <- lapply(widths, convertWidth, "mm", valueOnly = TRUE)
    widths <- vapply(seq_len(ncol * TABLE_COLS), function(i) {
        area_col <- (i - 1L) %/% TABLE_COLS + 1L
        col_loc <- i %% TABLE_COLS
        if (col_loc == 0L) col_loc <- TABLE_COLS
        area_side <- if (col_loc <= PANEL_COL) "l" else "r"
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
    heights <- lapply(heights, convertHeight, "mm", valueOnly = TRUE)
    heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
        area_row <- (i - 1L) %/% TABLE_ROWS + 1L
        row_loc <- i %% TABLE_ROWS
        if (row_loc == 0L) row_loc <- TABLE_ROWS
        area_side <- if (row_loc <= PANEL_COL) "t" else "b"
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
