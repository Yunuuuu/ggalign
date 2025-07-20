#' @importFrom ggplot2 ggproto
#' @export
alignpatch.alignpatches <- function(x) {
    ggproto(NULL, PatchAlignpatches, plot = x)
}

#' @noRd
PatchAlignpatches <- ggproto("PatchAlignpatches", Patch,
    set_guides = function(self, guides) guides,
    #' @importFrom gtable gtable gtable_add_grob
    #' @importFrom grid unit
    #' @importFrom ggplot2 wrap_dims calc_element zeroGrob theme_get
    patch_gtable = function(self, theme = theme_get(), guides = NULL,
                            top_level = FALSE, plot = self$plot) {
        patches <- lapply(.subset2(plot, "plots"), alignpatch)
        layout <- .subset2(plot, "layout")

        # get the design areas and dims ------------------
        panel_widths <- .subset2(layout, "widths")
        panel_heights <- .subset2(layout, "heights")
        if (is.null(area <- .subset2(layout, "design"))) {
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
            area <- create_area(
                dims[2L], dims[1L],
                .subset2(layout, "byrow")
            )
        } else {
            dims <- c(max(field(area, "b")), max(field(area, "r")))
        }

        # filter `plots` based on the design areas --------------------
        if (vec_size(area) < vec_size(patches)) {
            cli_warn(
                "Too few patch areas to hold all plots. Dropping plots"
            )
            plots <- vec_slice(patches, vec_seq_along(area))
        } else {
            area <- vec_slice(area, seq_along(patches))
        }

        # remove NULL patch -----------------------------------
        keep <- !vapply(patches, is.null, logical(1L), USE.NAMES = FALSE)
        patches <- vec_slice(patches, keep)

        # if no plots, we return empty gtable -----------------
        if (is_empty(patches)) return(make_patch_table()) # styler: off
        self$patches <- patches

        # add borders to patch --------------------------------
        area <- vec_slice(area, keep)
        for (i in seq_along(patches)) {
            patches[[i]]$borders <- c(
                if (field(area, "t")[i] == 1L) "top" else NULL,
                if (field(area, "l")[i] == 1L) "left" else NULL,
                if (field(area, "b")[i] == .subset(dims, 1L)) {
                    "bottom"
                } else {
                    NULL
                },
                if (field(area, "r")[i] == .subset(dims, 2L)) {
                    "right"
                } else {
                    NULL
                }
            )
        }

        # we inherit parameters from the parent --------------------
        # by default, we won't collect any guide legends
        parent_guides <- guides
        guides <- .subset2(layout, "guides") %|w|% parent_guides

        # by default, we use ggplot2 default theme
        if (!is.null(plot$theme)) theme <- theme + plot$theme
        self$theme <- theme

        #######################################################
        # 1. patch_gtable: create the gtable for the patch, will set internal
        #    `gt`
        # 2. `collect_guides`, can change the internal `gt`
        # 3. set_sizes:
        #     - (To-Do) align_panel_spaces: can change the internal `gt`
        #     - align_panel_sizes, can change the internal `gt`
        #     - get_sizes, the widths and heights for the internal `gt`
        # 4. set_grobs: will call `align_border` and `split_gt`, return the
        #    final gtable
        # setup gtable list ----------------------------------
        # Let each patch to determine whether to collect guides
        collected <- lapply(patches, function(patch) patch$set_guides(guides))
        collected_guides <- vector("list", length(patches))

        # Always ensure that plots placed in a border collect their guides, if
        # any guides are to be collected in that border. This prevents overlap,
        # unless the guides will be collected by the parent layout.
        border_with_guides <- unique(unlist(collected, FALSE, FALSE))
        border_with_guides <- setdiff(border_with_guides, parent_guides)
        for (i in seq_along(patches)) {
            patch <- .subset2(patches, i)
            g <- union(
                .subset2(collected, i),
                intersect(border_with_guides, patch$borders)
            )
            patch$gt <- patch$patch_gtable(theme = theme, guides = g)
            collected_guides[i] <- list(patch$collect_guides(g))
        }

        # collect guides  ---------------------------------------
        self$collected_guides <- collect_guides_list(collected_guides)

        # prepare the output ----------------------------------
        gt <- gtable(
            unit(rep(0L, TABLE_COLS * dims[2L]), "null"),
            unit(rep(0L, TABLE_ROWS * dims[1L]), "null")
        )

        # setup sizes for each row/column -----------------------
        gt <- self$set_sizes(
            area, dims, panel_widths, panel_heights,
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
                guide_list = self$collected_guides,
                theme = theme,
                panel_pos = panel_pos,
                gt = gt
            )
        }

        # setup grobs -------------------------------------------
        # For z in the gtable layout
        # 0L: layout background
        # 1L: background of the plot
        # 2L: plot table
        # 3L: foreground of the panel area
        # 4L: legends
        gt <- self$set_grobs(area, patches = patches, gt = gt)

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

        # arrange the grobs
        idx <- order(.subset2(.subset2(gt, "layout"), "z"))
        gt$layout <- vec_slice(.subset2(gt, "layout"), idx)
        gt$grobs <- .subset(.subset2(gt, "grobs"), idx)
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
    collect_guides = function(self, guides, gt = self$gt) {
        collected_guides <- self$collected_guides
        # for guides not collected by the top-level alignpatches, we attach the
        # guides
        self$gt <- self$attach_guide_list(
            collected_guides[
                vec_set_difference(names(collected_guides), guides)
            ],
            gt = gt
        )
        # return guides to be collected
        .subset(collected_guides, guides)
    },
    #' @importFrom grid is.unit unit
    set_sizes = function(self, area, dims,
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
        need_fix <- field(area, "l") == field(area, "r") &
            field(area, "t") == field(area, "b") &
            vapply(
                patches,
                function(patch) patch$respect(),
                logical(1L),
                USE.NAMES = FALSE
            )

        # here we respect the aspect ratio when necessary -----
        # if the width or height is NA, we will guess the panel widths or
        # heights based on the fixed aspect ratio
        guess_widths <- which(is.na(as.numeric(panel_widths)))
        guess_heights <- which(is.na(as.numeric(panel_heights)))
        cols <- field(area, "l")
        rows <- field(area, "t")
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
            area, dims[2L], dims[1L]
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
    set_grobs = function(self, area, patches, gt = self$gt) {
        widths <- .subset2(gt, "widths")
        heights <- .subset2(gt, "heights")
        for (i in seq_along(patches)) {
            loc <- vec_slice(area, i)
            # We must align the borders for the gtable grob with the
            # final plot area sizes
            l <- (field(loc, "l") - 1L) * TABLE_COLS + 1L
            l_widths <- widths[seq(l, l + LEFT_BORDER - 1L)]
            r <- field(loc, "r") * TABLE_COLS
            r_widths <- widths[seq(r - RIGHT_BORDER + 1L, r)]
            t <- (field(loc, "t") - 1L) * TABLE_ROWS + 1L
            t_heights <- heights[seq(t, t + TOP_BORDER - 1L)]
            b <- field(loc, "b") * TABLE_ROWS
            b_heights <- heights[seq(b - BOTTOM_BORDER + 1L, b)]
            patch <- .subset2(patches, i)

            grobs <- patch$split_gt(patch$align_border(
                t = t_heights, l = l_widths,
                b = b_heights, r = r_widths
            ))

            # then we add the plot ---------------------------------
            gt <- patch$add_plot(
                gt, .subset2(grobs, "plot"), t, l, b, r,
                name = paste("plot", i, sep = "-")
            )

            # add background grob ----------------------------------
            if (!is.null(bg <- .subset2(grobs, "bg"))) {
                # we always add background in the beginning --------
                gt <- patch$add_background(
                    gt, bg, t, l, b, r,
                    name = paste("plot", i, "background", sep = "-")
                )
            }

            # remove the grob from the patch, we wont' use it anymore
            patch$gt <- NULL
        }
        gt
    },
    attach_guide_list = function(self, guide_list, theme = self$theme,
                                 panel_pos = find_panel(gt),
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
                    clip = "off", z = 4L, gt = gt
                )
            }
        }
        gt
    },
    #' @importFrom gtable gtable_width gtable_height
    #' @importFrom grid unit.c grobWidth grobHeight
    #' @importFrom ggplot2 find_panel zeroGrob
    attach_guides = function(self, guide_pos, guides, theme,
                             panel_pos = find_panel(gt), ...,
                             gt = self$gt) {
        guide_box <- assemble_guides(guides, guide_pos, theme = theme)
        name <- sprintf("guide-box-collected-%s", guide_pos)
        if (guide_pos == "inside") {
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t,
                l = panel_pos$l,
                b = panel_pos$b,
                r = panel_pos$r,
                name = name,
                ...
            )
            return(gt)
        }
        spacing <- .subset2(theme, "legend.box.spacing")
        if (guide_pos == "left") {
            if (is.gtable(guide_box)) {
                legend_width <- gtable_width(guide_box)
                widths <- unit.c(spacing, legend_width)
            } else {
                legend_width <- grobWidth(guide_box)
                widths <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t,
                l = panel_pos$l - 6L,
                b = panel_pos$b,
                name = name,
                ...
            )
            gt$widths[.subset2(panel_pos, "l") - 5:6] <- widths
        } else if (guide_pos == "right") {
            if (is.gtable(guide_box)) {
                legend_width <- gtable_width(guide_box)
                widths <- unit.c(spacing, legend_width)
            } else {
                legend_width <- grobWidth(guide_box)
                widths <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t,
                l = panel_pos$r + 6L,
                b = panel_pos$b,
                name = name,
                ...
            )
            gt$widths[.subset2(panel_pos, "r") + 5:6] <- widths
        } else if (guide_pos == "bottom") {
            location <- .subset2(theme, "legend.location") %||% "panel"
            place <- switch(location,
                panel = panel_pos,
                list(l = 1L, r = ncol(gt))
            )
            if (is.gtable(guide_box)) {
                legend_height <- gtable_height(guide_box)
                heights <- unit.c(spacing, legend_height)
            } else {
                legend_height <- grobHeight(guide_box)
                heights <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$b + 6L,
                l = place$l,
                r = place$r,
                name = name,
                ...
            )
            gt$heights[.subset2(panel_pos, "b") + 5:6] <- heights
        } else if (guide_pos == "top") {
            location <- .subset2(theme, "legend.location") %||% "panel"
            place <- switch(location,
                panel = panel_pos,
                list(l = 1L, r = ncol(gt))
            )
            if (is.gtable(guide_box)) {
                legend_height <- gtable_height(guide_box)
                heights <- unit.c(spacing, legend_height)
            } else {
                legend_height <- grobHeight(guide_box)
                heights <- unit(c(0, 0), "mm")
            }
            gt <- gtable_add_grob(
                x = gt,
                grobs = guide_box,
                t = panel_pos$t - 6L,
                l = place$l,
                r = place$r,
                name = name,
                ...
            )
            gt$heights[.subset2(panel_pos, "t") - 5:6] <- heights
        }
        gt
    },

    #' @importFrom rlang is_empty
    free_border = function(self, borders, gt = self$gt,
                           patches = self$patches) {
        gt <- self$recurse_lapply(
            function(patch, grob, t, l, b, r) {
                borders <- intersect(borders, c(t, l, b, r))
                if (is_empty(borders)) return(grob) # styler: off
                patch$free_border(borders = borders, gt = grob)
            },
            t = "top", l = "left", b = "bottom", r = "right",
            gt = gt, patches = patches
        )
        # for the collected guides, we should also liberate them
        guide_index <- sprintf("guide-box-collected-%s", borders) %in%
            .subset2(.subset2(gt, "layout"), "name")
        if (any(guide_index)) {
            gt <- PatchGgplot$free_border(
                borders = borders[guide_index], gt = gt
            )
        }
        gt
    },
    align_free_border = function(self, borders,
                                 t = NULL, l = NULL, b = NULL, r = NULL,
                                 gt = self$gt, patches = self$patches) {
        gt <- self$recurse_lapply(
            function(patch, grob, t, l, b, r) {
                patch$align_free_border(
                    borders = borders,
                    t = t, l = l, b = b, r = r, gt = grob
                )
            },
            t = t, l = l, b = b, r = r,
            gt = gt, patches = patches
        )
        PatchGgplot$align_free_border(
            borders = borders,
            t = t, l = l, b = b, r = r, gt = gt
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
        # For each grob, we reuse the method from the patch
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

#' @importFrom grid convertHeight convertWidth unit
table_sizes <- function(sizes, area, ncol, nrow) {
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
        idx <- field(area, area_side) == area_col
        if (any(idx)) {
            max(
                vapply(.subset(widths, idx), .subset, numeric(1L), col_loc),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L), USE.NAMES = FALSE)
    heights <- lapply(sizes, function(size) {
        convertHeight(.subset2(size, "heights"), "mm", valueOnly = TRUE)
    })
    heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
        area_row <- recycle_each(i, TABLE_ROWS)
        row_loc <- recycle_whole(i, TABLE_ROWS)
        area_side <- if (row_loc <= TOP_BORDER + 1L) "t" else "b"
        idx <- field(area, area_side) == area_row
        if (any(idx)) {
            max(
                vapply(
                    .subset(heights, idx), .subset, numeric(1L),
                    row_loc,
                    USE.NAMES = FALSE
                ),
                0L
            )
        } else {
            0L
        }
    }, numeric(1L), USE.NAMES = FALSE)
    list(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
}
