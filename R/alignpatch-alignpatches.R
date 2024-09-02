#' @export
patch_align.gtable_alignpatches <- function(gt, guides,
                                            panel_width, panel_height) {
    list(
        gt = make_full_patch(gt, clip = "off", name = "alignpatches-table"),
        width = panel_width, height = panel_height, respect = FALSE
    )
}

#' @importFrom gtable gtable
#' @importFrom grid unit
#' @export
patch_gtable.alignpatches <- function(patch, guides) {
    plots <- .subset2(patch, "plots")
    layout <- .subset2(patch, "layout")

    # complete the theme object
    theme <- complete_theme(.subset2(layout, "theme"))

    # get the design areas and design dims ------------------
    panel_widths <- .subset2(layout, "widths")
    panel_heights <- .subset2(layout, "heights")
    if (is.null(design <- .subset2(layout, "design"))) {
        if (is.null(layout$ncol) && length(panel_widths) > 1L) {
            layout$ncol <- length(panel_widths)
        }
        if (is.null(layout$nrow) && length(panel_heights) > 1) {
            layout$nrow <- length(panel_heights)
        }
        dims <- ggplot2::wrap_dims(
            length(plots),
            .subset2(layout, "nrow"),
            .subset2(layout, "ncol")
        )
        design <- create_design(dims[2L], dims[1L], .subset2(layout, "byrow"))
    } else {
        dims <- c(max(.subset2(design, "b")), max(.subset2(design, "r")))
    }

    # filter gt based on the design areas --------------------
    design <- quickdf(design)
    if (nrow(design) < length(plots)) {
        cli::cli_warn("Too few patch areas to hold all plots. Dropping plots")
        plots <- plots[seq_len(nrow(design))]
    } else {
        design <- design[seq_along(plots), , drop = FALSE]
    }

    # we use R6 object here, since we need modify the gtables of the plots
    # across the building process
    builder <- ggplot2::ggproto(NULL, BuilderAlignPatches, gt_list = NULL)
    builder$build(plots,
        design = design, dims = dims,
        panel_widths = panel_widths,
        panel_heights = panel_heights,
        guides = .subset2(layout, "guides"),
        theme = theme
    )
}

#' @importFrom ggplot2 zeroGrob
#' @importFrom gtable gtable gtable_add_grob
#' @importFrom grid unit
BuilderAlignPatches <- ggplot2::ggproto(
    "BuilderAlignPatches", NULL,
    build = function(self, plots, design, dims,
                     panel_widths, panel_heights, guides, theme) {
        # prepare the output ----------------------------------
        gt <- gtable(
            unit(rep(0L, TABLE_COLS * dims[2L]), "null"),
            unit(rep(0L, TABLE_ROWS * dims[1L]), "null")
        )

        # setup gtable list ----------------------------------
        # create gtable from the plot
        self$gt_list <- lapply(plots, patch_gtable, guides = guides)

        # collect guides  ---------------------------------------
        if (length(guides)) {
            # this'll also remove guides from `gt_list`
            collected_guides <- self$collect_guides(guides)
        } else {
            collected_guides <- NULL
        }

        # setup sizes for each row/column -----------------------
        gt <- self$set_sizes(
            gt, design, guides,
            dims, panel_widths, panel_heights
        )

        # setup grobs -------------------------------------------
        gt <- self$set_grobs(gt, design)

        # setup the output layout -------------------------------
        gt <- self$set_layout(gt, design)

        # `attach_guides` will use `align_border_size`
        # we add the class here for usage of `attach_guides`
        gt <- add_class(gt, "gtable_alignpatches")

        # add guides into the final gtable ----------------------
        panel_pos <- list(
            t = PANEL_ROW,
            l = PANEL_COL,
            b = PANEL_ROW + TABLE_ROWS * (dims[1L] - 1L),
            r = PANEL_COL + TABLE_COLS * (dims[2L] - 1L)
        )
        if (length(collected_guides)) {
            for (guide_pos in names(collected_guides)) {
                gt <- attach_guides(
                    gt, guide_pos,
                    guides = .subset2(collected_guides, guide_pos),
                    theme = theme,
                    panel_pos = panel_pos
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
    collect_guides = function(self, guides,
                              gt_list = .subset2(self, "gt_list")) {
        ans <- vector("list", length(gt_list))
        for (i in seq_along(gt_list)) {
            split <- collect_guides(.subset2(gt_list, i), guides)
            gt_list[[i]] <- .subset2(split, "gt")
            ans[[i]] <- .subset2(split, "guides")
        }
        # save the modified `gt_list`
        self$gt_list <- gt_list

        # collapse the guides in the same guide position
        # remove duplicated guides
        ans <- lapply(BORDERS, function(guide_pos) {
            unlist(
                lapply(ans, .subset2, guide_pos),
                recursive = FALSE, use.names = FALSE
            )
        })
        names(ans) <- BORDERS
        ans <- compact(ans)
        ans <- lapply(ans, collapse_guides)
        compact(ans)
    },
    set_sizes = function(self, gt, design, guides, dims,
                         panel_widths, panel_heights,
                         gt_list = .subset2(self, "gt_list")) {
        panel_widths <- rep(panel_widths, length.out = dims[2])
        panel_heights <- rep(panel_heights, length.out = dims[1])
        if (!is.unit(panel_widths)) panel_widths <- unit(panel_widths, "null")
        if (!is.unit(panel_heights)) {
            panel_heights <- unit(panel_heights, "null")
        }

        # For gtable with fixed aspect ratio -------------
        # if it cannot be fixed and aligned, the strip, axis and labs will be
        # attached into the panel
        # the plot to be fixed must in only one squre of the area
        need_fix <- .subset2(design, "l") == .subset2(design, "r") &
            .subset2(design, "t") == .subset2(design, "b") &
            vapply(gt_list, function(gt) {
                isTRUE(.subset2(gt, "respect"))
            }, logical(1L))

        # here we respect the aspect ratio when necessary -----
        # if the width or height is NA, we will guess the panel widths or
        # heights based on the fixed aspect ratio
        guess_widths <- which(is.na(as.numeric(panel_widths)))
        guess_heights <- which(is.na(as.numeric(panel_heights)))
        cols <- .subset2(design, "l")
        rows <- .subset2(design, "t")
        gt_index <- order(
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
        respect_dims <- vector("list", length(gt_list))

        # For plot cannot be fixed, we always attach strips, axes and labels
        # into the panel area
        for (i in gt_index) {
            row <- .subset(rows, i)
            col <- .subset(cols, i)
            # we always build a standard gtable layout from the gtable
            aligned_elements <- patch_align(
                gt = .subset2(gt_list, i),
                guides = guides,
                panel_width = panel_widths[col],
                panel_height = panel_heights[row]
            )
            gt_list[[i]] <- .subset2(aligned_elements, "gt")
            panel_widths[col] <- .subset2(aligned_elements, "width")
            panel_heights[row] <- .subset2(aligned_elements, "height")
            if (.subset2(aligned_elements, "respect")) {
                respect_dims[[i]] <- matrix(
                    c(
                        (row - 1L) * TABLE_ROWS + PANEL_ROW,
                        (col - 1L) * TABLE_COLS + PANEL_COL
                    ),
                    nrow = 1L
                )
            }
        }

        if (!is.null(respect_dims <- do.call(rbind, respect_dims))) {
            respect <- matrix(
                0L, TABLE_ROWS * dims[1L],
                TABLE_COLS * dims[2]
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
            lapply(gt_list, .subset2, "widths"),
            lapply(gt_list, .subset2, "heights"),
            design, dims[2L], dims[1L]
        )
        widths <- .subset2(sizes, "widths")
        heights <- .subset2(sizes, "heights")

        # restore the panel sizes -----------------------------
        width_ind <- seq(PANEL_COL, by = TABLE_COLS, length.out = dims[2])
        height_ind <- seq(PANEL_ROW, by = TABLE_ROWS, length.out = dims[1L])
        widths[width_ind] <- panel_widths
        heights[height_ind] <- panel_heights

        # setup the widths and heights -----------------------
        gt$widths <- widths
        gt$heights <- heights

        # save the modified `gt_list` (fix_area) -------------
        self$gt_list <- gt_list
        gt
    },
    set_grobs = function(self, gt, design,
                         gt_list = .subset2(self, "gt_list")) {
        widths <- .subset2(gt, "widths")
        heights <- .subset2(gt, "heights")
        gt$grobs <- unlist(lapply(seq_along(gt_list), function(i) {
            # Only used for a `full_patch` object, the actual object should
            # be the second grob in the `full_patch` object.
            # we must align the borders for the underlying grob with
            # the final `gtable` sizes
            align_grobs(
                gt = .subset2(gt_list, i),
                widths = widths, heights = heights,
                loc = design[i, , drop = FALSE]
            )
        }), recursive = FALSE, use.names = FALSE)
        gt
    },
    set_layout = function(self, gt, design,
                          gt_list = .subset2(self, "gt_list")) {
        max_z <- vapply(gt_list, function(x) {
            max(.subset2(.subset2(x, "layout"), "z"))
        }, numeric(1L))
        max_z <- c(0, cumsum(max_z))
        gt$layout <- do.call(
            `rbind`,
            lapply(seq_along(gt_list), function(i) {
                loc <- design[i, , drop = FALSE]
                lay <- .subset2(.subset2(gt_list, i), "layout")
                lay$z <- lay$z + ifelse(lay$name == "background", 0, max_z[i])
                # we only expand the panel area, here is the magic from
                # patchwork
                lay$t <- lay$t + ifelse(
                    lay$t <= PANEL_ROW,
                    (loc$t - 1) * TABLE_ROWS,
                    (loc$b - 1) * TABLE_ROWS
                )
                lay$b <- lay$b + ifelse(
                    lay$b < PANEL_ROW,
                    (loc$t - 1) * TABLE_ROWS,
                    (loc$b - 1) * TABLE_ROWS
                )
                lay$l <- lay$l + ifelse(
                    lay$l <= PANEL_COL,
                    (loc$l - 1) * TABLE_COLS,
                    (loc$r - 1) * TABLE_COLS
                )
                lay$r <- lay$r + ifelse(
                    lay$r < PANEL_COL,
                    (loc$l - 1) * TABLE_COLS,
                    (loc$r - 1) * TABLE_COLS
                )
                lay$name <- paste0(lay$name, "-plot-", i)
                lay
            })
        )
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
        area_col <- (i - 1) %/% TABLE_COLS + 1L
        col_loc <- i %% TABLE_COLS
        if (col_loc == 0) col_loc <- TABLE_COLS
        area_side <- if (col_loc <= PANEL_COL) "l" else "r"
        idx <- .subset2(design, area_side) == area_col
        if (any(idx)) {
            max(vapply(widths[idx], `[[`, numeric(1), col_loc), 0L)
        } else {
            0L
        }
    }, numeric(1))
    heights <- lapply(heights, convertHeight, "mm", valueOnly = TRUE)
    heights <- vapply(seq_len(nrow * TABLE_ROWS), function(i) {
        area_row <- (i - 1) %/% TABLE_ROWS + 1
        row_loc <- i %% TABLE_ROWS
        if (row_loc == 0) row_loc <- TABLE_ROWS
        area_side <- if (row_loc <= PANEL_COL) "t" else "b"
        idx <- .subset2(design, area_side) == area_row
        if (any(idx)) {
            max(vapply(heights[idx], `[[`, numeric(1), row_loc), 0L)
        } else {
            0L
        }
    }, numeric(1))
    list(widths = unit(widths, "mm"), heights = unit(heights, "mm"))
}

#' @return A list of grobs
#' @noRd
align_grobs <- function(gt, widths, heights, loc) {
    UseMethod("align_grobs")
}

#' @export
align_grobs.gtable <- function(gt, widths, heights, loc) {
    .subset2(gt, "grobs")
}

#' @export
align_grobs.full_patch <- function(gt, widths, heights, loc) {
    grobs <- .subset2(gt, "grobs")
    l <- (.subset2(loc, "l") - 1L) * TABLE_COLS
    l_widths <- widths[seq(l + 1L, l + PANEL_COL - 1L)]
    r <- (.subset2(loc, "r") - 1L) * TABLE_COLS
    r_widths <- widths[seq(r + PANEL_COL + 1L, r + TABLE_COLS)]
    t <- (.subset2(loc, "t") - 1L) * TABLE_ROWS
    t_heights <- heights[seq(t + 1L, t + PANEL_ROW - 1L)]
    b <- (.subset2(loc, "b") - 1L) * TABLE_ROWS
    b_heights <- heights[seq(b + PANEL_ROW + 1L, b + TABLE_ROWS)]
    # the second grob should be the original gtable of the
    # `gtable_alignpatches` or `align_free_align` objects
    # see function
    # can be `gtable_free_align`, `gtable_alignpatches` or
    # `gtable_free_borders` objects
    grobs[[2]] <- align_border_size(
        .subset2(grobs, 2L),
        t = t_heights,
        l = l_widths,
        b = b_heights,
        r = r_widths
    )
    grobs
}

#' @param gt A `gtable` object fill in the full_patch
#' @noRd
align_border_size <- function(gt, t = NULL, l = NULL, b = NULL, r = NULL) {
    UseMethod("align_border_size")
}

#' @export
align_border_size.grob <- function(gt, t = NULL, l = NULL, b = NULL, r = NULL) {
    gt
}

#' @export
align_border_size.gtable_alignpatches <- function(gt, t = NULL, l = NULL,
                                                  b = NULL, r = NULL) {
    if (is.null(t) && is.null(l) && is.null(b) && is.null(r)) {
        return(gt)
    }
    n_row <- nrow(gt)
    n_col <- ncol(gt)
    if (!is.null(t)) gt$heights[seq_along(t)] <- t
    if (!is.null(l)) gt$widths[seq_along(l)] <- l
    if (!is.null(b)) gt$heights[seq(n_row - length(b) + 1, n_row)] <- b
    if (!is.null(r)) gt$widths[seq(n_col - length(r) + 1, n_col)] <- r

    grobs <- .subset2(gt, "grobs")
    layout <- .subset2(gt, "layout")
    gt$grobs <- lapply(seq_along(grobs), function(i) {
        # For each nested plot, we reuse this function to set the border sizes
        # we only set the border sizes for plot in the border
        align_border_size(
            .subset2(grobs, i),
            t = if (.subset2(layout, "t")[i] == 1L) t else NULL,
            l = if (.subset2(layout, "l")[i] == 1L) l else NULL,
            b = if (.subset2(layout, "b")[i] == n_row) b else NULL,
            r = if (.subset2(layout, "r")[i] == n_col) r else NULL
        )
    })
    gt
}

#' @importFrom gtable gtable_width gtable_height
#' @importFrom grid viewport
#' @export
align_border_size.gtable_plot_just <- function(gt, t = NULL, l = NULL,
                                               b = NULL, r = NULL) {
    ans <- NextMethod() # call `gtable_alignpatches` method

    # we allow the justifactions by top, left, bottom, and right
    if (!is.null(just <- attr(ans, "just"))) {
        # if all plots have absolute sizes or we have set the viewport width or
        # height, we will make justification
        horizontal_just <- attr(ans, "horizontal_just")
        vertical_just <- attr(ans, "vertical_just")
        x <- y <- NULL
        if (!is.null(vp_width <- attr(ans, "vp_width")) || horizontal_just) {
            if (horizontal_just) {
                # all is absolute size, we always follow this gtable width
                # if we use the `vp_width`, and it is smaller than the actual
                # gtable width, we'll fail to make the justification
                vp_width <- gtable_width(ans)
            } else {
                # if some unit are null, we can just use the maximal size,
                # since the null unit will fill the remaining blank space
                vp_width <- max(vp_width, gtable_width(ans))
            }
            if (any(just == "left")) {
                x <- 0L
            } else if (any(just == "right")) {
                x <- 1L
            }
        }
        if (!is.null(vp_height <- attr(ans, "vp_height")) || vertical_just) {
            if (vertical_just) {
                vp_height <- gtable_height(ans)
            } else {
                vp_height <- max(vp_height, gtable_height(ans))
            }
            if (any(just == "top")) {
                y <- 1L
            } else if (any(just == "bottom")) {
                y <- 0L
            }
        }
        if (!is.null(x) || !is.null(y)) {
            ans$vp <- viewport(
                x = x %||% 0.5, y = y %||% 0.5,
                just = c(x %||% 0.5, y %||% 0.5),
                width = if (is.null(x)) 1L else vp_width,
                height = if (is.null(y)) 1L else vp_height,
            )
        }
    }
    ans
}

#' @export
align_border_size.gtable_free_align <- function(gt, t = NULL, l = NULL,
                                                b = NULL, r = NULL) {
    free_axes <- attr(gt, "free_axes")
    for (axis in free_axes) {
        assign(x = axis, value = NULL, envir = environment())
    }
    # can be `gtable_ggplot` or `gtable_alignpatches`
    ans <- NextMethod()
    attr(ans, "free_axes") <- free_axes
    if (!inherits(ans, "gtable_free_align")) {
        ans <- add_class(ans, "gtable_free_align")
    }
    ans
}

#' @export
align_border_size.gtable_ggplot <- function(gt, t = NULL, l = NULL,
                                            b = NULL, r = NULL) {
    if (!is.null(t)) gt$heights[seq_along(t)] <- t
    if (!is.null(l)) gt$widths[seq_along(l)] <- l
    if (!is.null(b)) gt$heights[seq(nrow(gt) - length(b) + 1, nrow(gt))] <- b
    if (!is.null(r)) gt$widths[seq(ncol(gt) - length(r) + 1, ncol(gt))] <- r
    gt
}
