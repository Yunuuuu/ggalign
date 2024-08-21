# copied from patchwork
#' @importFrom ggplot2 find_panel zeroGrob
#' @importFrom gtable gtable gtable_add_grob
#' @export
patch_build.gtable_alignpatches <- function(gt) {
    panel_pos <- find_panel(gt)
    widths <- unit.c(
        gt$widths[seq_len(.subset2(panel_pos, "l") - 1)],
        unit(1, "null"), # panel width
        gt$widths[seq(.subset2(panel_pos, "r") + 1, ncol(gt))]
    )
    heights <- unit.c(
        gt$heights[seq_len(.subset2(panel_pos, "t") - 1)],
        unit(1, "null"), # panel height
        gt$heights[seq(.subset2(panel_pos, "b") + 1, nrow(gt))]
    )
    gt_new <- gtable(widths = widths, heights = heights)
    gt_new <- gtable_add_grob(
        gt_new, zeroGrob(),
        PANEL_ROW, PANEL_COL,
        name = "panel-area"
    )
    gt_new <- gtable_add_grob(
        gt_new, gt, 1, 1,
        nrow(gt_new), ncol(gt_new),
        clip = "off", name = "alignpatches-table"
    )
    add_class(gt_new, "gtable_alignpatch", "alignpatch")
}

#' @importFrom gtable gtable
#' @importFrom grid unit
#' @export
patch_gtable.alignpatches <- function(x) {
    plots <- .subset2(x, "plots")
    layout <- .subset2(x, "layout")
    layout <- utils::modifyList(
        default_layout,
        layout[!vapply(layout, is.null, logical(1))]
    )

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
    panel_widths <- rep(panel_widths, length.out = dims[2])
    panel_heights <- rep(panel_heights, length.out = dims[1])

    # filter gt based on the design areas --------------------
    design <- quickdf(design)
    if (nrow(design) < length(plots)) {
        cli::cli_warn("Too few patch areas to hold all plots. Dropping plots")
        plots <- plots[seq_len(nrow(design))]
    } else {
        design <- design[seq_along(plots), , drop = FALSE]
    }

    # prepare the output ----------------------------------
    gt_new <- gtable(
        unit(rep(0L, TABLE_COLS * dims[2L]), "null"),
        unit(rep(0L, TABLE_ROWS * dims[1L]), "null")
    )

    # we always build a standard gtable layout for the `patch`
    gt_list <- lapply(plots, patch_gtable) # return a list of gtables
    gt_list <- lapply(gt_list, patch_build) # standardize the gtable

    # collect guides  ---------------------------------------

    # setup the output gtable widths and heights ------------
    ## deal with gtable with fixed aspect ratio -------------
    # if it cannot be fixed and aligned, the strip, axis and labs will be
    # attached into panel
    fixed_elements <- fix_areas(gt_list, design, panel_widths, panel_heights)
    gt_list <- .subset2(fixed_elements, 1L)

    ## set sizes for each row/column ------------------------
    sizes <- table_sizes(
        lapply(gt_list, .subset2, "widths"),
        lapply(gt_list, .subset2, "heights"),
        design, dims[2], dims[1]
    )
    gt_new$grobs <- set_grob_sizes(
        gt_list,
        .subset2(sizes, "widths"),
        .subset2(sizes, "heights"),
        design
    )
    gt_new$widths <- .subset2(sizes, "widths")
    gt_new$heights <- .subset2(sizes, "heights")

    ## restore the panel sizes -----------------------------
    gt_new <- set_panel_sizes(gt_new,
        respect = .subset2(fixed_elements, "respect"),
        panel_heights = .subset2(fixed_elements, "panel_heights"),
        panel_widths = .subset2(fixed_elements, "panel_widths")
    )

    # setup the output layout --------------------------------
    max_z <- vapply(gt_list, function(x) {
        max(.subset2(.subset2(x, "layout"), "z"))
    }, numeric(1L))
    max_z <- c(0, cumsum(max_z))
    gt_new$layout <- rlang::exec(
        rbind,
        !!!lapply(seq_along(gt_list), function(i) {
            loc <- design[i, , drop = FALSE]
            lay <- .subset2(.subset2(gt_list, i), "layout")
            lay$z <- lay$z + ifelse(lay$name == "background", 0, max_z[i])
            # we only expand the panel area, here is the magic from patchwork
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
    gt_new <- gtable_add_grob(
        gt_new, zeroGrob(),
        t = PANEL_ROW,
        l = PANEL_COL,
        b = PANEL_ROW + TABLE_ROWS * (dims[1] - 1),
        r = PANEL_COL + TABLE_COLS * (dims[2] - 1),
        z = -1,
        name = "panel-area"
    )
    # if (layout$guides == "collect") {
    #     guide_grobs <- collapse_guides(guide_grobs)
    #     if (length(guide_grobs) != 0) {
    #         theme <- x$annotation$theme
    #         if (!attr(theme, "complete")) {
    #             theme <- theme_get() + theme
    #         }
    #         guide_grobs <- assemble_guides(guide_grobs, theme)
    #         gt_new <- attach_guides(gt_new, guide_grobs, theme)
    #     }
    # } else {
    #     gt_new$collected_guides <- guide_grobs
    # }
    # gt_new <- gtable_add_grob(
    #     gt_new, zeroGrob(),
    #     t = PANEL_ROW,
    #     l = PANEL_COL,
    #     b = PANEL_ROW + TABLE_ROWS * (dims[1] - 1),
    #     r = PANEL_COL + TABLE_COLS * (dims[2] - 1),
    #     z = -1,
    #     name = "panel-area"
    # )
    add_class(gt_new, "gtable_alignpatches")
}

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

set_panel_sizes <- function(gt, respect, panel_heights, panel_widths) {
    width_ind <- seq(PANEL_COL,
        by = TABLE_COLS,
        length.out = length(panel_widths)
    )
    height_ind <- seq(PANEL_ROW,
        by = TABLE_ROWS,
        length.out = length(panel_heights)
    )
    if (!is.null(respect)) gt$respect <- respect
    gt$widths[width_ind] <- panel_widths
    gt$heights[height_ind] <- panel_heights
    gt
}

set_grob_sizes <- function(gt_list, widths, heights, design) {
    unlist(lapply(seq_along(gt_list), function(i) {
        gt <- .subset2(gt_list, i)
        grobs <- .subset2(gt, "grobs")
        if (!inherits(gt, "gtable_alignpatch")) {
            return(grobs)
        }
        table_loc <- design[i, , drop = FALSE]
        l <- (table_loc$l - 1) * TABLE_COLS
        l_widths <- widths[seq(l + 1, l + PANEL_COL - 1)]
        r <- (table_loc$r - 1) * TABLE_COLS
        r_widths <- widths[seq(r + PANEL_COL + 1, r + TABLE_COLS)]
        t <- (table_loc$t - 1) * TABLE_ROWS
        t_heights <- heights[seq(t + 1, t + PANEL_ROW - 1)]
        b <- (table_loc$b - 1) * TABLE_ROWS
        b_heights <- heights[seq(b + PANEL_ROW + 1, b + TABLE_ROWS)]
        # the second grob should be the original gtable of the alignpatches
        # from `alignpatches_build` function named as `alignpatches-table`
        # see function patch_build.alignpatches()
        grobs[[2]] <- set_border_sizes(
            .subset2(grobs, 2L),
            l_widths, r_widths, t_heights, b_heights
        )
        grobs
    }), recursive = FALSE, use.names = FALSE)
}

#' @param gt A `gtable_alignpatches` object
#' @noRd
set_border_sizes <- function(gt, l = NULL, r = NULL, t = NULL, b = NULL) {
    if (is.null(l) && is.null(r) && is.null(t) && is.null(b)) {
        return(gt)
    }
    n_row <- nrow(gt)
    n_col <- ncol(gt)
    if (!is.null(l)) gt$widths[seq_along(l)] <- l
    if (!is.null(r)) gt$widths[seq(n_col - length(r) + 1, n_col)] <- r
    if (!is.null(t)) gt$heights[seq_along(t)] <- t
    if (!is.null(b)) gt$heights[seq(n_row - length(b) + 1, n_row)] <- b

    grobs <- .subset2(gt, "grobs")
    layout <- .subset2(gt, "layout")
    gt$grobs <- lapply(seq_along(grobs), function(i) {
        grob <- .subset2(grobs, i)
        if (!inherits(grob, "gtable_alignpatches")) {
            return(grob)
        }
        # For each nested plot, we reuse this function to set the border sizes
        # we only set the border sizes for plot in the border
        set_border_sizes(
            grob,
            if (.subset2(layout, "l")[i] == 1L) l else NULL,
            if (.subset2(layout, "r")[i] == n_col) r else NULL,
            if (.subset2(layout, "t")[i] == 1L) t else NULL,
            if (.subset2(layout, "b")[i] == n_row) b else NULL
        )
    })
    gt
}
