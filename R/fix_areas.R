#' @importFrom grid is.unit unit
fix_areas <- function(gt_list, design, panel_widths, panel_heights) {
    if (!is.unit(panel_widths)) panel_widths <- unit(panel_widths, "null")
    if (!is.unit(panel_heights)) panel_heights <- unit(panel_heights, "null")

    # the plot to be fixed must in only one squre of the area
    # length relative to `gt_list`/`design`
    need_fix <- .subset2(design, "l") == .subset2(design, "r") &
        .subset2(design, "t") == .subset2(design, "b") &
        # all gtables should be an alignpatch object
        # Now, only gtable from ggplot2 will have a respect of `TRUE`
        vapply(gt_list, .subset2, logical(1L), "respect")

    # here we respect the aspect ratio when necessary -----
    # if the width or height is NA, we will guess the widths or heights
    # based on the fixed aspect ratio
    guess_widths <- which(is.na(as.numeric(panel_widths)))
    guess_heights <- which(is.na(as.numeric(panel_heights)))
    cols <- .subset2(design, "l")
    rows <- .subset2(design, "t")
    gt_index <- order(
        # we first set the widths for the fixed plot with heights set by user
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
    respect_index <- vector("list", sum(need_fix))

    # For plot cannot be fixed, we always attach strips, axes and labels into
    # the panel area
    for (i in intersect(gt_index, which(need_fix))) {
        cur_gt <- .subset2(gt_list, i)
        row <- .subset(rows, i)
        col <- .subset(cols, i)
        fixed_elements <- fix_area(
            cur_gt,
            panel_widths[col],
            panel_heights[row]
        )
        gt_list[[i]] <- .subset2(fixed_elements, "gt")
        panel_widths[col] <- .subset2(fixed_elements, "width")
        panel_heights[row] <- .subset2(fixed_elements, "height")
        if (.subset2(fixed_elements, "respect")) {
            respect_index[[i]] <- matrix(
                c(
                    (row - 1L) * TABLE_ROWS + PANEL_ROW,
                    (col - 1L) * TABLE_COLS + PANEL_COL
                ),
                nrow = 1L
            )
        }
    }

    # we set the widths/heights with no fixed plots to be 1 null
    if (any(guess_widths <- is.na(as.numeric(panel_widths)))) {
        panel_widths[guess_widths] <- unit(1L, "null")
    }
    if (any(guess_heights <- is.na(as.numeric(panel_heights)))) {
        panel_heights[guess_heights] <- unit(1L, "null")
    }

    if (!is.null(respect_index <- do.call(rbind, respect_index))) {
        respect <- matrix(
            0L, TABLE_ROWS * length(panel_heights),
            TABLE_COLS * length(panel_widths)
        )
        respect[respect_index] <- 1L
    } else {
        respect <- NULL
    }
    list(gt_list,
        respect = respect,
        panel_heights = panel_heights,
        panel_widths = panel_widths
    )
}

fix_area <- function(gt, panel_width, panel_height) {
    UseMethod("fix_area")
}

#' @export
fix_area.ggplot_nested_alignpatch <- function(gt, panel_width, panel_height) {
    list(
        gt = attach_into_panel(gt),
        width = panel_width, height = panel_height,
        respect = FALSE
    )
}

#' @export
fix_area.ggplot_alignpatch <- function(gt, panel_width, panel_height) {
    respect <- TRUE
    can_set_width <- is.na(as.numeric(panel_width))
    can_set_height <- is.na(as.numeric(panel_height))
    w <- .subset2(gt, "widths")[PANEL_COL]
    h <- .subset2(gt, "heights")[PANEL_ROW]
    if (can_set_width && can_set_height) {
        panel_width <- w
        panel_height <- h
    } else if (can_set_width) {
        panel_width <- as.numeric(w) / as.numeric(h) * panel_height
        # gt$widths[PANEL_COL] <- panel_widths[col]
        # gt$heights[PANEL_ROW] <- panel_heights[row]
    } else if (can_set_height) {
        panel_height <- as.numeric(h) / as.numeric(w) * panel_width
        # gt$widths[PANEL_COL] <- panel_widths[col]
        # gt$heights[PANEL_ROW] <- panel_heights[row]
    } else {
        # ratio <- as.numeric(w) / as.numeric(h)
        # actual_ratio <- panel_widths[col] / panel_heights[row]
        # the plot panel won't be aligned in this ways
        # # use dplyr::near to compare float number
        # if (abs(ratio - actual_ratio) < sqrt(.Machine$double.eps)) {
        #     will_be_fixed <- FALSE
        # }
        respect <- FALSE

        # should we remove the spaces of the axis and labs automatically?
        # if (ratio < actual_ratio) {
        #     gt$widths[
        #         PANEL_COL + c(-(1:2), 1:2)
        #     ] <- unit(0, "mm")
        # } else {
        #     gt$heights[
        #         PANEL_ROW + c(-(1:2), 1:2)
        #     ] <- unit(0, "mm")
        # }

        # attach strip, axes and labels into the panel area
        gt <- attach_into_panel(gt)
    }
    list(gt = gt, width = panel_width, height = panel_height, respect = respect)
}

#' @param gt A standard gtable object
#' @importFrom grid viewport unit convertWidth convertHeight
#' @importFrom gtable is.gtable gtable_add_grob
#' @noRd
attach_into_panel <- function(gt) {
    # we remove the class and the subclas of "alignpatch" for added later
    # since the subset of gt shouldn't be a object "alignpatch"
    added_class <- class(gt)
    added_class <- added_class[seq_len(which(added_class == "alignpatch"))]
    class(gt) <- setdiff(class(gt), added_class)

    layout <- .subset2(gt, "layout")
    nms <- .subset2(layout, "name")

    # there should be only one panel grob for a standard gtable object
    # we attach the elements into the panel grob
    panel_index <- which(startsWith(nms, "panel"))
    if (!is.gtable(panel <- .subset2(.subset2(gt, "grobs"), panel_index))) {
        panel <- subset_gt(gt, panel_index)
    }

    # strip; axis; (x|y)lab -(t|l|b|r)
    top <- grepl("-t(-|$)", nms) & .subset2(layout, "b") < PANEL_ROW
    left <- grepl("-l(-|$)", nms) & .subset2(layout, "r") < PANEL_COL
    bottom <- grepl("-b(-|$)", nms) & .subset2(layout, "t") > PANEL_ROW
    right <- grepl("-r(-|$)", nms) & .subset2(layout, "l") > PANEL_COL

    # Add strip, axes and labels to panel grob
    if (any(top)) {
        top_grob <- subset_gt(gt, top)
        gt$grobs[top] <- rep_len(list(zeroGrob()), sum(top))
        top_grob$vp <- viewport(
            y = 1, just = "bottom",
            height = gtable_height(top_grob)
        )
        panel <- gtable_add_grob(panel,
            grobs = list(top_grob),
            t = 1, l = 1, b = nrow(panel), r = ncol(panel),
            z = Inf, clip = "off", name = "top-near-panel"
        )
    }
    if (any(left)) {
        left_grob <- subset_gt(gt, left)
        gt$grobs[left] <- rep_len(list(zeroGrob()), sum(left))
        left_grob$vp <- viewport(
            x = 0, just = "right",
            width = gtable_width(left_grob)
        )
        panel <- gtable_add_grob(
            panel,
            grobs = list(left_grob),
            t = 1, l = 1, b = nrow(panel), r = ncol(panel),
            z = Inf, clip = "off", name = "left-near-panel"
        )
    }
    if (any(bottom)) {
        bottom_grob <- subset_gt(gt, bottom)
        gt$grobs[bottom] <- rep_len(list(zeroGrob()), sum(bottom))
        bottom_grob$vp <- viewport(
            y = 0, just = "top",
            height = gtable_height(bottom_grob)
        )
        panel <- gtable_add_grob(panel,
            grobs = list(bottom_grob),
            t = 1, l = 1, b = nrow(panel), r = ncol(panel),
            z = Inf, clip = "off", name = "bottom-near-panel"
        )
    }
    if (any(right)) {
        right_grob <- subset_gt(gt, right)
        gt$grobs[right] <- rep_len(list(zeroGrob()), sum(right))
        right_grob$vp <- viewport(
            x = 1, just = "left",
            width = gtable_width(right_grob)
        )
        panel <- gtable_add_grob(panel,
            grobs = list(right_grob),
            t = 1, l = 1, b = nrow(panel), r = ncol(panel),
            z = Inf, clip = "off", name = "right-near-panel"
        )
    }

    gt <- subset_gt(gt, -panel_index, trim = FALSE)
    gt <- gtable_add_grob(
        gt, list(panel),
        t = PANEL_ROW, l = PANEL_COL,
        clip = "off",
        name = paste0(
            .subset2(.subset2(panel, "layout"), "name"),
            collapse = ", "
        ),
        z = 1
    )
    add_class(gt, "attach_alignpatch", added_class)
}
