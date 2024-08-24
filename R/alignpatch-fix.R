# Only used for ggplot object with fix aspect ratio
#' @return
#' A list with following elements
#'  - `gt`: the modified gtable object
#'  - `width`/`height`: the panel width and height.
#'  - `respect`: A boolean value indicates whether to fix this area.
#' @noRd
fix_area <- function(gt, panel_width, panel_height) {
    UseMethod("fix_area")
}

#' @export
fix_area.align_panel_nested <- function(gt, panel_width, panel_height) {
    list(
        gt = attach_into_panel(gt),
        width = panel_width, height = panel_height,
        respect = FALSE
    )
}

#' @export
fix_area.align_ggplot <- function(gt, panel_width, panel_height) {
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

# Should we remove the spaces?
#' @param gt A standard gtable object
#' @importFrom ggplot2 zeroGrob
#' @importFrom grid viewport unit convertWidth convertHeight
#' @importFrom gtable is.gtable gtable_add_grob
#' @noRd
attach_into_panel <- function(gt, borders = c("t", "l", "b", "r")) {
    # we remove the class and the subclass of "alignpatch" for added later
    # since the subset of gt shouldn't be a object "alignpatch"
    added_class <- alignpatch_class(gt)
    class(gt) <- setdiff(class(gt), added_class)

    layout <- .subset2(gt, "layout")
    nms <- .subset2(layout, "name")

    # there should be only one panel grob for a standard gtable object
    # we attach the elements into the panel grob
    panel_index <- which(startsWith(nms, "panel"))
    if (!is.gtable(panel <- .subset2(.subset2(gt, "grobs"), panel_index))) {
        panel <- subset_gt(gt, panel_index)
    }
    for (border in borders) {
        # strip; axis; (x|y)lab -(t|l|b|r)
        index <- grepl(sprintf("-%s(-|$)", border), nms) &
            switch(border,
                t = .subset2(layout, "b") < PANEL_ROW,
                l = .subset2(layout, "r") < PANEL_COL,
                b = .subset2(layout, "t") > PANEL_ROW,
                r = .subset2(layout, "l") > PANEL_COL
            )

        # Add strip, axes and labels to panel grob
        if (any(index)) {
            grob <- subset_gt(gt, index)
            gt$grobs[index] <- rep_len(list(zeroGrob()), sum(index))
            # Don't remove the spaces, otherwise, the axis will overlap
            # the plot nearby
            # if (border %in% c("t", "b")) {
            #     rows <- .subset(.subset2(layout, "t"), index)
            #     gt$heights[rows] <- unit(0, "mm")
            # } else {
            #     cols <- .subset(.subset2(layout, "l"), index)
            #     gt$widths[cols] <- unit(0, "mm")
            # }
            grob$vp <- switch(border,
                t = viewport(
                    y = 1, just = "bottom",
                    height = gtable_height(grob)
                ),
                l = viewport(
                    x = 0, just = "right",
                    width = gtable_width(grob)
                ),
                b = viewport(
                    y = 0, just = "top",
                    height = gtable_height(grob)
                ),
                r = viewport(
                    x = 1, just = "left",
                    width = gtable_width(grob)
                )
            )
            panel <- gtable_add_grob(panel,
                grobs = list(grob),
                t = 1, l = 1, b = nrow(panel), r = ncol(panel),
                z = Inf, clip = "off",
                name = paste(
                    switch(border,
                        t = "top",
                        l = "left",
                        b = "bottom",
                        r = "right"
                    ), "near-panel",
                    sep = "-"
                )
            )
        }
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
    add_class(gt, "align_attach", added_class)
}
