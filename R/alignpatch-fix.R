# Only used for ggplot object with fix aspect ratio
#' @return
#' A list with following elements
#'  - `gt`: the modified gtable object
#'  - `width`/`height`: the panel width and height.
#'  - `respect`: A boolean value indicates whether to fix this area.
#' @noRd
fix_area <- function(gt, guides, panel_width, panel_height) {
    UseMethod("fix_area")
}

#' @export
fix_area.align_panel_nested <- function(gt, guides, panel_width, panel_height) {
    list(
        # the panel has been suspend, and border has been attached into panel
        gt = gt,
        width = panel_width, height = panel_height,
        respect = FALSE
    )
}

#' @importFrom gtable gtable_add_grob
#' @export
fix_area.align_ggplot <- function(gt, guides, panel_width, panel_height) {
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
    } else if (can_set_height) {
        panel_height <- as.numeric(h) / as.numeric(w) * panel_width
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
        gt <- attach_border(gt, guides)

        # we suspend the panel area to allow the fixed aspect ratio
        panel <- gt[PANEL_ROW, PANEL_COL]
        gt <- gt[-PANEL_ROW, -PANEL_COL]
        gt$respect <- FALSE
        gt <- gtable_add_rows(gt, unit(1L, "null"), PANEL_ROW - 1L)
        gt <- gtable_add_cols(gt, unit(1L, "null"), PANEL_COL - 1L)
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
    }
    list(gt = gt, width = panel_width, height = panel_height, respect = respect)
}
