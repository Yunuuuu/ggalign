#' Used to make justification to top, left, bottom, or right.
#' @param plot An alignpatches object.
#' @keywords internal
#' @noRd
free_just <- function(plot, just, width = NULL, height = NULL) {
    attr(plot, "just") <- just
    attr(plot, "vp_width") <- width
    attr(plot, "vp_height") <- height
    add_class(plot, "free_just")
}

####################################################
#' @export
patch_gtable.free_just <- function(patch, guides) {
    gt <- NextMethod()

    # we check if all panel widths/heights are absolute sizes --------
    panel_widths <- .subset2(gt, "widths")[
        seq(PANEL_COL, to = ncol(gt), by = TABLE_COLS)
    ]
    panel_heights <- .subset2(gt, "heights")[
        seq(PANEL_ROW, to = nrow(gt), by = TABLE_ROWS)
    ]

    # if all plots have absolute sizes or we have set the viewport width or
    # height, we set the viewport to make justification
    attr(gt, "just") <- attr(patch, "just")
    attr(gt, "horizontal_just") <- !any(is_null_unit(panel_widths))
    attr(gt, "vertical_just") <- !any(is_null_unit(panel_heights))
    attr(gt, "vp_width") <- attr(patch, "vp_width")
    attr(gt, "vp_height") <- attr(patch, "vp_height")
    add_class(gt, "gtable_free_just")
}
