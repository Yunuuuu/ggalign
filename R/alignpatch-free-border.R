#' @param borders Which border shouldn't be aligned? Allowed values: `r rd_values(BORDERS)`.
#' @return 
#' - `free_border`: A modified version of `plot` with a `free_border` class.
#' @export
#' @rdname free
free_border <- function(plot, borders = c("t", "l", "b", "r")) {
    UseMethod("free_border")
}

#' @export
free_border.ggplot <- function(plot, borders = c("t", "l", "b", "r")) {
    attr(plot, "free_borders") <- check_borders(borders)
    add_class(plot, "free_border")
}

#' @export
free_border.alignpatches <- free_border.ggplot

#' @export
free_border.free_align <- function(plot, borders = c("t", "l", "b", "r")) {
    borders <- setdiff(borders, attr(plot, "free_axes"))
    if (length(borders) == 0L) {
        return(plot)
    }
    NextMethod()
}

#' @export
free_border.free_lab <- function(plot, borders = c("t", "l", "b", "r")) {
    borders <- check_borders(borders)
    free_labs <- setdiff(attr(plot, "free_labs"), borders)
    if (length(free_labs) == 0L) {
        class(plot) <- setdiff(class(plot), "free_lab")
    } else {
        attr(plot, "free_labs") <- free_labs
    }
    NextMethod()
}

#' @export
free_border.free_border <- function(plot, borders = c("t", "l", "b", "r")) {
    borders <- check_borders(borders)
    attr(plot, "free_borders") <- union(attr(plot, "free_borders"), borders)
    plot
}

#' @export
free_border.default <- function(plot, borders = c("t", "l", "b", "r")) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_border.wrapped_plot <- free_border.default

################################################################
#' @export
patch_gtable.free_border <- function(patch, guides) {
    class(patch) <- setdiff(class(patch), "free_border")
    gt <- NextMethod()
    attach_border(gt, guides, attr(patch, "free_borders"))
}

#' @importFrom gtable gtable_add_grob gtable_height gtable_width
#' @importFrom grid unit viewport
#' @importFrom ggplot2 find_panel
attach_border <- function(gt, guides, borders = BORDERS) {
    added_class <- alignpatch_class(gt)
    class(gt) <- setdiff(class(gt), added_class)
    panel_pos <- find_panel(gt)
    for (border in borders) {
        layout <- .subset2(gt, "layout")
        not_guides <- !grepl("^guide-", .subset2(layout, "name"))
        panel_border <- .subset2(panel_pos, border)
        if (border == "t") {
            index <- .subset2(layout, "b") < panel_border &
                .subset2(layout, "l") >= .subset2(panel_pos, "l") &
                .subset2(layout, "r") <= .subset2(panel_pos, "r")

            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            if (!any(index)) next
            grob <- subset_gt(gt, index)
            grob$respect <- FALSE
            grob$vp <- viewport(
                y = 1L, just = "bottom",
                height = gtable_height(grob)
            )
        } else if (border == "l") {
            index <- .subset2(layout, "r") < panel_border &
                .subset2(layout, "t") >= .subset2(panel_pos, "t") &
                .subset2(layout, "b") <= .subset2(panel_pos, "b")
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            if (!any(index)) next
            grob <- subset_gt(gt, index)
            grob$respect <- FALSE
            grob$vp <- viewport(
                x = 0L, just = "right",
                width = gtable_width(grob)
            )
        } else if (border == "b") {
            index <- .subset2(layout, "t") > panel_border &
                .subset2(layout, "l") >= .subset2(panel_pos, "l") &
                .subset2(layout, "r") <= .subset2(panel_pos, "r")
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            if (!any(index)) next
            grob <- subset_gt(gt, index)
            grob$respect <- FALSE
            grob$vp <- viewport(
                y = 0L, just = "top",
                height = gtable_height(grob)
            )
        } else if (border == "r") {
            index <- .subset2(layout, "l") > panel_border &
                .subset2(layout, "t") >= .subset2(panel_pos, "t") &
                .subset2(layout, "b") <= .subset2(panel_pos, "b")
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            if (!any(index)) next
            grob <- subset_gt(gt, index)
            grob$respect <- FALSE
            grob$vp <- viewport(
                x = 1L, just = "left",
                width = gtable_width(grob)
            )
        }
        gt <- subset_gt(gt, !index, trim = FALSE)
        gt <- gtable_add_grob(gt,
            grobs = list(grob),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = Inf, clip = "off",
            name = paste(
                "attach",
                switch(border,
                    t = "top",
                    l = "left",
                    b = "bottom",
                    r = "right"
                ),
                "border",
                sep = "-"
            )
        )
    }
    add_class(gt, added_class)
}
