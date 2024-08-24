free_lab <- function(plot, labs = c("t", "l", "b", "r")) {
    UseMethod("free_lab")
}

#' @export
free_lab.ggplot <- function(plot, labs = c("t", "l", "b", "r")) {
    if (length(labs) == 0L) {
        return(plot)
    }
    attr(plot, "free_labs") <- labs
    add_class(plot, "free_lab")
}

#' @export
free_lab.alignpatches <- free_lab.ggplot

#' @export
free_lab.free_align <- function(plot, labs = c("t", "l", "b", "r")) {
    labs <- setdiff(labs, attr(plot, "free_axes"))
    NextMethod()
}

#' @export
free_lab.free_borders <- function(plot, labs = c("t", "l", "b", "r")) {
    labs <- setdiff(labs, attr(plot, "free_borders"))
    NextMethod()
}

#' @export
free_lab.default <- function(plot, labs = c("t", "l", "b", "r")) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_lab.align_wrapped <- free_lab.default

####################################################
#' @export
patch_gtable.free_lab <- function(patch) {
    class(patch) <- setdiff(class(patch), "free_lab")
    gt <- NextMethod()
    attach_lab(gt, free_labs = attr(patch, "free_labs"))
}

#' @importFrom gtable is.gtable gtable_height gtable_width gtable_add_grob
#' @importFrom grid grobHeight grobWidth viewport
attach_lab <- function(gt, free_labs) {
    added_class <- alignpatch_class(gt)
    class(gt) <- setdiff(class(gt), added_class)
    panel_pos <- find_panel(gt)
    for (free_lab in free_labs) {
        layout <- .subset2(gt, "layout")
        panel_border <- .subset2(panel_pos, free_lab)
        if (free_lab == "t") {
            index <- .subset2(layout, "t") >= (panel_border - 3L) &
                .subset2(layout, "b") < panel_border &
                .subset2(layout, "l") >= .subset2(panel_pos, "l") &
                .subset2(layout, "r") <= .subset2(panel_pos, "r")
            if (!any(index)) next

            # this grob contain both axis labels and axis title
            grob <- subset_gt(gt, index)
            grob$vp <- viewport(
                y = 1L, just = "bottom",
                height = gtable_height(grob)
            )
        } else if (free_lab == "l") {
            index <- .subset2(layout, "r") < panel_border &
                .subset2(layout, "l") >= (panel_border - 3L) &
                .subset2(layout, "t") >= .subset2(panel_pos, "t") &
                .subset2(layout, "b") <= .subset2(panel_pos, "b")
            if (!any(index)) next

            # this grob contain both axis labels and axis title
            grob <- subset_gt(gt, index)
            grob$vp <- viewport(
                x = 0L, just = "right",
                width = gtable_width(grob)
            )
        } else if (free_lab == "b") {
            index <- .subset2(layout, "t") > panel_border &
                .subset2(layout, "b") <= (panel_border + 3L) &
                .subset2(layout, "l") >= .subset2(panel_pos, "l") &
                .subset2(layout, "r") <= .subset2(panel_pos, "r")
            if (!any(index)) next

            # this grob contain both axis labels and axis title
            grob <- subset_gt(gt, index)
            grob$vp <- viewport(
                y = 0L, just = "top",
                height = gtable_height(grob)
            )
        } else if (free_lab == "r") {
            index <- .subset2(layout, "l") > panel_border &
                .subset2(layout, "r") <= (panel_border + 3L) &
                .subset2(layout, "t") >= .subset2(panel_pos, "t") &
                .subset2(layout, "b") <= .subset2(panel_pos, "b")
            if (!any(index)) next

            # this grob contain both axis labels and axis title
            grob <- subset_gt(gt, index)
            grob$vp <- viewport(
                x = 1L, just = "left",
                width = gtable_width(grob)
            )
        }
        grob$respect <- FALSE
        gt <- subset_gt(gt, !index, trim = FALSE)
        gt <- gtable_add_grob(gt,
            grobs = list(grob),
            t = .subset2(panel_pos, "t"),
            l = .subset2(panel_pos, "l"),
            b = .subset2(panel_pos, "b"),
            r = .subset2(panel_pos, "r"),
            z = Inf, clip = "off",
            name = paste(
                switch(free_lab,
                    t = ,
                    b = "xlab",
                    l = ,
                    r = "ylab"
                ),
                "axis", free_lab,
                sep = "-"
            )
        )
    }
    add_class(gt, added_class)
}
