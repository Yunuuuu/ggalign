free_border <- function(plot, borders = c("t", "l", "b", "r")) {
    UseMethod("free_border")
}

#' @export
free_border.ggplot <- function(plot, borders = c("t", "l", "b", "r")) {
    if (length(borders) == 0L) {
        return(plot)
    }
    attr(plot, "free_borders") <- borders
    add_class(plot, "free_border")
}

#' @export
free_border.alignpatches <- free_border.ggplot

#' @export
free_border.free_align <- function(plot, borders = c("t", "l", "b", "r")) {
    borders <- setdiff(borders, attr(plot, "free_axes"))
    NextMethod()
}

#' @export
free_border.free_lab <- function(plot, borders = c("t", "l", "b", "r")) {
    attr(plot, "free_labs") <- setdiff(attr(plot, "free_labs"), borders)
    NextMethod()
}

#' @export
free_border.free_border <- function(plot, borders = c("t", "l", "b", "r")) {
    attr(plot, "free_borders") <- union(attr(plot, "free_borders"), borders)
    plot
}

#' @export
free_border.default <- function(plot, borders = c("t", "l", "b", "r")) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_border.align_wrapped <- free_border.default

################################################################
#' @export
patch_gtable.free_border <- function(patch) {
    gt <- NextMethod()
    attr(gt, "free_borders") <- attr(patch, "free_borders")
    add_class(gt, "gtable_free_border")
}

#' @export
patch_align.gtable_free_border <- function(gt, guides) {
    class(gt) <- setdiff(class(gt), "gtable_free_border")
    ans <- NextMethod()
    # here, we attach the borders into the panel
    make_free_border(ans, guides, borders = attr(gt, "free_borders"))
}

make_free_border <- function(gt, guides, borders) {
    UseMethod("make_free_border")
}

# For normal ggplot obbject ----------------------
#' @export
make_free_border.align_ggplot <- function(gt, guides, borders) {
    ans <- attach_border(gt, guides, borders)
    add_class(ans, "align_free_border")
}

# For `full_patch` obbject ----------------------
# including both alignpatches and free_align objects
#' @export
make_free_border.gtable_ggplot <- function(gt, guides, borders) {
    ans <- attach_border(gt, guides, borders)
    add_class(ans, "gtable_free_border")
}

#' @export
make_free_border.gtable_alignpatches <- function(gt, guides, borders) {
    ans <- attach_border(gt, guides, borders)
    add_class(ans, "gtable_free_border")
}

#' @export
make_free_border.full_patch <- function(gt, guides, borders) {
    gt$grobs[[2L]] <- make_free_border(
        .subset2(.subset2(gt, "grobs"), 2L),
        guides, borders
    )
    gt
}

attach_border <- function(gt, guides, borders) {
    added_class <- alignpatch_class(gt)
    class(gt) <- setdiff(class(gt), added_class)
    panel_pos <- find_panel(gt)
    layout <- .subset2(gt, "layout")
    nms <- .subset2(layout, "name")
    not_guides <- !grepl("^guide-(%s)", nms)
    n_row <- nrow(gt)
    n_col <- ncol(gt)
    for (border in borders) {
        panel_border <- .subset2(panel_pos, border)
        if (border == "t") {
            index <- .subset2(layout, "b") < panel_border
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            grob <- subset_gt(gt, index, trim = FALSE)
            grob$respect <- FALSE
            gt$grobs[index] <- rep_len(list(zeroGrob()), sum(index))
            grob <- gtable_trim_heights(grob)
            grob$vp <- viewport(
                y = 0L, just = "bottom",
                height = gtable_height(grob)
            )
            gt <- gtable_add_grob(gt,
                grobs = list(grob),
                t = 1L, l = 1L, b = panel_border - 1L, r = n_col,
                z = Inf, clip = "off",
                name = "top-border"
            )
        } else if (border == "l") {
            index <- .subset2(layout, "r") < panel_border
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            grob <- subset_gt(gt, index, trim = FALSE)
            grob$respect <- FALSE
            gt$grobs[index] <- rep_len(list(zeroGrob()), sum(index))
            grob <- gtable_trim_widths(grob)
            grob$vp <- viewport(
                x = 1L, just = "right",
                width = gtable_width(grob)
            )
            gt <- gtable_add_grob(gt,
                grobs = list(grob),
                t = 1L, l = 1L, b = n_row, r = panel_border - 1L,
                z = Inf, clip = "off",
                name = "left-border"
            )
        } else if (border == "b") {
            index <- .subset2(layout, "t") > panel_border
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            grob <- subset_gt(gt, index, trim = FALSE)
            grob$respect <- FALSE
            gt$grobs[index] <- rep_len(list(zeroGrob()), sum(index))
            grob <- gtable_trim_heights(grob)
            grob$vp <- viewport(
                y = 1L, just = "top",
                height = gtable_height(grob)
            )
            gt <- gtable_add_grob(gt,
                grobs = list(grob),
                t = panel_border + 1L, l = 1L, b = n_row, r = n_col,
                z = Inf, clip = "off",
                name = "bottom-border"
            )
        } else if (border == "r") {
            index <- .subset2(layout, "l") > panel_border
            # if we'll collect the guides, we shouldn't attach the guides
            if (any(border == guides)) {
                index <- not_guides & index
            }
            grob <- subset_gt(gt, index, trim = FALSE)
            grob$respect <- FALSE
            gt$grobs[index] <- rep_len(list(zeroGrob()), sum(index))
            grob <- gtable_trim_widths(grob)
            grob$vp <- viewport(
                x = 0L, just = "left",
                width = gtable_width(grob)
            )
            gt <- gtable_add_grob(gt,
                grobs = list(grob),
                t = 1L, l = panel_border + 1L, b = n_row, r = n_col,
                z = Inf, clip = "off",
                name = "right-border"
            )
        }
    }
    add_class(gt, added_class)
}
