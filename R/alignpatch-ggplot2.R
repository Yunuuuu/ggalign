# ggplot2 has following grobs:
#    panel
#    axis: must follow panel
#    strip: must follow the panel
#    xlab/ylab: can be aligned or follow the panel
#    subtitle
#    title
#    caption
#    guide: can be collected or kept
#' @importFrom ggplot2 ggplotGrob calc_element theme
#' @export
patch_gtable.ggplot <- function(patch, guides) {
    ans <- ggplotGrob(patch)
    ans <- add_strips(ans) # always add strips columns and/or rows
    ans <- add_guides(ans) # add guides columns and/or rows for ggplot2 < 3.5.0
    ans <- setup_patch_titles(ans, patch)
    add_class(ans, "gtable_ggplot")
}

#' @importFrom ggplot2 find_panel
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit convertWidth convertHeight
#' @export
patch_align.gtable_ggplot <- function(gt, guides, panel_width, panel_height) {
    panel_pos <- find_panel(gt)
    rows <- c(.subset2(panel_pos, "t"), .subset2(panel_pos, "b"))
    cols <- c(.subset2(panel_pos, "l"), .subset2(panel_pos, "r"))
    respect <- .subset2(gt, "respect")
    if (rows[1L] == rows[2L] && cols[1L] == cols[2L]) {
        if (respect) {
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
        }
    } else {
        # for ggplot with multiple panels, we cannot fix the aspect ratio
        # we always set respect to `FALSE`, and merge multiple panels into
        # one
        if (respect) {
            gt <- attach_border(gt, guides)
            respect <- FALSE
        }
        gt <- merge_panels(gt, rows, cols)
    }
    list(gt = gt, width = panel_width, height = panel_height, respect = respect)
}

#' @importFrom gtable is.gtable gtable_height gtable_width
#' @importFrom grid viewport unit convertWidth convertHeight
merge_panels <- function(gt, rows, cols) {
    p_rows <- seq(rows[1L], rows[2L])
    p_cols <- seq(cols[1L], cols[2L])
    panels <- gt[p_rows, p_cols]
    gt_new <- gt[-p_rows, -p_cols]
    # if (is.matrix(respect <- .subset2(gt, "respect"))) {
    #     # Not used currenly
    #     # if the respect is a matrix, the input should be an `alignpatches`
    #     # we only need respect the size of panel
    #     panels$respect <- respect[p_rows, p_cols]
    #     gt_new$respect <- FALSE
    #     gt$respect <- FALSE
    # }
    # if (.subset2(ans, "respect")) {
    #     panel_sizes <- list(width = panels$widths, heights = panels$heights)
    #     panel_sizes <- lapply(panel_sizes, function(size) {
    #         unit(sum(as.numeric(size[unitType(size) == "null"])), "null")
    #     })
    # }
    # add panel area
    gt_new <- gtable_add_rows(gt_new, unit(1L, "null"), rows[1L] - 1L)
    gt_new <- gtable_add_cols(gt_new, unit(1L, "null"), cols[1L] - 1L)

    p_cols <- seq(cols[1L], cols[2L])
    if (is_scalar(p_cols)) { # No multiple column panels
        # For elemnents in the top of the panel
        top <- which(gt$layout$l == p_cols &
            gt$layout$r == p_cols &
            gt$layout$b < rows[1])

        gt_new <- gtable_add_grob(
            gt_new,
            gt$grobs[top],
            gt$layout$t[top],
            p_cols,
            gt$layout$b[top],
            z = gt$layout$z[top],
            clip = gt$layout$clip[top],
            name = gt$layout$name[top]
        )

        # For elemnents in the bottom of the panel
        bottom <- which(gt$layout$l == p_cols &
            gt$layout$r == p_cols &
            gt$layout$t > rows[2L])
        b_mod <- rows[2L] - rows[1L]
        gt_new <- gtable_add_grob(
            gt_new,
            gt$grobs[bottom],
            gt$layout$t[bottom] - b_mod,
            p_cols,
            gt$layout$b[bottom] - b_mod,
            z = gt$layout$z[bottom],
            clip = gt$layout$clip[bottom],
            name = gt$layout$name[bottom]
        )
    } else {
        for (i in seq_len(nrow(gt))) {
            if (i >= rows[1L]) {
                if (i <= rows[2L]) next
                ii <- i - diff(rows)
            } else {
                ii <- i
            }
            table <- gt[i, p_cols]
            if (length(table$grobs) != 0L) {
                grobname <- paste(table$layout$name, collapse = ", ")
                gt_new <- gtable_add_grob(
                    gt_new, table, ii, cols[1L],
                    clip = "off",
                    name = grobname,
                    z = max(table$layout$z)
                )
            }
        }
    }

    p_rows <- seq(rows[1L], rows[2L])
    if (is_scalar(p_rows)) {
        left <- which(gt$layout$t == p_rows &
            gt$layout$b == p_rows &
            gt$layout$r < cols[1L])
        gt_new <- gtable_add_grob(
            gt_new, gt$grobs[left],
            p_rows, gt$layout$l[left],
            p_rows, gt$layout$r[left],
            z = gt$layout$z[left],
            clip = gt$layout$clip[left], name = gt$layout$name[left]
        )
        right <- which(gt$layout$t == p_rows &
            gt$layout$b == p_rows &
            gt$layout$l > cols[2L])
        r_mod <- cols[2L] - cols[1L]
        gt_new <- gtable_add_grob(
            gt_new, gt$grobs[right],
            p_rows, gt$layout$l[right] - r_mod,
            p_rows, gt$layout$r[right] - r_mod,
            z = gt$layout$z[right],
            clip = gt$layout$clip[right],
            name = gt$layout$name[right]
        )
    } else {
        for (i in seq_len(ncol(gt))) {
            if (i >= cols[1L]) {
                if (i <= cols[2L]) next
                ii <- i - diff(cols)
            } else {
                ii <- i
            }
            table <- gt[p_rows, i]
            if (length(table$grobs) != 0L) {
                grobname <- paste(table$layout$name, collapse = ", ")
                gt_new <- gtable_add_grob(
                    gt_new, table, rows[1], ii,
                    clip = "off",
                    name = grobname,
                    z = max(table$layout$z)
                )
            }
        }
    }
    panel_name <- paste0("panel-nested; ", paste(
        .subset2(.subset2(panels, "layout"), "name"),
        collapse = ", "
    ))
    gtable_add_grob(
        gt_new, panels, rows[1L], cols[1L],
        clip = "off", name = panel_name, z = 1L
    )
}

#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom ggplot2 find_panel
#' @importFrom grid unit
add_strips <- function(gt) {
    panel_loc <- find_panel(gt)
    strip_pos <- switch(find_strip_pos(gt),
        inside = 0L,
        outside = 2L
    )
    layout <- .subset2(gt, "layout")
    if (!any(grepl("strip-b", layout$name))) { # No strips
        gt <- gtable_add_rows(
            gt, unit(0L, "mm"),
            panel_loc$b + strip_pos
        )
    } else if (strip_pos == 2L && !any(layout$b == panel_loc$b + 2L)) {
        # Merge the strip-gap height into the axis and remove it. Only performed
        # if an axis exist
        gt$heights[panel_loc$b + 1L] <- sum(gt$heights[panel_loc$b + 1:2])
        gt <- gt[-(panel_loc$b + 2L), ]
    }
    if (!any(grepl("strip-t", layout$name))) {
        gt <- gtable_add_rows(
            gt, unit(0L, "mm"),
            panel_loc$t - 1L - strip_pos
        )
    } else if (strip_pos == 2L && !any(layout$t == panel_loc$t - 2L)) {
        gt$heights[panel_loc$t - 1L] <- sum(gt$heights[panel_loc$t - 1:2])
        gt <- gt[-(panel_loc$t - 2L), ]
    }
    if (!any(grepl("strip-r", layout$name))) {
        gt <- gtable_add_cols(
            gt, unit(0L, "mm"),
            panel_loc$r + strip_pos
        )
    } else if (strip_pos == 2L && !any(layout$r == panel_loc$r + 2L)) {
        gt$widths[panel_loc$r + 1L] <- sum(gt$widths[panel_loc$r + 1:2])
        gt <- gt[, -(panel_loc$r + 2L)]
    }
    if (!any(grepl("strip-l", layout$name))) {
        gt <- gtable_add_cols(
            gt, unit(0L, "mm"),
            panel_loc$l - 1L - strip_pos
        )
    } else if (strip_pos == 2L && !any(layout$l == panel_loc$l - 2L)) {
        gt$widths[panel_loc$l - 1L] <- sum(gt$widths[panel_loc$l - 1:2])
        gt <- gt[, -(panel_loc$l - 2L)]
    }
    gt
}

# theme(strip.placement)
#' @importFrom ggplot2 find_panel
find_strip_pos <- function(gt) {
    panel_loc <- find_panel(gt)
    layout <- .subset2(gt, "layout")
    nms <- .subset2(layout, "name")
    ind <- grep("strip-t", nms)
    if (length(ind) != 0L && panel_loc$t - min(layout$t[ind]) != 1L) {
        return("outside")
    }
    ind <- grep("strip-l", nms)
    if (length(ind) != 0L && panel_loc$l - min(layout$l[ind]) != 1L) {
        return("outside")
    }
    ind <- grep("strip-r", nms)
    if (length(ind) != 0L && max(layout$r[ind]) - panel_loc$r != 1L) {
        return("outside")
    }
    ind <- grep("strip-b", nms)
    if (length(ind) != 0L && max(layout$b[ind]) - panel_loc$b != 1L) {
        return("outside")
    }
    "inside"
}

#' @importFrom ggplot2 find_panel
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom grid unit
add_guides <- function(gt) {
    layout <- .subset2(gt, "layout")
    guide_ind <- grep("guide-box", layout$name)

    if (length(guide_ind) == 5L) { # for ggplot2 >= 3.5.0
        return(gt)
    }
    panel_loc <- find_panel(gt)[, c("t", "l", "b", "r"), drop = FALSE]
    guide_loc <- layout[guide_ind, c("t", "l", "b", "r"), drop = FALSE]
    guide_pos <- if (nrow(guide_loc) == 0) {
        "none"
    } else if (panel_loc$t == guide_loc$t) {
        if (panel_loc$l > guide_loc$l) {
            "left"
        } else if (panel_loc$r < guide_loc$r) {
            "right"
        } else {
            "inside"
        }
    } else if (panel_loc$t > guide_loc$t) {
        "top"
    } else {
        "bottom"
    }

    # https://www.tidyverse.org/blog/2024/02/ggplot2-3-5-0-legends/#spacing-and-margins
    # add `legend box spacing` and `legend box`
    if (guide_pos != "right") {
        gt <- gtable_add_cols(gt, unit(c(0L, 0L), "mm"), panel_loc$r + 3L)
    }
    if (guide_pos != "left") {
        gt <- gtable_add_cols(gt, unit(c(0L, 0L), "mm"), panel_loc$l - 4L)
    }
    if (guide_pos != "bottom") {
        gt <- gtable_add_rows(gt, unit(c(0L, 0L), "mm"), panel_loc$b + 5L)
    }
    if (guide_pos != "top") {
        gt <- gtable_add_rows(gt, unit(c(0L, 0L), "mm"), panel_loc$t - 4L)
    }
    gt
}
