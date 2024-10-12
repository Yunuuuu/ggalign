#' @export
ggalign_gtable.ggplot <- function(x) alignpatch(x)$patch_gtable()

#' @export
ggalign_build.ggplot <- function(x) x

##################################################
#' @export
print.patch_ggplot <- print.alignpatches

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.patch_ggplot <- grid.draw.alignpatches

#################################################
#' @importFrom ggplot2 ggproto
#' @export
alignpatch.ggplot <- function(x) ggproto(NULL, PatchGgplot, plot = x)

# ggplot2 has following grobs:
#    panel
#    axis: must follow panel
#    strip: must follow the panel
#    xlab/ylab: can be aligned or follow the panel
#    subtitle
#    title
#    caption
#    guide: can be collected or kept
#' @importFrom ggplot2 ggplotGrob update_labels
PatchGgplot <- ggproto("PatchGgplot", Patch,
    patch_gtable = function(self, guides, plot = self$plot) {
        # extract patch titles --------------------------------
        patch_titles <- .subset(
            .subset2(plot, "labels"),
            c("top", "left", "bottom", "right")
        )

        # we remove patch titles to avoid warning message for unknown labels
        plot <- update_labels(plot, list(
            top = NULL, left = NULL, bottom = NULL, right = NULL
        ))

        # complete_theme() will ensure elements exist --------
        theme <- complete_theme(.subset2(plot, "theme"))
        # here: we remove tick length when the tick is blank
        theme <- setup_tick_length_element(theme)
        plot$theme <- theme

        # build the grob -------------------------------------
        ans <- ggplotGrob(plot)
        strip_pos <- find_strip_pos(ans)
        # always add strips columns and/or rows
        ans <- add_strips(ans, strip_pos)
        # add guides columns and/or rows for ggplot2 < 3.5.0
        ans <- add_guides(ans)
        setup_patch_titles(ans, patch_titles = patch_titles, theme = theme)
    },
    respect = function(self, gt = self$gt) .subset2(gt, "respect"),

    #' @importFrom ggplot2 find_panel
    align_panel_sizes = function(self, panel_width, panel_height,
                                 gt = self$gt) {
        panel_pos <- find_panel(gt)
        rows <- c(.subset2(panel_pos, "t"), .subset2(panel_pos, "b"))
        cols <- c(.subset2(panel_pos, "l"), .subset2(panel_pos, "r"))
        respect <- .subset2(gt, "respect")
        if (rows[1L] == rows[2L] && cols[1L] == cols[2L]) {
            if (respect) {
                can_set_width <- is.na(as.numeric(panel_width))
                can_set_height <- is.na(as.numeric(panel_height))
                w <- .subset2(gt, "widths")[LEFT_BORDER + 1L]
                h <- .subset2(gt, "heights")[TOP_BORDER + 1L]
                if (can_set_width && can_set_height) {
                    panel_width <- w
                    panel_height <- h
                } else if (can_set_width) {
                    panel_width <- as.numeric(w) / as.numeric(h) * panel_height
                } else if (can_set_height) {
                    panel_height <- as.numeric(h) / as.numeric(w) * panel_width
                } else {
                    respect <- FALSE
                }
            }
        } else {
            respect <- FALSE
        }
        list(width = panel_width, height = panel_height, respect = respect)
    },

    #' @importFrom gtable gtable_add_grob gtable_height gtable_width
    #' @importFrom grid unit viewport
    #' @importFrom ggplot2 find_panel
    free_border = function(self, borders, gt = self$gt, guides = self$guides) {
        panel_pos <- find_panel(gt)
        for (border in borders) {
            layout <- .subset2(gt, "layout")
            not_guides <- !grepl("^guide-", .subset2(layout, "name"))
            if (border == "top") {
                index <- .subset2(layout, "b") < .subset2(panel_pos, "t") &
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
            } else if (border == "left") {
                index <- .subset2(layout, "r") < .subset2(panel_pos, "l") &
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
            } else if (border == "bottom") {
                index <- .subset2(layout, "t") > .subset2(panel_pos, "b") &
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
            } else if (border == "right") {
                index <- .subset2(layout, "l") > .subset2(panel_pos, "r") &
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
                name = paste("attach", border, "border", sep = "-")
            )
        }
        gt
    },

    #' @importFrom ggplot2 find_panel
    #' @importFrom gtable is.gtable gtable_height gtable_width gtable_add_grob
    #' @importFrom grid grobHeight grobWidth viewport
    free_lab = function(self, labs, gt = self$gt) {
        panel_pos <- find_panel(gt)
        for (lab in labs) {
            layout <- .subset2(gt, "layout")
            if (lab == "top") {
                panel_border <- .subset2(panel_pos, "t")
                index <- .subset2(layout, "b") < panel_border &
                    .subset2(layout, "t") >= (panel_border - 3L) &
                    .subset2(layout, "l") >= .subset2(panel_pos, "l") &
                    .subset2(layout, "r") <= .subset2(panel_pos, "r")
                if (!any(index)) next

                # this grob contain both axis labels and axis title
                grob <- subset_gt(gt, index)
                grob$vp <- viewport(
                    y = 1L, just = "bottom",
                    height = gtable_height(grob)
                )
            } else if (lab == "left") {
                panel_border <- .subset2(panel_pos, "l")
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
            } else if (lab == "bottom") {
                panel_border <- .subset2(panel_pos, "b")
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
            } else if (lab == "right") {
                panel_border <- .subset2(panel_pos, "r")
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
                    switch_position(lab, "xlab", "ylab"),
                    "axis", lab,
                    sep = "-"
                )
            )
        }
        gt
    }
)

#' @importFrom ggplot2 calc_element
#' @importFrom grid unit
setup_tick_length_element <- function(theme) {
    for (tick in c("x.top", "y.left", "x.bottom", "y.right")) {
        for (axis in c("axis.minor", "axis")) {
            blank <- inherits(calc_element(
                paste(axis, "ticks", tick, sep = "."), theme
            ), "element_blank")
            if (blank) { # No ticks, no length
                element <- paste(axis, "ticks.length", tick, sep = ".")
                theme[[element]] <- unit(0, "mm")
            }
        }
    }
    theme
}

#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom ggplot2 find_panel
#' @importFrom grid unit
add_strips <- function(gt, strip_pos) {
    panel_loc <- find_panel(gt)
    strip_pos <- switch(strip_pos, inside = 0L, outside = 2L) # styler: off
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
