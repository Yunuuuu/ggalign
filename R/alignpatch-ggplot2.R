#' @export
ggalign_gtable.ggplot <- function(x) alignpatch(x)$patch_gtable()

#' @export
ggalign_build.ggplot <- function(x) x

##################################################
#' @export
#' @include alignpatch-build.R
print.patch_ggplot <- `print.ggalign::AlignPatches`

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.patch_ggplot <- `grid.draw.ggalign::AlignPatches`

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
#' @include alignpatch-.R
PatchGgplot <- ggproto("PatchGgplot", Patch,
    set_guides = function(guides) guides,
    patch_gtable = function(self, theme, guides, plot = self$plot) {
        # extract patch titles --------------------------------
        patch_titles <- plot$ggalign_patch_labels

        # complete_theme() will ensure elements exist --------
        theme <- complete_theme(plot$theme)
        # here: we remove tick length when the tick is blank
        theme <- setup_tick_length_element(theme)
        plot$theme <- theme

        # build the grob -------------------------------------
        ans <- ggplotGrob(plot)
        strip_pos <- find_strip_pos(ans)
        # always add strips columns and/or rows
        ans <- add_strips(ans, strip_pos)
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
    free_border = function(self, borders, gt = self$gt) {
        panel_pos <- find_panel(gt)
        for (border in borders) {
            if (border == "top") {
                gt <- liberate_area(
                    gt,
                    1L,
                    1L,
                    .subset2(panel_pos, "t") - 1L,
                    ncol(gt),
                    clip = "off",
                    name = "free-border-top",
                    vp = ~ viewport(
                        y = 0L, just = "bottom",
                        height = gtable_height(.x)
                    )
                )
            } else if (border == "left") {
                gt <- liberate_area(
                    gt,
                    1L,
                    1L,
                    nrow(gt),
                    .subset2(panel_pos, "l") - 1L,
                    clip = "off",
                    name = "free-border-left",
                    vp = ~ viewport(
                        x = 1L, just = "right",
                        width = gtable_width(.x)
                    )
                )
            } else if (border == "bottom") {
                gt <- liberate_area(
                    gt,
                    .subset2(panel_pos, "b") + 1L,
                    1L,
                    nrow(gt),
                    ncol(gt),
                    clip = "off",
                    name = "free-border-bottom",
                    vp = ~ viewport(
                        y = 1L, just = "top",
                        height = gtable_height(.x)
                    )
                )
            } else if (border == "right") {
                gt <- liberate_area(
                    gt,
                    1L,
                    .subset2(panel_pos, "r") + 1L,
                    nrow(gt),
                    ncol(gt),
                    clip = "off",
                    name = "free-border-right",
                    vp = ~ viewport(
                        x = 0L, just = "left",
                        width = gtable_width(.x)
                    )
                )
            }
        }
        gt
    },
    align_free_border = function(self, borders,
                                 t = NULL, l = NULL, b = NULL, r = NULL,
                                 gt = self$gt) {
        if (is.null(t) && is.null(l) && is.null(b) && is.null(r)) {
            return(gt)
        }
        # For free borders, we also align the margins
        for (border in borders) {
            i <- .subset2(.subset2(gt, "layout"), "name") ==
                sprintf("free-border-%s", border)
            if (any(i)) {
                i <- which(i)
                gt$grobs[[i]] <- switch_position(
                    border,
                    Patch$align_border(l = l, r = r, gt = gt$grobs[[i]]),
                    Patch$align_border(t = t, b = b, gt = gt$grobs[[i]])
                )
            }
        }
        gt
    },

    #' @importFrom ggplot2 find_panel
    #' @importFrom gtable is.gtable gtable_height gtable_width gtable_add_grob
    #' @importFrom grid grobHeight grobWidth viewport
    free_lab = function(self, labs, gt = self$gt) {
        panel_pos <- find_panel(gt)
        for (lab in labs) {
            name <- paste(
                switch_position(lab, "xlab", "ylab"),
                "axis", lab,
                sep = "-"
            )
            if (lab == "top") {
                panel_border <- .subset2(panel_pos, "t")
                gt <- liberate_area(
                    gt,
                    panel_border - 3L,
                    .subset2(panel_pos, "l"),
                    panel_border - 1L,
                    .subset2(panel_pos, "r"),
                    name = name,
                    vp = ~ viewport(
                        y = 0L, just = "bottom",
                        height = gtable_height(.x)
                    )
                )
            } else if (lab == "left") {
                panel_border <- .subset2(panel_pos, "l")
                gt <- liberate_area(
                    gt,
                    .subset2(panel_pos, "t"),
                    panel_border - 3L,
                    .subset2(panel_pos, "b"),
                    panel_border - 1L,
                    name = name,
                    vp = ~ viewport(
                        x = 1L, just = "right",
                        width = gtable_width(.x)
                    )
                )
            } else if (lab == "bottom") {
                panel_border <- .subset2(panel_pos, "b")
                gt <- liberate_area(
                    gt,
                    panel_border + 1L,
                    .subset2(panel_pos, "l"),
                    panel_border + 3L,
                    .subset2(panel_pos, "r"),
                    name = name,
                    vp = ~ viewport(
                        y = 1L, just = "top",
                        height = gtable_height(.x)
                    )
                )
            } else if (lab == "right") {
                panel_border <- .subset2(panel_pos, "r")
                gt <- liberate_area(
                    gt,
                    .subset2(panel_pos, "t"),
                    panel_border + 1L,
                    .subset2(panel_pos, "b"),
                    panel_border + 3L,
                    name = name,
                    vp = ~ viewport(
                        x = 0L, just = "left",
                        width = gtable_width(.x)
                    )
                )
            }
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
