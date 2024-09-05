#' @return
#' A list with following elements
#'  - `gt`: the modified gtable object
#'  - `guides`: A list of guides object
#' @noRd
collect_guides <- function(gt, guides) {
    UseMethod("collect_guides")
}

#' @export
collect_guides.default <- function(gt, guides) {
    list(gt = gt, guides = list())
}

#' @export
collect_guides.gtable_alignpatches <- function(gt, guides) {
    collect_guides.gtable_ggplot(gt, guides)
}

#' @importFrom ggplot2 find_panel
#' @export
collect_guides.gtable_ggplot <- function(gt, guides) {
    layout <- .subset2(gt, "layout")
    grobs <- .subset2(gt, "grobs")
    guides_ind <- grep("guide-box", .subset2(layout, "name"))
    guides_loc <- layout[guides_ind, , drop = FALSE]
    collected_guides <- vector("list", length(guides))
    names(collected_guides) <- guides
    panel_pos <- find_panel(gt)
    remove_grobs <- NULL
    for (guide_pos in guides) {
        panel_border <- .subset2(panel_pos, guide_pos)
        guide_ind <- switch(guide_pos,
            t = .subset2(guides_loc, "b") < panel_border,
            l = .subset2(guides_loc, "r") < panel_border,
            b = .subset2(guides_loc, "t") > panel_border,
            r = .subset2(guides_loc, "l") > panel_border
        )
        if (!any(guide_ind)) next
        guide_loc <- guides_loc[guide_ind, , drop = FALSE]
        guide_ind <- .subset(guides_ind, guide_ind)
        remove_grobs <- c(guide_ind, remove_grobs)
        guide_box <- .subset2(grobs, guide_ind)
        collected_guides[[guide_pos]] <- .subset(
            .subset2(guide_box, "grobs"),
            grepl("guides", .subset2(.subset2(guide_box, "layout"), "name"))
        )

        # remove the guide from the original gtable
        space_pos <- switch(guide_pos,
            t = ,
            l = 1L,
            b = ,
            r = -1L
        )
        if (guide_pos %in% c("r", "l")) {
            gt$widths[c(guide_loc$l, guide_loc$l + space_pos)] <- unit(
                c(0L, 0L), "mm"
            )
        } else if (guide_pos %in% c("b", "t")) {
            gt$heights[c(guide_loc$t, guide_loc$t + space_pos)] <- unit(
                c(0L, 0L), "mm"
            )
        }
    }
    if (length(remove_grobs)) {
        gt <- subset_gt(gt, -remove_grobs, trim = FALSE)
    }
    list(gt = gt, guides = collected_guides)
}


#############################################################
#' @importFrom gtable gtable_width gtable_height
#' @importFrom grid unit.c unit
#' @importFrom ggplot2 calc_element
attach_guides <- function(table, guide_pos, guides, theme,
                          panel_pos = find_panel(table)) {
    guides <- assemble_guides(guides, guide_pos, theme = theme)
    spacing <- calc_element("legend.box.spacing", theme) %||% unit(0.2, "cm")
    legend_width <- gtable_width(guides)
    legend_height <- gtable_height(guides)
    if (guide_pos == "left") {
        table <- gtable_add_grob(table, guides,
            clip = "off",
            t = panel_pos$t,
            l = panel_pos$l - 5L,
            b = panel_pos$b,
            name = "guide-box-collected-left"
        )
        table <- align_border_size(table,
            l = unit.c(
                table$widths[seq_len(panel_pos$l - 6L)],
                legend_width, spacing
            )
        )
    } else if (guide_pos == "right") {
        table <- gtable_add_grob(table, guides,
            clip = "off", t = panel_pos$t,
            l = panel_pos$r + 5L, b = panel_pos$b,
            name = "guide-box-collected-right"
        )
        table <- align_border_size(table,
            r = unit.c(
                spacing, legend_width,
                table$widths[seq(panel_pos$r + 6L, ncol(table))]
            )
        )
    } else if (guide_pos == "bottom") {
        table <- gtable_add_grob(table, guides,
            clip = "off", t = panel_pos$b + 5L,
            l = panel_pos$l, r = panel_pos$r,
            name = "guide-box-collected-bottom"
        )
        table <- align_border_size(table,
            b = unit.c(
                spacing, legend_height,
                table$heights[seq(panel_pos$b + 6L, nrow(table))]
            )
        )
    } else if (guide_pos == "top") {
        table <- gtable_add_grob(table, guides,
            clip = "off", t = panel_pos$t - 5L,
            l = panel_pos$l, r = panel_pos$r,
            name = "guide-box-collected-top"
        )
        table <- align_border_size(table,
            t = unit.c(
                table$heights[seq_len(panel_pos$t - 6L)],
                legend_height, spacing
            )
        )
    }
    table
}

assemble_guides <- function(guides, guide_pos, theme) {
    # for every position, collect all individual guides and arrange them
    # into a guide box which will be inserted into the main gtable
    Guides <- utils::getFromNamespace("Guides", "ggplot2")
    Guides$package_box(guides, guide_pos, theme)
}

# Copied from patchwork
collapse_guides <- function(guides) {
    unnamed <- lapply(guides, unname_grob)
    for (i in rev(seq_along(unnamed)[-1])) {
        for (j in seq_len(i - 1)) {
            if (isTRUE(all.equal(unnamed[[i]], unnamed[[j]],
                check.names = FALSE, check.attributes = FALSE
            ))) {
                guides[i] <- NULL
                break
            }
        }
    }
    guides
}

#' @importFrom grid is.unit absolute.size
unname_vp <- function(x) {
    if (inherits(x, "vpTree")) {
        x$parent <- unname_vp(x$parent)
        x$children <- lapply(x$children, unname_vp)
    } else if (inherits(x, "viewport")) {
        x$name <- ""
        if (!is.null(x$layout$widths)) {
            x$layout$widths <- absolute.size(x$layout$widths)
        }
        if (!is.null(x$layout$heights)) {
            x$layout$heights <- absolute.size(x$layout$heights)
        }
    }
    unit_elements <- vapply(x, is.unit, logical(1))
    x[unit_elements] <- lapply(.subset(x, unit_elements), absolute.size)
    x
}

#' @importFrom grid is.grob is.unit absolute.size
#' @importFrom gtable is.gtable
unname_grob <- function(x) {
    if (is.gtable(x)) {
        x$name <- ""
        x$rownames <- NULL
        x$vp <- unname_vp(x$vp)
        names(x$grobs) <- NULL
        x$grobs <- lapply(x$grobs, unname_grob)
    } else if (is.grob(x)) {
        x$name <- ""
        x$vp <- unname_vp(x$vp)
        x$children <- unname(lapply(x$children, unname_grob))
        x$childrenOrder <- rep_len("", length(x$childrenOrder))
    }
    unit_elements <- vapply(x, is.unit, logical(1))
    x[unit_elements] <- lapply(.subset(x, unit_elements), absolute.size)
    x
}
