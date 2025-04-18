#' Returns a list of guide boxes collected from all plots.
#' Each element in the list corresponds to a specific position, containing a
#' sub-list of guide boxes, where each guide box represents a single plot.
#' @noRd
collect_guides_list <- function(guides_list) {
    ans <- lapply(c(.TLBR, "inside"), function(guide_pos) {
        guides <- lapply(guides_list, function(guides) {
            # IF no guide-box, a single `zeroGrob()` will be given
            # here, we regard each position is a `zeroGrob()`
            if (is_null_grob(guides)) return(list(guides)) # styler: off
            o <- .subset2(guides, guide_pos)
            # A guide-box should be a `zeroGrob()` or a `gtable` object
            if (maybe_guide_box(o)) {
                return(list(o))
            }
            # For other grobs, we just removed them silently
            if (is.grob(o)) {
                list(NULL)
            } else if (is.list(o)) {
                o[
                    vapply(o, maybe_guide_box,              # styler: off
                        logical(1L), USE.NAMES = FALSE      # styler: off
                    )
                ]
            } else {
                list(NULL)
            }
        })
        guides <- unlist(guides, FALSE, FALSE)
        guides <- guides[
            !vapply(guides, is.null, logical(1L), USE.NAMES = FALSE)
        ]
        if (is_empty(guides)) NULL else guides
    })
    names(ans) <- c(.TLBR, "inside")
    ans[!vapply(ans, is.null, logical(1L), USE.NAMES = FALSE)]
}

#' @param guides A list of guide-box
#' @importFrom ggplot2 zeroGrob
#' @importFrom gtable gtable gtable_add_grob
#' @noRd
assemble_guides <- function(guides, guide_pos, theme) {
    if (guide_pos == "inside") {
        # for `zeroGrob()`, it doesn't record the `viewport` information
        # used to identify the inside guide groups, we just removed them
        guides <- guides[
            !vapply(guides, is_null_grob, logical(1L), USE.NAMES = FALSE)
        ]
        if (is_empty(guides)) {
            guide_box <- zeroGrob()
        } else {
            positions <- justs <- vector("list", length(guides))
            for (i in seq_along(guides)) {
                guide <- .subset2(guides, i)
                # for inside guides, it may contain multiple guide-box
                is_box <- grepl("guide-box-inside", guide$layout$name)
                if (any(is_box)) {
                    guides[[i]] <- guide$grobs[is_box]
                } else {
                    guides[[i]] <- list(guide)
                }
                positions[[i]] <- lapply(guides[[i]], function(guide_box) {
                    unit.c(guide_box$vp$x, guide_box$vp$y)
                })
                justs[[i]] <- lapply(guides[[i]], function(guide_box) {
                    guide_box$vp$justification
                })
            }
            guides <- unlist(guides, FALSE, FALSE)
            groups <- data_frame0(
                positions = unlist(positions, FALSE, FALSE),
                justs = unlist(justs, FALSE, FALSE)
            )
            groups <- vec_group_loc(groups)
            index <- vec_seq_along(groups)

            # pakcage each group into a guide-box
            box_list <- vector("list", vec_size(index))
            for (i in index) {
                box_list[[i]] <- assemble_box(
                    guides[groups$loc[[i]]], guide_pos,
                    theme = theme + theme(
                        legend.position.inside = groups$key$positions[[i]],
                        legend.justification.inside = groups$key$justs[[i]]
                    )
                )
            }
            if (vec_size(box_list) > 1L) {
                guide_box <- gtable(unit(1L, "npc"), unit(1L, "npc"))
                guide_box <- gtable_add_grob(
                    guide_box, box_list,
                    t = 1L, l = 1L, clip = "off",
                    name = paste("guide-box-collected-inside", index, sep = "-")
                )
            } else {
                guide_box <- box_list[[1L]]
            }
        }
    } else {
        guide_box <- assemble_box(guides, guide_pos, theme = theme)
    }
    guide_box
}

#' @param guides A list of guide-box
#' @importFrom rlang try_fetch
#' @importFrom ggplot2 zeroGrob
#' @noRd
assemble_box <- function(guides, guide_pos, theme) {
    guides <- guides[
        !vapply(guides, is_null_grob, logical(1L), USE.NAMES = FALSE)
    ]
    if (is_empty(guides)) {
        zeroGrob()
    } else {
        # Remove the guide box background
        grobs <- lapply(guides, function(box) {
            box$grobs[grepl("guides", box$layout$name)]
        })
        grobs <- unlist(grobs, FALSE, FALSE)

        # remove duplicated guides
        grobs <- collapse_guides(grobs)
        if (is_empty(grobs)) return(zeroGrob()) # styler: off
        # for every position, collect all individual guides and arrange them
        # into a guide box which will be inserted into the main gtable
        package_box <- try_fetch(
            .subset2(ggfun("Guides"), "package_box"),
            error = function(cnd) package_box
        )
        package_box(grobs, guide_pos, theme)
    }
}

#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom ggplot2 calc_element
#' @importFrom grid valid.just editGrob viewport
package_box <- function(guides, guide_pos, theme) {
    theme <- complete_guide_theme(guide_pos, theme)
    guides <- guides_build(guides, theme)

    # Set the justification of the legend box
    # First value is xjust, second value is yjust
    just <- valid.just(calc_element("legend.justification", theme))
    xjust <- just[1L]
    yjust <- just[2L]
    guides <- editGrob(guides,
        vp = viewport(x = xjust, y = yjust, just = c(xjust, yjust))
    )
    guides <- gtable_add_rows(guides, unit(yjust, "null"))
    guides <- gtable_add_rows(guides, unit(1L - yjust, "null"), 0L)
    guides <- gtable_add_cols(guides, unit(xjust, "null"), 0L)
    guides <- gtable_add_cols(guides, unit(1L - xjust, "null"))
    guides
}

#' @importFrom gtable gtable_width gtable_height gtable gtable_add_grob
#' @importFrom grid editGrob heightDetails widthDetails valid.just unit.c unit
#' @importFrom ggplot2 margin element_grob element_blank calc_element element_render
guides_build <- function(guides, theme) {
    legend.spacing.y <- .subset2(theme, "legend.spacing.y")
    legend.spacing.x <- .subset2(theme, "legend.spacing.x")
    legend.box.margin <- calc_element("legend.box.margin", theme) %||%
        margin()
    widths <- do.call(`unit.c`, lapply(guides, gtable_width))
    heights <- do.call(`unit.c`, lapply(guides, gtable_height))

    just <- valid.just(.subset2(theme, "legend.box.just"))
    xjust <- just[1]
    yjust <- just[2]
    vert <- identical(.subset2(theme, "legend.box"), "horizontal")
    guides <- lapply(guides, function(g) {
        editGrob(g, vp = viewport(
            x = xjust, y = yjust, just = c(xjust, yjust),
            height = if (vert) heightDetails(g) else 1,
            width = if (!vert) widthDetails(g) else 1
        ))
    })
    guide_ind <- seq(by = 2, length.out = length(guides))
    sep_ind <- seq(2, by = 2, length.out = length(guides) - 1)
    if (vert) {
        heights <- max(heights)
        if (length(widths) != 1) {
            w <- unit(rep_len(0, length(widths) * 2 - 1), "mm")
            w[guide_ind] <- widths
            w[sep_ind] <- legend.spacing.x
            widths <- w
        }
    } else {
        widths <- max(widths)
        if (length(heights) != 1) {
            h <- unit(rep_len(0, length(heights) * 2 - 1), "mm")
            h[guide_ind] <- heights
            h[sep_ind] <- legend.spacing.y
            heights <- h
        }
    }
    widths <- unit.c(legend.box.margin[4], widths, legend.box.margin[2])
    heights <- unit.c(legend.box.margin[1], heights, legend.box.margin[3])
    guides <- gtable_add_grob(
        gtable(widths, heights, name = "guide-box"),
        guides,
        t = 1 + if (!vert) guide_ind else 1,
        l = 1 + if (vert) guide_ind else 1,
        name = "guides"
    )
    gtable_add_grob(
        guides,
        element_render(theme, "legend.box.background"),
        t = 1, l = 1, b = -1, r = -1,
        z = -Inf, clip = "off", name = "legend.box.background"
    )
}

#' @importFrom ggplot2 calc_element
complete_guide_theme <- function(guide_pos, theme) {
    if (guide_pos %in% c("top", "bottom")) {
        theme$legend.box <- calc_element("legend.box", theme) %||% "horizontal"
        theme$legend.direction <- calc_element("legend.direction", theme) %||%
            "horizontal"
        theme$legend.box.just <- calc_element("legend.box.just", theme) %||%
            c("center", "top")
    } else {
        theme$legend.box <- calc_element("legend.box", theme) %||% "vertical"
        theme$legend.direction <- calc_element("legend.direction", theme) %||%
            "vertical"
        theme$legend.box.just <- calc_element("legend.box.just", theme) %||%
            c("left", "top")
    }
    theme
}

################################################################ 3
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
    unit_elements <- vapply(x, is.unit, logical(1), USE.NAMES = FALSE)
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
    unit_elements <- vapply(x, is.unit, logical(1), USE.NAMES = FALSE)
    x[unit_elements] <- lapply(.subset(x, unit_elements), absolute.size)
    x
}
