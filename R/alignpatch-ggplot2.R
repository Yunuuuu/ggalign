#' @importFrom grid grid.draw
#' @importFrom rlang try_fetch cnd_signal
#' @export
print.patch_ggplot <- function(x, newpage = is.null(vp),
                               vp = NULL, ...) {
    ggplot2::set_last_plot(x)
    if (newpage) {
        grid::grid.newpage()
        if (is.character(vp)) {
            cli_abort(c(
                "{.arg vp} cannot be a character string when {.arg newpage} is TRUE.",
                i = "Please provide a viewport object or set {.arg newpage} to FALSE."
            ))
        }
    }
    if (!is.null(vp)) {
        if (is.character(vp)) {
            cur <- grid::current.viewport()$name
            grid::seekViewport(vp)
            if (!identical(cur, "ROOT")) on.exit(grid::seekViewport(cur))
        } else {
            grid::pushViewport(vp)
            on.exit(grid::upViewport())
        }
    }

    # render the plot
    try_fetch(
        grid.draw(x, ...),
        error = function(e) {
            if (inherits(e, "simpleError") &&
                deparse(conditionCall(e)[[1L]]) == "grid.Call") {
                error_name <- obj_type_friendly(x)
                if (Sys.getenv("RSTUDIO") == "1") {
                    cli_abort(c(paste(
                        "The RStudio {.field Plots} window may be",
                        "too small to show", error_name
                    ), i = "Please make the window larger."), parent = e)
                } else {
                    cli_abort(c(
                        "The viewport may be too small to show {error_name}.",
                        i = "Please make the window larger."
                    ), parent = e)
                }
            }
            cnd_signal(e)
        }
    )
    invisible(x)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.patch_ggplot <- function(x, recording = TRUE) {
    grid.draw(ggalignGrob(x), recording = recording)
}

##################################################
S7::method(ggalign_gtable, ggplot2::class_ggplot) <- function(x) {
    patch(x)$gtable(patch_options())
}

S7::method(ggalign_build, ggplot2::class_ggplot) <- function(x) x

#################################################
#' @importFrom ggplot2 ggproto
S7::method(patch, ggplot2::class_ggplot) <- function(x) {
    ggproto(NULL, PatchGgplot, plot = x)
}

# ggplot2 has following grobs:
#    panel
#    axis: must follow panel
#    strip: must follow the panel
#    xlab/ylab: can be aligned or follow the panel
#    subtitle
#    title
#    caption
#    guide: can be collected or kept
#' @importFrom ggplot2 ggplotGrob update_labels complete_theme
#' @include alignpatch-.R
PatchGgplot <- ggproto("PatchGgplot", Patch,
    gtable = function(self, options) {
        plot <- self$plot
        if (is.null(prop(options, "theme"))) {
            theme <- plot$theme
        } else {
            theme <- tag_theme(prop(options, "theme")) + plot$theme
        }

        # complete_theme() will ensure elements exist --------
        theme <- complete_theme(theme)
        # here: we remove tick length when the tick is blank
        theme <- setup_tick_length_element(theme)
        plot$theme <- theme

        # build the grob -------------------------------------
        ans <- ggplotGrob(plot)
        self$strip_pos <- find_strip_pos(ans, theme)
        # always add strips columns and/or rows
        ans <- add_strips(ans, self$strip_pos)
        ans <- setup_patch_title(ans, plot$ggalign_patch_title, theme = theme)
        if (!is.null(prop(options, "tagger"))) {
            ans <- prop(options, "tagger")$tag_table(ans, theme)
        }
        ans
    },
    border_sizes = function(self, gt = NULL, free = NULL) {
        out <- ggproto_parent(Patch, self)$border_sizes(gt, free)
        if (is.null(out) || self$strip_pos == "inside") {
            return(out)
        }
        if (!is.null(.subset2(out, "top"))) {
            out$top[length(out$top) - 1:0] <- out$top[length(out$top) - 0:1]
        }
        if (!is.null(.subset2(out, "left"))) {
            out$left[length(out$left) - 1:0] <- out$left[length(out$left) - 0:1]
        }
        if (!is.null(.subset2(out, "bottom"))) {
            out$bottom[1:2] <- out$bottom[2:1]
        }
        if (!is.null(.subset2(out, "right"))) {
            out$right[1:2] <- out$right[2:1]
        }
        out
    },
    align_border = function(self, gt, t = NULL, l = NULL, b = NULL, r = NULL) {
        if (self$strip_pos == "outside") {
            if (!is.null(t)) {
                t[length(t) - 1:0] <- t[length(t) - 0:1]
            }
            if (!is.null(l)) {
                l[length(l) - 1:0] <- l[length(l) - 0:1]
            }
            if (!is.null(b)) {
                b[1:2] <- b[2:1]
            }
            if (!is.null(r)) {
                r[1:2] <- r[2:1]
            }
        }
        ggproto_parent(Patch, self)$align_border(gt, t, l, b, r)
    }
)

#' @importFrom ggplot2 calc_element is_theme_element
#' @importFrom grid unit
setup_tick_length_element <- function(theme) {
    for (tick in c("x.top", "y.left", "x.bottom", "y.right")) {
        for (axis in c("axis.minor", "axis")) {
            blank <- is_theme_element(
                calc_element(paste(axis, "ticks", tick, sep = "."), theme),
                "blank"
            )
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
    layout <- .subset2(gt, "layout")
    if (!any(grepl("strip-b", layout$name))) { # No strips
        gt <- gtable_add_rows(
            gt, unit(0L, "mm"),
            panel_loc$b + switch(strip_pos,
                inside = 0L,
                outside = 1L
            )
        )
    } else if (strip_pos == "outside" && !any(layout$b == panel_loc$b + 2L)) {
        # Merge the strip-gap height into the axis and remove it. Only performed
        # if an axis exist
        gt$heights[panel_loc$b + 1L] <- sum(gt$heights[panel_loc$b + 1:2])
        gt <- gt[-(panel_loc$b + 2L), ]
    }
    if (!any(grepl("strip-t", layout$name))) {
        gt <- gtable_add_rows(
            gt, unit(0L, "mm"),
            panel_loc$t - switch(strip_pos,
                inside = 1L,
                outside = 2L
            )
        )
    } else if (strip_pos == "outside" && !any(layout$t == panel_loc$t - 2L)) {
        gt$heights[panel_loc$t - 1L] <- sum(gt$heights[panel_loc$t - 1:2])
        gt <- gt[-(panel_loc$t - 2L), ]
    }
    if (!any(grepl("strip-r", layout$name))) {
        gt <- gtable_add_cols(
            gt, unit(0L, "mm"),
            panel_loc$r + switch(strip_pos,
                inside = 0L,
                outside = 1L
            )
        )
    } else if (strip_pos == "outside" && !any(layout$r == panel_loc$r + 2L)) {
        gt$widths[panel_loc$r + 1L] <- sum(gt$widths[panel_loc$r + 1:2])
        gt <- gt[, -(panel_loc$r + 2L)]
    }
    if (!any(grepl("strip-l", layout$name))) {
        gt <- gtable_add_cols(
            gt, unit(0L, "mm"),
            panel_loc$l - switch(strip_pos,
                inside = 1L,
                outside = 2L
            )
        )
    } else if (strip_pos == "outside" && !any(layout$l == panel_loc$l - 2L)) {
        gt$widths[panel_loc$l - 1L] <- sum(gt$widths[panel_loc$l - 1:2])
        gt <- gt[, -(panel_loc$l - 2L)]
    }
    gt
}

#' @importFrom ggplot2 find_panel calc_element
find_strip_pos <- function(gt, theme) {
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
    calc_element("strip.placement", theme) %||% "inside"
}
