#' Add Patch Titles to Plot Borders
#'
#' This function extends ggplot2's title functionality, allowing you to add
#' titles to each border of the plot: top, left, bottom, and right.
#'
#' @details
#' In addition to the main titles, you can use [labs()][ggplot2::labs] to
#' specify titles for the top, left, bottom, and right borders of the plot.
#'
#' The appearance and alignment of these patch titles can be customized using
#' [theme()][ggplot2::theme]:
#' - `plot.patch_title_*`: Controls the text appearance of patch titles.
#'   Inherits from `plot.title` by default.
#' - `plot.patch_title_*.position`: Determines the alignment of the patch
#'   titles. The default value `"panel"` aligns the titles with the plot panels.
#'   Setting this to `"plot"` aligns the titles with the entire plot (excluding
#'   margins and plot tags).
#'
#' @param top,left,bottom,right A string specifying the title to be added to the
#' top, left, bottom, and right border of the plot.
#' @export
#' @importFrom ggplot2 waiver
patch_titles <- function(top = waiver(), left = waiver(), bottom = waiver(),
                         right = waiver()) {
    ggplot2::labs(top = top, left = left, bottom = bottom, right = right)
}

#' @importFrom ggplot2 find_panel zeroGrob calc_element element_grob
#' @importFrom rlang arg_match0
#' @importFrom grid grobName
setup_patch_titles <- function(table, patch) {
    patch_titles <- .subset(
        .subset2(patch, "labels"),
        c("top", "left", "bottom", "right")
    )
    theme <- complete_theme(.subset2(patch, "theme"))
    plot_title <- calc_element("plot.title", theme)
    for (border in c("top", "left", "bottom", "right")) {
        panel_pos <- find_panel(table)
        patch_title <- .subset2(patch_titles, border)
        name <- paste0("plot.patch_title_", border)
        if (is.null(patch_title)) {
            title <- zeroGrob()
        } else {
            # set the default angle
            plot_title$angle <- switch(border,
                top = 0,
                left = 90,
                bottom = 0,
                right = -90
            )
            # always justification by center
            plot_title$hjust <- 0.5
            el <- calc_element(name, theme) %||% plot_title
            title <- element_grob(el, patch_title,
                margin_y = TRUE, margin_x = TRUE
            )
            title$name <- grobName(title, name)
        }
        name <- paste(name, "position", sep = ".")
        pos <- arg_match0(
            .subset2(theme, name) %||% "panel",
            c("panel", "plot"),
            arg_nm = name,
            error_call = quote(theme())
        )
        if (border == "top") {
            height <- grobHeight(title)
            if (pos == "panel") {
                l <- min(panel_pos$l)
                r <- max(panel_pos$r)
            } else {
                l <- 1L
                r <- ncol(table)
            }
            h <- .subset2(panel_pos, "t") - 8L # above original title
            table <- gtable_add_rows(table, height, pos = h)
            table <- gtable_add_grob(table, title,
                name = "patch-title-top",
                t = h + 1L, b = h + 1L, l = l, r = r,
                clip = "off"
            )
        } else if (border == "left") {
            width <- grobWidth(title)
            if (pos == "panel") {
                t <- min(panel_pos$t)
                b <- max(panel_pos$b)
            } else {
                t <- 1L
                b <- ncol(table)
            }
            v <- .subset2(panel_pos, "l") - 6L # left of the guide
            table <- gtable_add_cols(table, width, pos = v)
            table <- gtable_add_grob(table, title,
                name = "patch-title-left",
                t = t, b = b, l = v + 1L, r = v + 1L,
                clip = "off"
            )
        } else if (border == "bottom") {
            height <- grobHeight(title)
            if (pos == "panel") {
                l <- min(panel_pos$l)
                r <- max(panel_pos$r)
            } else {
                l <- 1L
                r <- ncol(table)
            }
            h <- .subset2(panel_pos, "b") + 6L # below caption
            table <- gtable_add_rows(table, height, pos = h)
            table <- gtable_add_grob(table, title,
                name = "patch-title-bottom",
                t = h + 1L, b = h + 1L, l = l, r = r,
                clip = "off"
            )
        } else if (border == "right") {
            width <- grobWidth(title)
            if (pos == "panel") {
                t <- min(panel_pos$t)
                b <- max(panel_pos$b)
            } else {
                t <- 1L
                b <- ncol(table)
            }
            v <- .subset2(panel_pos, "r") + 5L # right of the guide
            table <- gtable_add_cols(table, width, pos = v)
            table <- gtable_add_grob(table, title,
                name = "patch-title-right",
                t = t, b = b, l = v + 1L, r = v + 1L,
                clip = "off"
            )
        }
    }
    table
}
