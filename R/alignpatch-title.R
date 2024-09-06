#' Add patch titles to plot borders
#'
#' This function extends ggplot2's title functionality, allowing you to add
#' titles to each border of the plot: top, left, bottom, and right.
#'
#' @details
#' You can also use [labs()][ggplot2::labs] to specify titles for the top, left,
#' bottom, and right borders of the plot.
#'
#' The appearance and alignment of these patch titles can be customized using
#' [theme()][ggplot2::theme]:
#' - `plot.patch_title`/`plot.patch_title.*`: Controls the text appearance of
#'   patch titles. By default, `plot.patch_title` inherit from `plot.title`, and
#'   settings for each border will inherit from `plot.patch_title`, with the
#'   exception of the `angle` property, which is not inherited.
#' - `plot.patch_title.position`/`plot.patch_title.position.*`: Determines the
#'   alignment of the patch titles. By default, `plot.patch_title.position`
#'   inherit from `plot.title.position`, and settings for each border will
#'   inherit from `plot.patch_title`. The value `"panel"` aligns the patch
#'   titles with the plot panels. Setting this to `"plot"` aligns the patch
#'   title with the entire plot (excluding margins and plot tags).
#'
#' @param top,left,bottom,right A string specifying the title to be added to the
#' top, left, bottom, and right border of the plot.
#' @export
#' @importFrom ggplot2 waiver
patch_titles <- function(top = waiver(), left = waiver(), bottom = waiver(),
                         right = waiver()) {
    ggplot2::labs(top = top, left = left, bottom = bottom, right = right)
}

#' @importFrom ggplot2 find_panel calc_element zeroGrob element_grob merge_element
#' @importFrom rlang arg_match0
#' @importFrom grid grobName
setup_patch_titles <- function(table, patch) {
    patch_titles <- .subset(
        .subset2(patch, "labels"),
        c("top", "left", "bottom", "right")
    )
    # complete_theme() will ensure `plot_title` exists
    theme <- .subset2(patch, "theme")
    old_text <- calc_element("plot.title", theme)
    # always justification by center for patch title
    old_text$hjust <- 0.5
    if (is.null(text <- .subset2(theme, "plot.patch_title"))) {
        text <- old_text
    } else if (inherits(text, "element_text")) {
        text <- merge_element(text, old_text)
    } else {
        cli::cli_abort(paste(
            "Theme element {.var plot.patch_title} must have",
            "class {.cls element_text}."
        ), call = quote(theme()))
    }
    position <- .subset2(theme, "plot.patch_title.position") %||%
        .subset2(theme, "plot.title.position") %||% "panel"
    for (border in c("top", "left", "bottom", "right")) {
        panel_pos <- find_panel(table)
        patch_title <- .subset2(patch_titles, border)
        name <- paste("plot.patch_title", border, sep = ".")
        if (is.null(patch_title)) {
            title <- zeroGrob()
        } else {
            # set the default angle
            text$angle <- switch(border,
                top = 0L,
                left = 90L,
                bottom = 0L,
                right = -90L
            )
            # we merge the element with `plot.patch_title`
            if (is.null(el <- .subset2(theme, name))) {
                el <- text
            } else if (inherits(el, "element_text")) {
                el <- merge_element(el, text)
            } else {
                cli::cli_abort(paste(
                    "Theme element {.var {name}} must have",
                    "class {.cls element_text}."
                ), call = quote(theme()))
            }
            # render the patch title grob
            title <- element_grob(el, patch_title,
                margin_y = TRUE, margin_x = TRUE
            )
            title$name <- grobName(title, name)
        }

        name <- paste("plot.patch_title.position", border, sep = ".")
        pos <- arg_match0(
            .subset2(theme, name) %||% position,
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
            h <- .subset2(panel_pos, "t") - 4L # above original xlab
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
            v <- .subset2(panel_pos, "l") - 4L # left of the ylab
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
            h <- .subset2(panel_pos, "b") + 3L # below original xlab
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
            v <- .subset2(panel_pos, "r") + 3L # right of the ylab
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
