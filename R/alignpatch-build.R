#' @include alignpatch-ggplot2.R
local(S7::method(print, alignpatches) <- print.patch_ggplot)

#' @importFrom grid grid.draw
#' @include alignpatch-ggplot2.R
local(S7::method(grid.draw, alignpatches) <- grid.draw.patch_ggplot)

S7::method(ggalign_build, alignpatches) <- function(x) x

#' @importFrom ggplot2 find_panel element_render theme theme_get complete_theme
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @importFrom rlang arg_match0
#' @importFrom S7 prop
S7::method(ggalign_gtable, alignpatches) <- function(x) {
    p <- patch(x)
    options <- p$setup_options(patch_options())
    table <- p$gtable(options)

    # ensure theme has no missing value
    theme <- prop(options, "theme")
    matrix_respect <- is.matrix(.subset2(table, "respect"))

    # Add title, subtitle, and caption -------------------
    titles <- ggalign_init(prop(x, "titles"))
    # https://github.com/tidyverse/ggplot2/blob/2e08bba0910c11a46b6de9e375fade78b75d10dc/R/plot-build.R#L219C3-L219C9
    title <- element_render(
        theme, "plot.title", prop(titles, "title"),
        margin_y = TRUE, margin_x = TRUE
    )
    title_height <- grobHeight(title)

    # Subtitle
    subtitle <- element_render(
        theme, "plot.subtitle", prop(titles, "subtitle"),
        margin_y = TRUE, margin_x = TRUE
    )
    subtitle_height <- grobHeight(subtitle)

    # caption
    caption <- element_render(
        theme, "plot.caption", prop(titles, "caption"),
        margin_y = TRUE, margin_x = TRUE
    )
    caption_height <- grobHeight(caption)

    # positioning of title and subtitle is governed by plot.title.position
    # positioning of caption is governed by plot.caption.position
    #   "panel" means align to the panel(s)
    #   "plot" means align to the entire plot (except margins and tag)
    panel_pos <- find_panel(table)
    title_pos <- arg_match0(
        theme$plot.title.position %||% "panel",
        c("panel", "plot"),
        arg_nm = "plot.title.position",
        error_call = quote(layout_theme())
    )

    caption_pos <- arg_match0(
        theme$plot.caption.position %||% "panel",
        values = c("panel", "plot"),
        arg_nm = "plot.caption.position",
        error_call = quote(layout_theme())
    )
    if (title_pos == "panel") {
        title_l <- panel_pos$l
        title_r <- panel_pos$r
    } else {
        title_l <- 1L
        title_r <- ncol(table)
    }
    if (caption_pos == "panel") {
        caption_l <- panel_pos$l
        caption_r <- panel_pos$r
    } else {
        caption_l <- 1L
        caption_r <- ncol(table)
    }

    table <- gtable_add_rows(table, subtitle_height, pos = 0)
    table <- gtable_add_grob(table, subtitle,
        name = "subtitle",
        t = 1, b = 1, l = title_l, r = title_r, clip = "off"
    )

    table <- gtable_add_rows(table, title_height, pos = 0)
    table <- gtable_add_grob(table, title,
        name = "title",
        t = 1, b = 1, l = title_l, r = title_r, clip = "off"
    )

    table <- gtable_add_rows(table, caption_height, pos = -1)
    table <- gtable_add_grob(table, caption,
        name = "caption",
        t = -1, b = -1, l = caption_l, r = caption_r, clip = "off"
    )
    if (matrix_respect) table$respect <- rbind(0L, 0L, table$respect, 0L)

    # add margins --------------------------------------
    plot_margin <- calc_element("plot.margin", theme)

    table <- gtable_add_rows(table, plot_margin[1L], 0L)
    table <- gtable_add_rows(table, plot_margin[3L])
    if (matrix_respect) table$respect <- rbind(0L, table$respect, 0L)

    table <- gtable_add_cols(table, plot_margin[2L], 0L)
    table <- gtable_add_cols(table, plot_margin[4L])
    if (matrix_respect) table$respect <- cbind(0L, table$respect, 0L)

    # add background -----------------------------------
    if (is_theme_element(theme$plot.background)) {
        table <- gtable_add_grob(table,
            element_render(theme, "plot.background"),
            t = 1L, l = 1L, b = -1L, r = -1L, name = "background",
            z = LAYOUT_BACKGROUND_Z
        )
        table$layout <- table$layout[
            c(nrow(table$layout), 1:(nrow(table$layout) - 1L)),
        ]
        table$grobs <- table$grobs[
            c(nrow(table$layout), 1:(nrow(table$layout) - 1L))
        ]
    }
    table
}
