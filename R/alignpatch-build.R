#' @importFrom grid grid.draw
#' @importFrom rlang try_fetch cnd_signal
#' @export
print.alignpatches <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    ggplot2::set_last_plot(x)
    if (newpage) grid::grid.newpage()
    if (!is.null(vp)) {
        if (is.character(vp)) {
            grid::seekViewport(vp)
        } else {
            grid::pushViewport(vp)
        }
        on.exit(grid::upViewport())
    }
    # render the plot
    try_fetch(
        grid.draw(x, ...),
        error = function(e) {
            if (inherits(e, "simpleError") &&
                deparse(conditionCall(e)[[1L]]) == "grid.Call") {
                error_name <- style_cls(obj_type_friendly(x))
                if (Sys.getenv("RSTUDIO") == "1") {
                    cli::cli_abort(c(paste(
                        "The RStudio {.field Plots} window may be",
                        "too small to show", error_name
                    ), i = "Please make the window larger."), parent = e)
                } else {
                    cli::cli_abort(c(
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
grid.draw.alignpatches <- function(x, recording = TRUE) {
    grid.draw(ggalignGrob(x), recording = recording)
}

#' @export
ggalign_build.alignpatches <- function(x) x

#' @importFrom ggplot2 find_panel element_render theme theme_get
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @importFrom rlang arg_match0
#' @export
ggalign_gtable.alignpatches <- function(x) {
    titles <- .subset2(x, "titles")

    # ensure theme has no missing value
    theme <- .subset2(x, "theme") %||% theme_get()

    # `TO-DO`: use `complete_theme()` from ggplot2 release
    theme <- complete_theme(theme)
    x$theme <- theme
    table <- alignpatch(x)$patch_gtable(top_level = TRUE)

    fix_respect <- is.matrix(.subset2(table, "respect"))

    # Add title, subtitle, and caption -------------------
    # https://github.com/tidyverse/ggplot2/blob/2e08bba0910c11a46b6de9e375fade78b75d10dc/R/plot-build.R#L219C3-L219C9
    title <- element_render(
        theme = theme, "plot.title", .subset2(titles, "title"),
        margin_y = TRUE, margin_x = TRUE
    )
    title_height <- grobHeight(title)

    # Subtitle
    subtitle <- element_render(
        theme, "plot.subtitle", .subset2(titles, "subtitle"),
        margin_y = TRUE, margin_x = TRUE
    )
    subtitle_height <- grobHeight(subtitle)

    # caption
    caption <- element_render(
        theme, "plot.caption", .subset2(titles, "caption"),
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
        error_call = quote(theme())
    )

    caption_pos <- arg_match0(
        theme$plot.caption.position %||% "panel",
        values = c("panel", "plot"),
        arg_nm = "plot.caption.position",
        error_call = quote(theme())
    )
    if (title_pos == "panel") {
        title_l <- min(panel_pos$l)
        title_r <- max(panel_pos$r)
    } else {
        title_l <- 1L
        title_r <- ncol(table)
    }
    if (caption_pos == "panel") {
        caption_l <- min(panel_pos$l)
        caption_r <- max(panel_pos$r)
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
    if (fix_respect) {
        table$respect <- rbind(0L, 0L, table$respect, 0L)
    }

    # add margins --------------------------------------
    plot_margin <- calc_element("plot.margin", theme)

    table <- gtable_add_rows(table, plot_margin[1L], 0L)
    table <- gtable_add_rows(table, plot_margin[3L])
    if (fix_respect) table$respect <- rbind(0L, table$respect, 0L)
    table <- gtable_add_cols(table, plot_margin[2L], 0L)
    table <- gtable_add_cols(table, plot_margin[4L])
    if (fix_respect) table$respect <- cbind(0L, table$respect, 0L)

    # add background -----------------------------------
    if (inherits(theme$plot.background, "element")) {
        table <- gtable_add_grob(table,
            element_render(theme, "plot.background"),
            t = 1L, l = 1L, b = -1L, r = -1L, name = "background", z = -Inf
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
