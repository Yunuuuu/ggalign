#' @export
print.alignpatches <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    if (newpage) grid::grid.newpage()
    ggplot2::set_last_plot(x)
    # render plot
    if (!is.null(vp)) {
        if (is.character(vp)) {
            grid::seekViewport(vp)
        } else {
            grid::pushViewport(vp)
        }
        on.exit(grid::upViewport())
    }
    tryCatch(
        grid.draw(x, ...),
        error = function(e) {
            if (inherits(e, "simpleError") &&
                deparse(conditionCall(e)[[1L]]) == "grid.Call") {
                if (Sys.getenv("RSTUDIO") == "1") {
                    cli::cli_abort(c(
                        "The RStudio {.field Plots} window may be too small to show this patchwork.",
                        i = "Please make the window larger."
                    ))
                } else {
                    cli::cli_abort(c(
                        "The viewport may be too small to show this patchwork.",
                        i = "Please make the window larger."
                    ))
                }
            }
        }
    )
    invisible(x)
}

#' @importFrom ggplot2 ggplot ggplotGrob calc_element element_render
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.alignpatches <- function(x, recording = TRUE) {
    theme <- .subset2(.subset2(x, "layout"), "theme")
    # use complete_theme() when ggplot2 release
    if (!attr(theme, "complete")) {
        theme <- ggplot2::theme_get() + theme
        x$layout$theme <- theme
    }
    table <- patch_gtable(x)

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

    # add margins --------------------------------------
    plot_margin <- calc_element("plot.margin", theme)
    fix_respect <- is.matrix(.subset2(table, "respect"))

    table <- gtable_add_rows(table, plot_margin[1L], 0L)
    table <- gtable_add_rows(table, plot_margin[3L])
    if (fix_respect) table$respect <- rbind(0L, table$respect, 0L)
    table <- gtable_add_cols(table, plot_margin[2L], 0L)
    table <- gtable_add_cols(table, plot_margin[4L])
    if (fix_respect) table$respect <- cbind(0L, table$respect, 0L)
    grid.draw(table, recording = recording)
}
