#' @importFrom ggplot2 ggplotGrob
#' @importFrom gtable gtable_add_grob gtable_add_rows gtable_add_cols
#' @export
print.alignpatches <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    if (newpage) grid::grid.newpage()
    ggplot2::set_last_plot(x)
    p <- ggplot2::ggplot() +
        .subset2(.subset2(x, "layout"), "theme")
    p <- ggplotGrob(p)
    table <- patch_gtable(x)
    fix_respect <- is.matrix(.subset2(table, "respect"))

    # add background
    table <- gtable_add_grob(table,
        get_grob(p, "background"),
        1L, 1L, nrow(table), ncol(table),
        z = -Inf, name = "background"
    )

    # add margins
    table <- gtable_add_rows(table, p$heights[1L], 0L)
    table <- gtable_add_rows(table, utils::tail(p$heights, 1L))
    if (fix_respect) table$respect <- rbind(0L, table$respect, 0L)
    table <- gtable_add_cols(table, p$widths[1L], 0L)
    table <- gtable_add_cols(table, utils::tail(p$widths, 1L))
    if (fix_respect) table$respect <- cbind(0L, table$respect, 0L)

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
        grid::grid.draw(table),
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

#' Generate a plot grob
#'
#' @param x A `layout` object.
#' @export
ggalignGrob <- function(x) UseMethod("ggalignGrob")
