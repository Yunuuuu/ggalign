#' @export
print.alignpatches <- function(x, newpage = is.null(vp), vp = NULL, ...) {
    if (newpage) grid::grid.newpage()
    gtable <- patch_gtable(x)
    ggplot2::set_last_plot(x)
    if (!is.null(vp)) {
        if (is.character(vp)) {
            grid::seekViewport(vp)
        } else {
            grid::pushViewport(vp)
        }
        on.exit(grid::upViewport())
    }
    tryCatch(
        grid::grid.draw(gtable),
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
