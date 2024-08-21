#' Arrange multiple plots into a grid
#'
#' @export
plot_grid <- function(..., ncol = NULL, nrow = NULL, byrow = NULL,
                      widths = NULL, heights = NULL, guides = NULL,
                      design = NULL) {
    if (is_valid_patch(..1)) {
        plots <- list(...)
    } else if (is.list(..1)) {
        plots <- ..1
    } else {
        cli::cli_abort(paste(
            "Can only wrap {.cls ggplot} and/or {.cls grob}",
            "objects or a list of them"
        ))
    }
    if (!all(vapply(plots, is_valid_patch, logical(1)))) {
        cli::cli_abort(paste(
            "Only know how to add {.cls ggplot}",
            "and/or {.cls grob} objects"
        ))
    }
    nms <- names(plots)
    if (!is.null(nms) && !is.null(design) && is.character(design)) {
        area_names <- unique(trimws(.subset2(strsplit(design, ""), 1L)))
        area_names <- sort(setdiff(area_names, c("", "#")))
        if (all(nms %in% area_names)) {
            plot_list <- vector("list", length(area_names))
            names(plot_list) <- area_names
            plot_list[nms] <- plots
            plots <- plot_list
        }
    }
    layout <- plot_layout(
        ncol = ncol, nrow = nrow, byrow = byrow,
        widths = widths, heights = heights,
        guides = guides, design = design
    )
    new_alignpatches(plots, layout)
}

new_alignpatches <- function(plots = list(), layout = plot_layout()) {
    structure(
        list(
            plots = plots, layout = layout,
            # Will ensure serialisation includes a link to the `ggalign`
            # namespace
            `_namespace` = ggalign_namespace_link
        ),
        class = "alignpatches"
    )
}

#' @importFrom ggplot2 is.ggplot
#' @importFrom grid is.grob
is_valid_patch <- function(x) {
    is.ggplot(x) || is.grob(x) || is.wrapped(x) || is.alignpatches(x)
}

is.alignpatches <- function(x) inherits(x, "alignpatches")
