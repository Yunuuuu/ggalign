#' @export
ggalign_build.patchwork <- function(x) as_alignpatches(x)

#' @export
#' @rdname ggalignGrob
ggalignGrob.patchwork <- function(x) patchwork::patchworkGrob(x)

######################################
# `patch` from `patchwork`: patchwork::plot_spacer
#' @export
ggalign_build.patch <- function(x) x

#' @export
#' @rdname ggalignGrob
ggalignGrob.patch <- function(x) patchwork::patchGrob(x)

# `patch` from `patchwork`: patchwork::plot_spacer
#' @importFrom gtable gtable_add_rows gtable_add_cols
#' @importFrom ggplot2 find_panel
#' @export
#' @rdname ggalign_build
#' @order 5
patch_gtable.patch <- function(patch, guides) {
    guides <- if (length(guides)) "collect" else "keep"
    table <- patchwork::patchGrob(patch, guides = guides)
    for (border in c("top", "left", "bottom", "right")) {
        panel_pos <- find_panel(table)
        if (border == "top") {
            h <- .subset2(panel_pos, "t") - 4L # above original xlab
            table <- gtable_add_rows(table, unit(0L, "mm"), pos = h)
        } else if (border == "left") {
            v <- .subset2(panel_pos, "l") - 4L # left of the ylab
            table <- gtable_add_cols(table, unit(0, "mm"), pos = v)
        } else if (border == "bottom") {
            h <- .subset2(panel_pos, "b") + 3L # below original xlab
            table <- gtable_add_rows(table, unit(0L, "mm"), pos = h)
        } else if (border == "right") {
            v <- .subset2(panel_pos, "r") + 3L # right of the ylab
            table <- gtable_add_cols(table, unit(0, "mm"), pos = v)
        }
    }
    table
}

#########################################
# `patch` from `patchwork`: patchwork::wrap_elements
#' @export
#' @rdname ggalign_build
#' @order 6
patch_gtable.wrapped_patch <- patch_gtable.patch

########################################
#' @export
#' @rdname as_alignpatches
as_alignpatches.patchwork <- function(x) {
    patches <- .subset2(x, "patches")
    plots <- .subset2(patches, "plots")
    if (!inherits(x, "plot_filler")) {
        plots <- c(plots, list(plot))
    }
    layout <- .subset2(patches, "layout")
    annotation <- .subset2(patches, "annotation")
    if (.subset2(layout, "guides") == "collect") {
        layout$guides <- TRUE
    } else {
        layout$guides <- FALSE
    }
    layout$theme <- .subset2(annotation, "theme")
    layout$title <- .subset2(annotation, "title")
    layout$subtitle <- .subset2(annotation, "subtitle")
    layout$caption <- .subset2(annotation, "caption")
    new_alignpatches(plots, layout)
}
