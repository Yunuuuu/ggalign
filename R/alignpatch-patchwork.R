########################################
#' @importFrom utils modifyList
#' @export
alignpatch.patchwork <- function(x) {
    rlang::check_installed("patchwork", "to align patchwork")
    get_patches <- getFromNamespace("get_patches", "patchwork")
    x <- get_patches(x)
    plots <- .subset2(x, "plots")
    layout <- .subset2(x, "layout")
    annotation <- .subset2(x, "annotation")
    default <- getFromNamespace("default_layout", "patchwork")
    layout <- modifyList(default, layout[
        !vapply(layout, is.null, logical(1L), USE.NAMES = FALSE)
    ])
    if (identical(.subset2(layout, "guides"), "collect")) {
        layout$guides <- .TLBR
    } else {
        layout$guides <- NULL
    }
    layout$theme <- .subset2(annotation, "theme")
    layout$title <- .subset2(annotation, "title")
    layout$subtitle <- .subset2(annotation, "subtitle")
    layout$caption <- .subset2(annotation, "caption")
    alignpatch(new_alignpatches(lapply(plots, alignpatch), layout))
}

######################################
# `patch` from `patchwork`: patchwork::plot_spacer
#' @importFrom ggplot2 ggproto
#' @export
alignpatch.patch <- function(x) {
    rlang::check_installed("patchwork", "to align patch")
    ggproto(NULL, PatchPatchworkPatch, plot = x)
}

#' @importFrom ggplot2 ggproto
PatchPatchworkPatch <- ggproto(
    "PatchPatchworkPatch", Patch,
    # `patch` from `patchwork`: patchwork::plot_spacer
    #' @importFrom gtable gtable_add_rows gtable_add_cols
    #' @importFrom ggplot2 find_panel
    patch_gtable = function(self, guides, plot = self$plot) {
        guides <- if (length(guides)) "collect" else "keep"
        table <- patchwork::patchGrob(patch, guides = guides)
        for (border in .TLBR) {
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
)

#' @export
alignpatch.spacer <- function(x) NULL

#########################################
# `patch` from `patchwork`: patchwork::wrap_elements
#' @export
alignpatch.wrapped_patch <- alignpatch.patch
