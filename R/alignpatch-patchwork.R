########################################
#' @importFrom utils modifyList getFromNamespace
#' @importFrom S7 convert
#' @export
alignpatch.patchwork <- function(x) {
    rlang::check_installed("patchwork", "to align patchwork")
    get_patches <- getFromNamespace("get_patches", "patchwork")
    # patchwork will keep the class when extracting patches from it.
    # we removed the classes for patchwork, added behind patchwork
    # in this way, the last plot won't have class like `free_align`,
    # `free_border`, `free_lab`, et al. which is added for the patchwork
    sub_patchwork_cls <- which(class(x) == "patchwork") - 1L # nolint
    if (sub_patchwork_cls > 0L) {
        class(x) <- class(x)[-seq_len(sub_patchwork_cls)]
    }
    x <- get_patches(x)
    layout <- .subset2(x, "layout")
    annotation <- .subset2(x, "annotation")
    default <- getFromNamespace("default_layout", "patchwork")
    layout <- modifyList(default, layout[
        !vapply(layout, is.null, logical(1L), USE.NAMES = FALSE)
    ])
    plots <- .subset2(x, "plots")
    plot <- class_alignpatches(plots)
    alignpatch(plot + layout + annotation)
}

#' @export
alignpatch.free_plot <- function(x) {
    if (inherits(x, "patchwork")) {
        free_settings <- attr(x, "patchwork_free_settings")
    } else {
        free_settings <- attr(x, "free_settings")
    }
    free_settings <- split(
        names(free_settings),
        factor(free_settings, rev(unique(free_settings)))
    )
    class(x) <- vec_set_difference(class(x), "free_plot")
    for (type in names(free_settings)) {
        side <- paste(.subset2(free_settings, type), collapse = "")
        x <- switch(type,
            panel = free_align(x, side),
            label = free_lab(x, side),
            space = free_space(free_border(x, side), side),
        )
    }
    alignpatch(x)
}

######################################
# `patch` from `patchwork`: patchwork::plot_spacer
#' @importFrom ggplot2 ggproto
#' @export
alignpatch.patch <- function(x) {
    rlang::check_installed(
        "patchwork", sprintf("to align %s plot", obj_type_friendly(x))
    )
    ggproto(NULL, PatchPatchworkPatch, patch = x)
}

#' @importFrom ggplot2 ggproto
PatchPatchworkPatch <- ggproto(
    "PatchPatchworkPatch", Patch,
    # `patch` from `patchwork`: patchwork::plot_spacer
    #' @importFrom gtable gtable_add_rows gtable_add_cols
    #' @importFrom ggplot2 find_panel
    gtable = function(self, theme = NULL, guides = NULL, tagger = NULL) {
        guides <- if (length(guides)) "collect" else "keep"
        ans <- patchwork::patchGrob(self$patch, guides = guides)
        # add rows and columns for `patch_titles()`
        for (border in .TLBR) {
            panel_pos <- find_panel(ans)
            if (border == "top") {
                h <- .subset2(panel_pos, "t") - 4L # above original xlab
                ans <- gtable_add_rows(ans, unit(0L, "mm"), pos = h)
            } else if (border == "left") {
                v <- .subset2(panel_pos, "l") - 4L # left of the ylab
                ans <- gtable_add_cols(ans, unit(0, "mm"), pos = v)
            } else if (border == "bottom") {
                h <- .subset2(panel_pos, "b") + 3L # below original xlab
                ans <- gtable_add_rows(ans, unit(0L, "mm"), pos = h)
            } else if (border == "right") {
                v <- .subset2(panel_pos, "r") + 3L # right of the ylab
                ans <- gtable_add_cols(ans, unit(0, "mm"), pos = v)
            }
        }
        if (!is.null(tagger)) {
            ans <- tagger$tag_table(ans, theme %||% theme_get())
        }
        ans
    }
)

#' @export
alignpatch.spacer <- function(x) NULL

#########################################
# `patch` from `patchwork`: patchwork::wrap_elements
#' @export
alignpatch.wrapped_patch <- alignpatch.patch
