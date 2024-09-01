#' @param ... What sizes of the ggplot2 elements to remove? Allowed values are:
#' `"title"`, `"subtitle"`, `"xlab-t"`, `"axis-t"`, `"strip-t"`,
#' `"patch-title-top"`, `"ylab-l"`, `"axis-l"`, `"strip-l"`,
#' `"patch-title-left"`, `"caption"`, `"xlab-b"`, `"axis-b"`, `"strip-b"`,
#' `"patch-title-bottom"`, `"ylab-r"`, `"axis-r"`, `"strip-r"`, and,
#' `"patch-title-right"`. Some unions also allowed:
#' - `"t"/"top`: "title", "subtitle", "xlab-t", "axis-t", "strip-t",
#'   "patch-title-top".
#  - `"l"/"left"`: "ylab-l", "axis-l", "strip-l", "patch-title-left".
#  - `"b"/"bottom"`："caption", "xlab-b", "axis-b", "strip-b",
#    "patch-title-bottom".
#  - `"r"/"right"`: "ylab-r", "axis-r", "strip-r", "patch-title-right".
#  - `"x"`: "xlab-t", "axis-t", "strip-t", "xlab-b", "axis-b", "strip-b".
#  - `"y"`: "ylab-l", "axis-l", "strip-l", "ylab-r", "axis-r", "strip-r".
#' - `"xlab"/"xlabs"`: "xlab-t", "xlab-b".
#' - `"ylab"/"ylabs"`: "ylab-l", "ylab-r".
#' - `"lab"/"labs"`: "xlab-t", "xlab-b", "ylab-l", "ylab-r".
#' - `"axis"/"axes"`: "axis-t", "axis-b", "axis-l", "axis-r".
#' - `"strip"/"strips"`: "strip-t", "strip-b", "strip-l", "strip-r".
#' - `"patch-title"`/`"patch-titles"`: "patch-title-top", "patch-title-left",
#'   "patch-title-bottom", "patch-title-right".
#' @return
#' - `free_size`: A modified version of `plot` with a `free_size` class.
#' @export
#' @rdname free
free_size <- function(plot, ...) {
    UseMethod("free_size")
}

#' @export
free_size.default <- function(plot, ...) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_size.ggplot <- function(plot, ...) {
    if (...length() == 0L) return(plot) # styler: off
    attr(plot, "free_sizes") <- check_ggelements(c(...), arg = "...")
    add_class(plot, "free_size")
}

#' @export
free_size.free_size <- function(plot, ...) {
    if (...length() == 0L) return(plot) # styler: off
    elements <- check_ggelements(c(...), arg = "...")
    attr(plot, "free_sizes") <- union(attr(plot, "free_sizes"), elements)
    plot
}

#' @export
free_size.wrapped_plot <- free_size.default

##########################################################
#' @export
patch_gtable.free_size <- function(patch, guides) {
    class(patch) <- setdiff(class(patch), "free_size")
    gt <- NextMethod()
    remove_border_sizes(gt, attr(patch, "free_sizes"))
}

#' @importFrom ggplot2 find_panel
remove_border_sizes <- function(gt, ggelements) {
    strip_pos <- find_strip_pos(gt)
    ggelements <- lapply(GGELEMENTS, intersect, ggelements)
    panel_pos <- find_panel(gt)
    for (border in names(ggelements)) {
        elements <- .subset2(ggelements, border)
        if (length(elements) == 0L) next
        pos <- .subset2(panel_pos, border) +
            ggelements_pos(border, ggelements, strip_pos)
        if (border %in% c("t", "b")) {
            gt$heights[pos] <- unit(0, "mm")
        } else {
            gt$widths[pos] <- unit(0, "mm")
        }
    }
    gt
}

GGELEMENTS <- list(
    t = c(
        "title", "subtitle", "xlab-t", "axis-t", "strip-t",
        "patch-title-top"
    ),
    l = c("ylab-l", "axis-l", "strip-l", "patch-title-left"),
    b = c("caption", "xlab-b", "axis-b", "strip-b", "patch-title-bottom"),
    r = c("ylab-r", "axis-r", "strip-r", "patch-title-right")
)

ggelements_pos <- function(border, ggelements, strip_pos) {
    to_pos <- list(
        t = c(
            `strip-t` = -1L, `axis-t` = -2L, `xlab-t` = -3L,
            subtitle = -6, title = -7, `patch-title-top` = -8L
        ),
        l = c(
            `strip-l` = -1L, `axis-l` = -2L, `ylab-l` = -3L,
            `patch-title-left` = -6L
        ),
        b = c(
            `strip-b` = 1L, `axis-b` = 2L, `xlab-b` = 3L, caption = 6L,
            `patch-title-bottom` = 7L
        ),
        r = c(
            `strip-r` = 1L, `axis-r` = 2L, `ylab-r` = 3L,
            `patch-title-right` = 6L
        )
    )
    pos <- .subset2(to_pos, border)
    if (strip_pos == "outside") {
        # switch between axis and strip position
        pos <- rename(
            pos,
            structure(
                c(
                    paste("strip", c("t", "l", "b", "r"), sep = "-"),
                    paste("axis", c("t", "l", "b", "r"), sep = "-")
                ),
                names = c(
                    paste("axis", c("t", "l", "b", "r"), sep = "-"),
                    paste("strip", c("t", "l", "b", "r"), sep = "-")
                )
            )
        )
    }
    pos
}
