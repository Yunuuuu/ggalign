#' @param ... What ggplot2 elements to remove?
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
    t = c("title", "subtitle", "xlab-t", "axis-t", "strip-t"),
    l = c("ylab-l", "axis-l", "strip-l"),
    b = c("caption", "xlab-b", "axis-b", "strip-b"),
    r = c("ylab-r", "axis-r", "strip-r")
)

ggelements_pos <- function(border, ggelements, strip_pos) {
    to_pos <- list(
        t = c(
            `strip-t` = -1L, `axis-t` = -2L, `xlab-t` = -3L,
            subtitle = -6, title = -7
        ),
        l = c(`strip-l` = -1L, `axis-l` = -2L, `ylab-l` = -3L),
        b = c(`strip-b` = 1L, `axis-b` = 2L, `xlab-b` = 3L, caption = 6L),
        r = c(`strip-r` = 1L, `axis-r` = 2L, `ylab-r` = 3L)
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
