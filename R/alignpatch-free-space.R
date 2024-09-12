#' @param ... What sizes of the ggplot2 elements to remove? Allowed values are:
#' `"title"`, `"subtitle"`, `"xlab-t"`, `"axis-t"`, `"strip-t"`,
#' `"patch-title-top"`, `"margin-t"`, `"ylab-l"`, `"axis-l"`, `"strip-l"`,
#' `"patch-title-left"`, `"margin-l"`, `"caption"`, `"xlab-b"`, `"axis-b"`,
#' `"strip-b"`, `"patch-title-bottom"`, `"margin-b"`, `"ylab-r"`, `"axis-r"`,
#' `"strip-r"`, `"patch-title-right"`, and `"margin-r"`. Some unions also
#' allowed:
#' - `"t"/"top`: "title", "subtitle", "xlab-t", "axis-t", "strip-t",
#'   "patch-title-top", "margin-t".
#' - `"l"/"left"`: "ylab-l", "axis-l", "strip-l", "patch-title-left",
#'   "margin-l".
#' - `"b"/"bottom"`: "caption", "xlab-b", "axis-b", "strip-b",
#'   "patch-title-bottom", "margin-b".
#' - `"r"/"right"`: "ylab-r", "axis-r", "strip-r", "patch-title-right",
#'   "margin-r".
#' - `"x"`: "xlab-t", "axis-t", "strip-t", "xlab-b", "axis-b", "strip-b".
#' - `"y"`: "ylab-l", "axis-l", "strip-l", "ylab-r", "axis-r", "strip-r".
#' - `"xlab"/"xlabs"`: "xlab-t", "xlab-b".
#' - `"ylab"/"ylabs"`: "ylab-l", "ylab-r".
#' - `"lab"/"labs"`: "xlab-t", "xlab-b", "ylab-l", "ylab-r".
#' - `"axis"/"axes"`: "axis-t", "axis-b", "axis-l", "axis-r".
#' - `"strip"/"strips"`: "strip-t", "strip-b", "strip-l", "strip-r".
#' - `"patch-title"`/`"patch-titles"`: "patch-title-top", "patch-title-left",
#'   "patch-title-bottom", "patch-title-right".
#' - `"margin"`/`"margins"`: "margin-t", "margin-l", "margin-b", "margin-r".
#' @return
#' - `free_space`: A modified version of `plot` with a `free_space` class.
#' @export
#' @rdname free
free_space <- function(plot, ...) {
    UseMethod("free_space")
}

#' @export
free_space.default <- function(plot, ...) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_space.ggplot <- function(plot, ...) {
    if (...length() == 0L) return(plot) # styler: off
    attr(plot, "free_spaces") <- check_ggelements(c(...), arg = "...")
    add_class(plot, "free_space")
}

#' @export
free_space.free_space <- function(plot, ...) {
    if (...length() == 0L) return(plot) # styler: off
    elements <- check_ggelements(c(...), arg = "...")
    attr(plot, "free_spaces") <- union(attr(plot, "free_spaces"), elements)
    plot
}

#' @export
free_space.wrapped_plot <- free_space.default

##########################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_space <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeLab", Parent,
        free_spaces = attr(x, "free_spaces"),
        patch_gtable = function(self, guides, plot = self$plot) {
            ans <- ggproto_parent(Parent, self)$patch_gtable(
                guides = guides, plot = plot
            )
            ggproto_parent(Parent, self)$free_space(
                free_spaces = self$free_spaces, gt = ans
            )
        },
        free_space = function(self, free_spaces, gt = self$gt) {
            if (length(free_spaces <- setdiff(free_spaces, self$free_spaces))) {
                ggproto_parent(Parent, self)$free_space(
                    free_spaces = free_spaces, gt = gt
                )
            } else {
                gt
            }
        }
    )
}

get_free_spaces <- function(ggelements, borders) {
    intersect(ggelements, unlist(.subset(GGELEMENTS, borders), FALSE, FALSE))
}

GGELEMENTS <- list(
    t = c(
        "title", "subtitle", "xlab-t", "axis-t", "strip-t",
        "patch-title-top", "margin-t"
    ),
    l = c("ylab-l", "axis-l", "strip-l", "patch-title-left", "margin-l"),
    b = c(
        "caption", "xlab-b", "axis-b", "strip-b", "patch-title-bottom",
        "margin-b"
    ),
    r = c("ylab-r", "axis-r", "strip-r", "patch-title-right", "margin-r")
)

ggelements_pos <- function(border, elements, strip_pos) {
    to_pos <- list(
        t = c(
            `strip-t` = -1L, `axis-t` = -2L, `xlab-t` = -3L,
            subtitle = -6, title = -7, `patch-title-top` = -8L,
            `margin-t` = -10L
        ),
        l = c(
            `strip-l` = -1L, `axis-l` = -2L, `ylab-l` = -3L,
            `patch-title-left` = -6L, `margin-l` = -8L
        ),
        b = c(
            `strip-b` = 1L, `axis-b` = 2L, `xlab-b` = 3L, caption = 6L,
            `patch-title-bottom` = 7L, `margin-b` = 9L
        ),
        r = c(
            `strip-r` = 1L, `axis-r` = 2L, `ylab-r` = 3L,
            `patch-title-right` = 6L, `margin-r` = 8L
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
    .subset(pos, elements)
}
