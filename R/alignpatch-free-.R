#' Free from alignment
#'
#' [align_plots] will try to align plot panels, and every elements of the plot,
#' following functions romove these restrictions:
#' - `free_align`: if we want to compose plots without alignment of some panel
#' axes (panel won't be aligned). we can wrap the plot with `free_align`.
#' - `free_border`: attaches borders (e.g., axis titles, tick marks) directly to
#'   the plot panel. This keeps them visually close to the panel during
#'   alignment.
#' - `free_lab()`: Similar to `free_border()`, but only attaches axis titles
#'   and tick labels, not full borders. It's mainly included for completeness;
#'   in most cases, combining `free_border()` and `free_space()` is sufficient.
#' - `free_space`: Removing the ggplot element sizes when aligning.
#' - `free_vp`: Customize the [viewport][grid::viewport] when aligning.
#' - `free_guide`: If we want to override the behaviour of the overall guides
#'   behaviour, we can wrap the plot with `free_guide`.
#'
#' @param plot A [ggplot][ggplot2::ggplot] or [alignpatches][align_plots]
#' object.
#' @examples
#' # directly copied from `patchwork`
#' # Sometimes you have a plot that defies good composition alginment, e.g. due
#' # to long axis labels
#' p1 <- ggplot(mtcars) +
#'     geom_bar(aes(y = factor(gear), fill = factor(gear))) +
#'     scale_y_discrete(
#'         "",
#'         labels = c(
#'             "3 gears are often enough",
#'             "But, you know, 4 is a nice number",
#'             "I would def go with 5 gears in a modern car"
#'         )
#'     )
#'
#' # When combined with other plots it ends up looking bad
#' p2 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#'
#' align_plots(p1, p2, ncol = 1L)
#'
#' # We can fix this be using `free_align`
#' align_plots(free_align(p1), p2, ncol = 1L)
#'
#' # If we still want the panels to be aligned to the right, we can choose to
#' # free only the left side
#' align_plots(free_align(p1, axes = "l"), p2, ncol = 1L)
#'
#' # We could use `free_lab` to fix the layout in a different way
#' align_plots(p1, free_lab(p2), ncol = 1L)
#'
#' # `free_border` is similar with `free_lab`, they have a distinction in terms
#' # of placement on either the top or bottom side of the panel. Specifically,
#' # the top side contains the `title` and `subtitle`, while the bottom side
#' # contains the `caption`. free_lab() does not attach these elements in the
#' # panel area.
#' p3 <- ggplot(mtcars) +
#'     geom_point(aes(hp, wt, colour = mpg)) +
#'     ggtitle("Plot 3")
#' p_axis_top <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp)) +
#'     ggtitle("Plot axis in top") +
#'     scale_x_continuous(position = "top")
#' align_plots(p_axis_top, free_lab(p3))
#' align_plots(p_axis_top, free_border(p3))
#'
#' # Another issue is that long labels can occupy much spaces
#' align_plots(NULL, p1, p2, p2)
#'
#' # This can be fixed with `free_space`
#' align_plots(NULL, free_space(p1, "l"), p2, p2)
#'
#' @name free
NULL

#' @include alignpatch-free-border.R
#' @include alignpatch-free-align.R
S7::method(free_border, FreeAlign) <- function(plot, borders = "tlbr") {
    out <- FreeBorder(plot, borders)

    # Remove axes from the requested borders
    borders <- setdiff_position(borders, prop(plot, "axes"))
    if (nzchar(borders)) { # update the borders
        prop(out, "borders", check = FALSE) <- borders
        out
    } else {
        # If no borders, return the original plot
        plot
    }
}

#' @include alignpatch-free-align.R
#' @include alignpatch-free-border.R
S7::method(free_align, FreeBorder) <- function(plot, axes = "tlbr") {
    out <- FreeAlign(plot, axes)

    # Remove axes from the original borders
    borders <- setdiff_position(prop(plot, "borders"), axes)
    if (nzchar(borders)) {
        # update the borders
        prop(prop(out, "plot"), "borders", check = FALSE) <- borders
    } else {
        # If no borders, revert the nested plot to original
        prop(out, "plot", check = FALSE) <- prop(prop(out, "plot"), "plot")
    }
    out
}

#' @include alignpatch-free-lab.R
#' @include alignpatch-free-border.R
S7::method(free_lab, FreeBorder) <- function(plot, labs = "tlbr") {
    out <- FreeLab(plot, labs)

    # Remove axes from the requested labs
    labs <- setdiff_position(labs, prop(plot, "borders"))
    if (nzchar(labs)) { # update the labs
        prop(out, "labs", check = FALSE) <- labs
        out
    } else {
        # If no labs, return the original plot
        plot
    }
}

#' @include alignpatch-free-lab.R
#' @include alignpatch-free-align.R
S7::method(free_lab, FreeAlign) <- function(plot, labs = "tlbr") {
    out <- FreeLab(plot, labs)

    # Remove axes from the requested labs
    labs <- setdiff_position(labs, prop(plot, "axes"))
    if (nzchar(labs)) { # update the labs
        prop(out, "labs", check = FALSE) <- labs
        out
    } else {
        # If no labs, return the original plot
        plot
    }
}

#' @include alignpatch-free-align.R
#' @include alignpatch-free-lab.R
S7::method(free_align, FreeLab) <- function(plot, axes = "tlbr") {
    out <- FreeAlign(plot, axes)

    # Remove axes from the original borders
    labs <- setdiff_position(prop(plot, "labs"), axes)
    if (nzchar(labs)) {
        # update the labs
        prop(prop(out, "plot"), "labs", check = FALSE) <- labs
    } else {
        # If no labs, revert the nested plot to original
        prop(out, "plot", check = FALSE) <- prop(prop(out, "plot"), "plot")
    }
    out
}

#' @include alignpatch-free-align.R
#' @include alignpatch-free-space.R
S7::method(free_align, FreeSpace) <- function(plot, axes = "tlbr") {
    out <- FreeAlign(plot, axes)

    # Remove axes from the original borders
    spaces <- setdiff_position(prop(plot, "spaces"), axes)
    if (nzchar(spaces)) {
        # update the spaces
        prop(prop(out, "plot"), "spaces", check = FALSE) <- spaces
    } else {
        # If no spaces, revert the nested plot to original
        prop(out, "plot", check = FALSE) <- prop(prop(out, "plot"), "plot")
    }
    out
}

#' @include alignpatch-free-space.R
#' @include alignpatch-free-align.R
S7::method(free_space, FreeAlign) <- function(plot, spaces = "tlbr") {
    out <- FreeSpace(plot, spaces)

    # Remove axes from the requested spaces
    spaces <- setdiff_position(spaces, prop(plot, "axes"))
    if (nzchar(spaces)) { # update the spaces
        prop(out, "spaces", check = FALSE) <- spaces
        out
    } else {
        # If no spaces, return the original plot
        plot
    }
}

