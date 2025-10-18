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
#' @param axes Which axes shouldn't be aligned? A string containing
#' one or more of `r oxford_and(.tlbr)`.
#' @return
#' - `free_align`: A modified version of `plot` with a `ggalign_free_align`
#'   class.
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
#' @export
#' @rdname free
free_align <- function(plot, axes = "tlbr") {
    assert_position(axes)
    UseMethod("free_align")
}

#' @export
free_align.default <- function(plot, axes = "tlbr") {
    attr(plot, "ggalign_free_axes") <- axes
    add_class(plot, "ggalign_free_align")
}

#' @importFrom rlang is_empty
#' @export
free_align.ggalign_free_lab <- function(plot, axes = "tlbr") {
    # if axes are free, it's not necessary to free the labs
    labs <- setdiff_position(
        attr(plot, "ggalign_free_labs", exact = TRUE),
        axes
    )
    if (nzchar(labs)) {
        attr(plot, "ggalign_free_labs") <- labs
    } else {
        attr(plot, "ggalign_free_labs") <- NULL
        plot <- remove_class(plot, "ggalign_free_lab")
    }
    NextMethod()
}

#' @importFrom rlang is_empty
#' @export
free_align.ggalign_free_space <- function(plot, axes = "tlbr") {
    spaces <- setdiff_position(
        attr(plot, "ggalign_free_spaces", exact = TRUE),
        axes
    )
    if (nzchar(spaces)) {
        attr(plot, "ggalign_free_spaces") <- spaces
    } else {
        attr(plot, "ggalign_free_spaces") <- NULL
        plot <- remove_class(plot, "ggalign_free_space")
    }
    NextMethod()
}

#' @importFrom rlang is_empty
#' @export
free_align.ggalign_free_border <- function(plot, axes = "tlbr") {
    borders <- setdiff_position(
        attr(plot, "ggalign_free_borders", exact = TRUE),
        axes
    )
    if (nzchar(borders)) {
        attr(plot, "ggalign_free_borders") <- borders
    } else {
        attr(plot, "ggalign_free_borders") <- NULL
        plot <- remove_class(plot, "ggalign_free_border")
    }
    NextMethod()
}

#' @export
free_align.ggalign_free_align <- function(plot, axes = "tlbr") {
    attr(plot, "ggalign_free_axes") <- union_position(
        attr(plot, "ggalign_free_axes", exact = TRUE), axes
    )
    plot
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.ggalign_free_align <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeAlign", Parent,
        axes = split_position(attr(x, "ggalign_free_axes", exact = TRUE)),
        get_sizes = function(self, gt, free = NULL) {
            ggproto_parent(Parent, self)$get_sizes(gt, union(free, self$axes))
        },
        align_border = function(self, gt, t = NULL, l = NULL,
                                b = NULL, r = NULL) {
            for (axis in self$axes) {
                assign(x = axis, value = NULL, envir = environment())
            }
            ggproto_parent(Parent, self)$align_border(gt, t, l, b, r)
        }
    )
}
