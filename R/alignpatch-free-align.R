#' Free plots from alignment constraints
#'
#' By default, [alignpatches()]/[align_plots()] attempts to align all plot
#' panels and their elements. The following helper functions can be used to
#' selectively remove or relax these alignment constraints:
#'
#' - `free_align()`: Prevents alignment of specific plot panels along certain
#'   axes. Wrap the plot with `free_align()` if you want to compose plots
#'   without forcing axis alignment.
#' - `free_border()`: Aligns the panel area but not its surrounding borders
#'   (such as axis titles, tick marks, or labels).
#' - `free_lab()`: attaches axis titles and tick labels directly to the plot
#'   panel.
#' - `free_space()`: Removes border spacing when aligning plots.
#' - `free_vp`: Customize the [viewport][grid::viewport] when aligning.
#' - `free_guide`: If we want to override the behaviour of the overall guides
#'   behaviour, we can wrap the plot with `free_guide`.
#'
#' @param plot Any object that can be aligned with [alignpatches()]. In
#' practice, this means the object must implement the [`patch()`] method.
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
#' # `free_border()` is similar with `free_lab`, they have a distinction in
#' # terms of placement on either the top or bottom side of the panel.
#' # Specifically, the top side contains the `title` and `subtitle`, while the
#' # bottom side contains the `caption`. `free_border()` also free these
#' # elements when ligning.
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
patch.ggalign_free_align <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeAlign", Parent,
        axes = setup_position(attr(x, "ggalign_free_axes", exact = TRUE)),
        border_sizes = function(self, gt, options) {
            out <- ggproto_parent(Parent, self)$border_sizes(gt, options)
            free_spaces(out, self$axes)
        },
        align_border = function(self, gt, t, l, b, r, options) {
            for (axis in substring(self$axes, 1L, 1L)) {
                assign(x = axis, value = NULL, envir = environment())
            }
            ggproto_parent(Parent, self)$align_border(gt, t, l, b, r, options)
        }
    )
}
