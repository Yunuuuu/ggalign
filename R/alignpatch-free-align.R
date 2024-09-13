#' Free from alignment
#'
#' [align_plots] will try to align plot panels, and every elements of the plot,
#' following functions romove these restrictions:
#' - `free_align`: if we want to compose plots without alignment of some panel
#' axes (panel won't be aligned). we can wrap the plot with `free_align`.
#' - `free_border`: If we want to compose plots without alignment of the panel
#' borders (but still align the panels themselves), we can wrap the plot with
#' `free_border`.
#' - `free_lab`: If we want to compose plots without alignment of the axis
#' title, we can wrap the plot with `free_lab`.
#' - `free_space`: Removing the ggplot element sizes when aligning.
#'
#' @param plot A [ggplot][ggplot2::ggplot] or [alignpatches][align_plots]
#' object.
#' @param axes Which axes shouldn't be aligned? A string containing
#' one or more of `r rd_values(.tlbr)`.
#' @return
#' - `free_align`: A modified version of `plot` with a `free_align` class.
#' @examples
#' # directly copied from patchwork
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
#' # We can fix this be using free (here, with the default "panel" type)
#' align_plots(free_align(p1), p2, ncol = 1L)
#'
#' # If we still want the panels to be aligned to the right, we can choose to
#' # free only the left side
#' align_plots(free_align(p1, axes = "l"), p2, ncol = 1L)
#'
#' # We could use "label" to fix the layout in a different way
#' align_plots(p1, free_lab(p2), ncol = 1L)
#'
#' # Another issue is that long labels are not using already available free
#' # space.
#' align_plots(NULL, p1, p2, p2)
#'
#' # This can be fixed with the "space" type
#' align_plots(NULL, free_space(p1, "l"), p2, p2)
#'
#' @export
#' @rdname free
free_align <- function(plot, axes = "tlbr") {
    UseMethod("free_align")
}

#' @export
free_align.ggplot <- function(plot, axes = "tlbr") {
    assert_position(axes)
    attr(plot, "free_axes") <- axes
    add_class(plot, "free_align")
}

#' @export
free_align.alignpatches <- free_align.ggplot

#' @export
free_align.free_border <- free_align.ggplot

#' @export
free_align.free_lab <- function(plot, axes = "tlbr") {
    assert_position(axes)
    # if axes are free, it's not necessary to free the labs
    free_labs <- setdiff_position(attr(plot, "free_labs"), axes)
    if (length(free_labs) == 0L) {
        attr(plot, "free_labs") <- NULL
        class(plot) <- setdiff(class(plot), "free_lab")
    } else {
        attr(plot, "free_labs") <- free_labs
    }
    NextMethod()
}

#' @export
free_align.free_align <- function(plot, axes = "tlbr") {
    assert_position(axes)
    attr(plot, "free_axes") <- union_position(attr(plot, "free_axes"), axes)
    plot
}

#' @export
free_align.default <- function(plot, axes = "tlbr") {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_align.wrapped_plot <- free_align.default

#' @importFrom ggplot2 ggproto ggproto_parent
#' @export
alignpatch.free_align <- function(x) {
    Parent <- NextMethod()
    ggproto(
        "PatchFreeAlign", Parent,
        free_axes = split_position(attr(x, "free_axes")),
        widths = function(self, free = self$free_axes, gt = self$gt) {
            ggproto_parent(Parent, self)$widths(free, gt = gt)
        },
        heights = function(self, free = self$free_axes, gt = self$gt) {
            ggproto_parent(Parent, self)$heights(free, gt = gt)
        },
        align_border = function(self, t = NULL, l = NULL, b = NULL, r = NULL,
                                gt = self$gt) {
            for (axis in self$free_axes) {
                assign(x = axis, value = NULL, envir = environment())
            }
            ggproto_parent(Parent, self)$align_border(
                t = t, l = l, b = b, r = r, gt = gt
            )
        }
    )
}
