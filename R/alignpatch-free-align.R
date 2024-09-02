#' Free from alignment
#'
#' [plot_grid] will try to align plots by their panels, and align every elements
#' of the plot, following functions romove this restrictions.
#' - `free_align`: if we want to compose plots without alignment of some panel
#' axes (panel won't be aligned). we can wrap the plot with `free_align`.
#' - `free_border`: If we want to compose plots without alignment of the panel
#' borders (but still align the panels themselves), we can wrap the plot with
#' `free_border`.
#' - `free_lab`: If we want to compose plots without alignment of the axis
#' title, we can wrap the plot with `free_lab`.
#' - `free_space`: Removing the ggplot element sizes when aligning.
#'
#' @param plot A [ggplot][ggplot2::ggplot] or [alignpatches][plot_grid] object.
#' @param axes Which axes shouldn't be aligned? Allowed values: `r rd_values(BORDERS)`.
#' @export
#' @return
#' - `free_align`: A modified version of `plot` with a `free_align` class.
#' @rdname free
free_align <- function(plot, axes = c("t", "l", "b", "r")) {
    UseMethod("free_align")
}

#' @export
free_align.ggplot <- function(plot, axes = c("t", "l", "b", "r")) {
    attr(plot, "free_axes") <- check_borders(axes)
    add_class(plot, "free_align")
}

#' @export
free_align.alignpatches <- free_align.ggplot

#' @export
free_align.free_border <- function(plot, axes = c("t", "l", "b", "r")) {
    free_borders <- attr(plot, "free_borders")
    # we always add `free_align` behind `free_border`
    class(plot) <- setdiff(class(plot), "free_border")
    plot <- NextMethod()
    free_border(plot, free_borders)
}

#' @export
free_align.free_lab <- function(plot, axes = c("t", "l", "b", "r")) {
    axes <- check_borders(axes)
    free_labs <- setdiff(attr(plot, "free_labs"), axes)
    if (length(free_labs) == 0L) {
        class(plot) <- setdiff(class(plot), "free_lab")
    } else {
        attr(plot, "free_labs") <- free_labs
    }
    NextMethod()
}

#' @export
free_align.free_align <- function(plot, axes = c("t", "l", "b", "r")) {
    attr(plot, "free_axes") <- union(attr(plot, "free_axes"), axes)
    plot
}

#' @export
free_align.default <- function(plot, axes = c("t", "l", "b", "r")) {
    cli::cli_abort("Cannot use with {.obj_type_friendly {plot}}")
}

#' @export
free_align.wrapped_plot <- free_align.default

#' @export
patch_gtable.free_align <- function(patch, guides) {
    class(patch) <- setdiff(class(patch), "free_align")

    # can be `gtable_ggplot` or `gtable_alignpatches`
    gt <- NextMethod()
    attr(gt, "free_axes") <- attr(patch, "free_axes")
    add_class(gt, "gtable_free_align")
}

#' @export
patch_align.gtable_free_align <- function(gt, guides,
                                          panel_width, panel_height) {
    list(
        gt = make_full_patch(gt,
            clip = "off", name = "free_align-table",
            borders = setdiff(c("t", "l", "b", "r"), attr(gt, "free_axes"))
        ),
        width = panel_width, height = panel_height, respect = FALSE
    )
}
