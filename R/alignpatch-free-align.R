#' Disable the align of panel axes
#'
#' @export
free_align <- function(plot, axes = c("t", "l", "b", "r")) {
    UseMethod("free_align")
}

#' @export
free_align.ggplot <- function(plot, axes = c("t", "l", "b", "r")) {
    if (length(axes) == 0L) {
        return(plot)
    }
    attr(plot, "free_axes") <- axes
    add_class(plot, "free_align")
}

#' @export
free_align.alignpatches <- free_align.ggplot

#' @export
free_align.free_border <- function(plot, axes = c("t", "l", "b", "r")) {
    free_borders <- attr(plot, "free_borders")
    class(plot) <- setdiff(class(plot), "free_border")
    plot <- NextMethod()
    free_border(plot, free_borders)
}

#' @export
free_align.free_lab <- function(plot, axes = c("t", "l", "b", "r")) {
    attr(plot, "free_labs") <- setdiff(attr(plot, "free_labs"), axes)
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
free_align.align_wrapped <- free_align.default

#' @export
patch_gtable.free_align <- function(patch) {
    gt <- NextMethod()
    attr(gt, "free_axes") <- attr(patch, "free_axes")
    add_class(gt, "gtable_free_align")
}

#' @export
patch_align.gtable_free_align <- function(gt, guides) {
    make_full_patch(gt,
        clip = "off", name = "free_align-table",
        borders = setdiff(c("t", "l", "b", "r"), attr(gt, "free_axes"))
    )
}
