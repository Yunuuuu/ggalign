#' Align Specifications in the Layout
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `plot_align()` function defines the align Specifications for plots.
#'
#' @param guides A string with one or more of `r oxford_and(.tlbr)` indicating
#' which side of guide legends should be collected. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If no
#' parent layout, all guides will be collected. If `NULL`, no guides will be
#' collected.
#'
#' @param free_spaces A string with one or more of `r oxford_and(.tlbr)`
#' indicating which border spaces should be removed. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If no
#' parent, the default is `NULL`, meaning no spaces are removed.
#'
#' Usually you want to apply this with the whole layout, instead of individual
#' plots.
#'
#' @param free_labs A string with one or more of `r oxford_and(.tlbr)`
#' indicating which axis titles should be free from alignment. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If no
#' parent layout, no axis titles will be aligned. If `NULL`, all axis titles
#' will be aligned.
#'
#' @return A `plot_align` object.
#' @examples
#' # used in the layout, define the default action for all plots in the layout
#' ggheatmap(matrix(rnorm(72), nrow = 8)) -
#'     theme(plot.background = element_rect(fill = "red"))
#'
#' # You can also add it for a single plot
#' ggheatmap(matrix(rnorm(72), nrow = 8)) -
#'     theme(plot.background = element_rect(fill = "red")) +
#'     # here, we modify the plot action for the heatmap body
#'     theme(plot.background = element_rect(fill = "blue"))
#'
#' @export
plot_align <- function(guides = NA, free_spaces = NA, free_labs = NA) {
    if (!identical(free_spaces, NA)) assert_layout_position(free_spaces)
    if (!identical(free_labs, NA)) assert_layout_position(free_labs)
    if (!identical(guides, NA)) assert_layout_position(guides)
    new_plot_align(
        free_spaces = free_spaces, free_labs = free_labs,
        guides = guides
    )
}

new_plot_align <- function(guides = waiver(), free_spaces = waiver(),
                           free_labs = waiver()) {
    new_option(
        name = "plot_align",
        list(free_spaces = free_spaces, free_labs = free_labs, guides = guides),
        class = "plot_align"
    )
}

#' @export
update_option.plot_align <- function(new, old, object_name) {
    update_plot_align(old, new)
}

#' @importFrom utils modifyList
update_plot_align <- function(old, new) {
    modifyList(old,
        new[!vapply(new, identical, logical(1L), y = NA, USE.NAMES = FALSE)],
        keep.null = TRUE
    )
}

#' @export
inherit_option.plot_align <- function(option, poption) {
    # `align_plots` control how to inherit `guides` from the layout
    # we don't need to inherit it here
    option["free_spaces"] <- list(.subset2(option, "free_spaces") %|w|%
        .subset2(poption, "free_spaces"))
    option["free_labs"] <- list(.subset2(option, "free_labs") %|w|%
        .subset2(poption, "free_labs"))
    option
}

#' @param theme Additional default theme elements to be added for the plot
#' @noRd
plot_add.plot_align <- function(option, plot) {
    if (!is.waive(free_guides <- .subset2(option, "guides"))) {
        plot <- free_guide(plot, free_guides)
    }
    # by default, we'll attach all labs to the axis
    if (!is.null(free_labs <- .subset2(option, "free_labs") %|w|% "tlbr")) {
        plot <- free_lab(plot, free_labs)
    }
    # by default, we won't remove any spaces
    if (!is.null(free_spaces <- .subset2(option, "free_spaces") %|w|% NULL)) {
        plot <- free_space(free_border(plot, free_spaces), free_spaces)
    }
    plot
}

deprecate_action <- function(fun, guides = deprecated(),
                             free_guides = deprecated(),
                             free_spaces = deprecated(),
                             free_labs = deprecated(),
                             plot_data = deprecated(),
                             theme = deprecated(),
                             call = caller_call()) {
    if (lifecycle::is_present(free_guides)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(free_guides)", fun),
            "plot_align()",
            env = call
        )
    }
    if (lifecycle::is_present(guides)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(guides)", fun),
            "plot_align()",
            env = call
        )
    }
    if (lifecycle::is_present(free_spaces)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(free_spaces)", fun),
            "plot_align()",
            env = call
        )
    }
    if (lifecycle::is_present(free_labs)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(free_labs)", fun),
            "plot_align()",
            env = call
        )
    }
    if (lifecycle::is_present(plot_data)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(plot_data)", fun),
            "plot_data()",
            env = call
        )
    }
    if (lifecycle::is_present(theme)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(theme)", fun),
            "plot_theme()",
            env = call
        )
    }
}
