#' Align Specifications in the Layout
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `scheme_align()` function defines the align Specifications for plots.
#'
#' @param guides A string with one or more of `r oxford_and(c(.tlbr, "i"))`
#' indicating which side of guide legends should be collected. Defaults to
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
#' @return A `scheme_align` object.
#' @examples
#' set.seed(123)
#' mat <- matrix(rnorm(72), nrow = 8)
#' # used in the layout, define the default action for all plots in the layout
#' ggheatmap(mat) -
#'     scheme_align(guides = NULL) +
#'     anno_right() +
#'     align_dendro(aes(color = branch), k = 3)
#'
#' # You can also add it for a single plot
#' ggheatmap(mat) -
#'     # for all plots in the layout, we default won't collect any guide legends
#'     scheme_align(guides = NULL) +
#'     # for the heatmap body, we collect guide legends in the right
#'     # note, the guide legends will be collected to the right side of the
#'     # layout which will overlap the legends in the right annotation
#'     scheme_align(guides = "r") +
#'     anno_right() +
#'     align_dendro(aes(color = branch), k = 3)
#'
#' # to avoid overlapping, we can also collect the guide legends in the
#' # right annotation
#' ggheatmap(mat) -
#'     scheme_align(guides = NULL) +
#'     scheme_align(guides = "r") +
#'     anno_right() +
#'     align_dendro(aes(color = branch), k = 3) +
#'     scheme_align(guides = "r")
#' @export
#' @include utils-S7.R
scheme_align <- S7::new_class(
    "scheme_align",
    parent = scheme,
    properties = list(
        guides = position_prop(),
        free_spaces = position_prop(),
        free_labs = position_prop()
    )
)

#' @importFrom utils modifyList
#' @export
update_scheme.scheme_align <- function(new, old, object_name) {
    modifyList(old,
        new[!vapply(new, identical, logical(1L), y = NA, USE.NAMES = FALSE)],
        keep.null = TRUE
    )
}

#' @export
inherit_scheme.scheme_align <- function(scheme, pscheme) {
    # `align_plots` control how to inherit `guides` from the layout
    # we don't need to inherit it here
    scheme["free_spaces"] <- list(.subset2(scheme, "free_spaces") %|w|%
        .subset2(pscheme, "free_spaces"))
    scheme["free_labs"] <- list(.subset2(scheme, "free_labs") %|w|%
        .subset2(pscheme, "free_labs"))
    scheme
}

#' @param theme Additional default theme elements to be added for the plot
#' @noRd
plot_add_scheme.scheme_align <- function(plot, scheme) {
    if (!is.waive(free_guides <- .subset2(scheme, "guides"))) {
        plot <- free_guide(plot, free_guides)
    }
    # by default, we'll attach all labs to the axis
    if (!is.null(free_labs <- .subset2(scheme, "free_labs") %|w|% "tlbr")) {
        plot <- free_lab(plot, free_labs)
    }
    # by default, we won't remove any spaces
    if (!is.null(free_spaces <- .subset2(scheme, "free_spaces") %|w|% NULL)) {
        plot <- free_space(free_border(plot, free_spaces), free_spaces)
    }
    plot
}
