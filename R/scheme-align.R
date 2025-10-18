#' @importFrom rlang is_na
prop_char_sets <- function(allowed, ...) {
    allowed <- vec_unique(allowed)
    pattern <- paste0("[^", paste0(allowed, collapse = ""), "]")
    S7::new_property(
        S7::class_any,
        validator = function(value) {
            if (is_na(value) || is_waiver(value) || is.null(value)) {
                return(NULL)
            }
            if (is_string(value) && !grepl(pattern, value)) {
                return(NULL)
            }
            sprintf(
                "must be a single string containing only the characters %s",
                oxford_and(allowed)
            )
        },
        ...,
        default = NA
    )
}

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
#' @importFrom rlang is_na
#' @export
scheme_align <- S7::new_class(
    "scheme_align",
    parent = Scheme,
    properties = list(
        guides = prop_char_sets(c(.tlbr, "i")),
        free_spaces = prop_char_sets(.tlbr),
        free_labs = prop_char_sets(.tlbr)
    )
)

#' @importFrom S7 prop prop<-
S7::method(scheme_init, scheme_align) <- function(scheme) {
    # By default, we collect all guide legends
    prop(scheme, "guides", check = FALSE) <- prop(scheme, "guides") %|w|% "tlbr"
    scheme
}

#' @importFrom rlang is_na
#' @importFrom S7 props prop<-
S7::method(scheme_update, list(scheme_align, scheme_align)) <-
    function(e1, e2, ...) {
        new <- props(e2)
        # key is read-only property
        new$key <- NULL
        for (nm in names(new)) {
            if (is_na(.subset2(new, nm))) next
            prop(e1, nm, check = FALSE) <- .subset2(new, nm)
        }
        e1
    }

#' @importFrom S7 prop prop<-
S7::method(scheme_inherit, list(scheme_align, scheme_align)) <-
    function(e1, e2) {
        # `alignpatches()` control how to inherit `guides` from the layout
        # we don't need to inherit it here
        prop(e2, "free_spaces", check = FALSE) <- prop(e2, "free_spaces") %|w|%
            prop(e1, "free_spaces")
        prop(e2, "free_labs", check = FALSE) <- prop(e2, "free_labs") %|w|%
            prop(e1, "free_labs")
        e2
    }

#' @importFrom S7 prop
S7::method(plot_add_scheme, list(ggplot2::class_ggplot, scheme_align)) <-
    function(plot, scheme, ...) {
        if (!is_waiver(free_guides <- prop(scheme, "guides"))) {
            plot <- free_guide(plot, free_guides)
        }
        # by default, we'll attach all labs to the axis
        if (!is.null(free_labs <- prop(scheme, "free_labs") %|w|% "tlbr")) {
            plot <- free_lab(plot, free_labs)
        }
        # by default, we won't remove any spaces
        if (!is.null(free_spaces <- prop(scheme, "free_spaces") %|w|% NULL)) {
            plot <- free_space(free_border(plot, free_spaces), free_spaces)
        }
        plot
    }
