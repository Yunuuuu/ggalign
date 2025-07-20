#' Plot Adding Context Settings
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' These settings control the behavior of the plot when added to a layout, as
#' well as the arrangement of individual plot areas within the layout.
#'
#' @details
#' By default, the active context is set only for functions that add plot areas.
#' This allows other `ggplot2` elements-such as `geoms`, `stats`, `scales`, or
#' `themes`- to be seamlessly added to the current plot area.
#'
#' The default ordering of the plot areas is from top to bottom or from left to
#' right, depending on the layout orientation. However, users can customize this
#' order using the `order` argument.
#'
#' @param order An integer specifying the order of the plot area within the
#'   layout.
#' @param use A logical (`TRUE`/`FALSE`) indicating whether to set the
#'   active context to the current plot when added to a layout. If `TRUE`,
#'   any subsequent `ggplot` elements will be applied to this plot.
#' @param name A string specifying the plot's name, useful for switching active
#'   contexts through the `what` argument in functions like
#'   [`quad_anno()`]/[`stack_switch()`].
#' @importFrom rlang is_na
#' @export
active <- S7::new_class("active",
    properties = list(
        order = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                # Validator runs *after* the value passes the class check
                # Ensure it's a single integer value
                if (length(value) != 1L) {
                    return("must be a single integer value")
                }
            },
            setter = function(self, value) {
                # Custom setter logic to bypass validator using attributes
                if (is.null(value) || is_na(value)) {
                    attr(self, "order") <- NA_integer_
                } else if (.rlang_check_number(value, allow_decimal = FALSE) ==
                           0L) { # styler: off
                    attr(self, "order") <- as.integer(value)
                } else {
                    self@order <- value
                }
                self
            },
            default = NA_integer_
        ),
        use = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
            },
            default = NA
        ),
        name = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            default = NA_character_
        )
    )
)

#' @importFrom S7 S7_inherits
is_active <- function(x) S7_inherits(x, active)

#' @importFrom S7 props
S7::method(`+`, list(active, active)) <- function(e1, e2) {
    prop_list <- props(e2)
    props(e1) <- prop_list[
        !vapply(prop_list, is.na, logical(1L), USE.NAMES = FALSE)
    ]
    e1
}

S7::method(`+`, list(active, S7::class_any)) <- function(e1, e2) {
    if (is.null(e2)) {
        return(e1)
    }
    stop_incompatible_op("+", e1, e2)
}

S7::method(`+`, list(S7::class_any, active)) <- function(e1, e2) {
    if (is.null(e1)) {
        return(e2)
    }
    stop_incompatible_op("+", e1, e2)
}
