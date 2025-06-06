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
#' @export
active <- function(order = waiver(), use = waiver(), name = waiver()) {
    if (!is.waive(order)) order <- check_order(order)
    if (!is.waive(use)) assert_bool(use)
    if (!is.waive(name)) {
        assert_string(name,
            empty_ok = FALSE, allow_na = TRUE,
            allow_null = FALSE
        )
    }
    new_active(order = order, use = use, name = name)
}

# for internal function, we only adjust to the `use` argument
# here, we put it in the first
new_active <- function(use, order = NA_integer_, name = NA_character_) {
    structure(
        list(order = order, use = use, name = name),
        class = "ggalign_active"
    )
}

#' @importFrom utils modifyList
update_active <- function(active, default) {
    if (is.null(active)) return(default) # styler: off
    modifyList(default,
        active[!vapply(active, is.waive, logical(1L), USE.NAMES = FALSE)],
        keep.null = TRUE
    )
}
