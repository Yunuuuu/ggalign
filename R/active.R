#' Plot Adding Settings
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' These settings control the behavior of the plot when added to a layout, as
#' well as the arrangement of individual plot areas within the layout. By
#' default, the active context is set only for functions that add plot areas.
#' This approach allows users to seamlessly add other `ggplot2` elements—such as
#' `geoms`, `stats`, `scales`, or `themes`—to the current plot area. The default
#' ordering of plot areas is from top to bottom or from left to right, depending
#' on the layout orientation. However, users can customize this order using the
#' `order` argument.
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
        assert_string(name, empty_ok = FALSE, na_ok = TRUE, null_ok = FALSE)
    }
    new_active(order, use, name)
}

new_active <- function(order, use, name) {
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

deprecate_active <- function(active, fun,
                             set_context = deprecated(),
                             order = deprecated(), name = deprecated(),
                             call = caller_call()) {
    if (lifecycle::is_present(set_context)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(set_context)", fun),
            sprintf("%s(active)", fun)
        )
        assert_bool(set_context, call = call)
        active["use"] <- list(set_context)
    }
    if (lifecycle::is_present(order)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(order)", fun),
            sprintf("%s(active)", fun)
        )
        order <- check_order(order, call = call)
        active["order"] <- list(order)
    }
    if (lifecycle::is_present(name)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(name)", fun),
            sprintf("%s(active)", fun)
        )
        assert_string(name, empty_ok = FALSE, na_ok = TRUE, call = call)
        active["name"] <- list(name)
    }
    active
}
