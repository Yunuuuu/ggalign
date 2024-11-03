#' Plot context settings
#'
#' @description
#' `r lifecycle::badge('experimental')` These settings control the arrangement
#' and behavior of individual plot areas within the layout. By default, the
#' active context is set only for functions that add plot areas. This approach
#' allows users to seamlessly add other `ggplot2` elements—such as `geoms`,
#' `stats`, `scales`, or `themes`—to the current plot area. The default ordering
#' of plot areas is from top to bottom or from left to right, depending on
#' layout orientation. However, users can customize this order using the `order`
#' argument.
#'
#' @param order An integer specifying the order of the plot area within the
#'   layout.
#' @param active A logical (`TRUE`/`FALSE`) indicating whether to set the
#'   active context to the current plot area when added to a layout. If `TRUE`,
#'   any subsequent `ggplot` elements will be applied to this active plot area.
#' @param name A string specifying the plot's name, useful for switching active
#'   contexts through the `what` argument in functions like
#'   [`quad_anno()`]/[`stack_switch()`].
#' @export
context <- function(order = waiver(), active = waiver(), name = waiver()) {
    if (!is.waive(order)) order <- check_order(order)
    if (!is.waive(active)) assert_bool(active)
    if (!is.waive(name)) {
        assert_string(name, empty_ok = FALSE, na_ok = TRUE, null_ok = FALSE)
    }
    new_context(order, active, name)
}

new_context <- function(order, active, name) {
    structure(
        list(order = order, active = active, name = name),
        class = "ggalign_context"
    )
}

#' @importFrom utils modifyList
update_context <- function(context, default) {
    if (is.null(context)) return(default) # styler: off
    modifyList(default,
        context[!vapply(context, is.waive, logical(1L), USE.NAMES = FALSE)],
        keep.null = TRUE
    )
}

deprecate_context <- function(context, fun,
                              set_context = deprecated(),
                              order = deprecated(), name = deprecated(),
                              call = caller_call()) {
    if (lifecycle::is_present(set_context)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(set_context)", fun),
            sprintf("%s(context)", fun)
        )
        assert_bool(set_context, call = call)
        context["active"] <- list(set_context)
    }
    if (lifecycle::is_present(order)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(order)", fun),
            sprintf("%s(context)", fun)
        )
        order <- check_order(order, call = call)
        context["order"] <- list(order)
    }
    if (lifecycle::is_present(name)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(name)", fun),
            sprintf("%s(context)", fun)
        )
        assert_string(name, empty_ok = FALSE, na_ok = TRUE, call = call)
        context["name"] <- list(name)
    }
    context
}
