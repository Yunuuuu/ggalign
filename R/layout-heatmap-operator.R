###############################################################
#' @keywords internal
layout_heatmap_subtract <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_subtract")
}

#' @export
layout_heatmap_subtract.default <- function(object, heatmap, object_name) {
    if (is.null(position <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
        return(heatmap)
    }
    slot(heatmap, position) <- layout_stack_subtract(
        object, slot(heatmap, position), object_name
    )
    heatmap
}

###############################################################
#' @keywords internal
layout_heatmap_and_add <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_and_add")
}

#' @export
layout_heatmap_and_add.default <- function(object, heatmap, object_name) {
    heatmap <- heatmap_add(object, heatmap, object_name)
    for (position in .TLBR) {
        stack <- slot(heatmap, position)
        if (is.null(stack)) next
        slot(heatmap, position) <- layout_stack_and_add(
            object, stack, object_name
        )
    }
    heatmap
}

#' @export
layout_heatmap_and_add.ggplot <- function(object, heatmap, object_name) {
    cli::cli_abort(c(
        "Cannot add {.var {object_name}} into the heatmap layout",
        i = "try to use {.fn ggalign} to initialize the {.cls ggplot}"
    ))
}
