#' @keywords internal
layout_heatmap_and_add <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_and_add")
}

#' @export
layout_heatmap_and_add.default <- function(object, heatmap, object_name) {
    cli::cli_abort(
        "Cannot add {.code {object_name}} to heatmap and annotations"
    )
}

#' @export
layout_heatmap_and_add.gg <- function(object, heatmap, object_name) {
    if (is.null(set <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
        set <- HEATMAP_ANNOTATION_POSITION
    }
    for (position in set) {
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
        "Cannot add {.code {object_name}} into the heatmap layout",
        i = "try to use {.fn ggalign} to initialize a {.cls ggplot} object"
    ))
}

#' @export
layout_heatmap_and_add.labels <- layout_heatmap_and_add.gg

#' @export
layout_heatmap_and_add.facetted_pos_scales <- layout_heatmap_and_add.gg

#' @export
layout_heatmap_and_add.NULL <- layout_heatmap_add.NULL
