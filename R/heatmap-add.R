# used to add elements for heatmap
#' @keywords internal
heatmap_add <- function(object, plot, object_name) UseMethod("heatmap_add")

#' @export
heatmap_add.default <- function(object, plot, object_name) {
    cli::cli_abort(
        "Can't add {.var {object_name}} to a {.cls ggheatmap} object"
    )
}

#' @export
heatmap_add.gg <- function(object, plot, object_name) {
    plot@heatmap <- ggplot2::ggplot_add(
        object, slot(plot, "heatmap"), object_name
    )
    plot
}

#' @export
heatmap_add.facetted_pos_scales <- function(object, plot, object_name) {
    slot(plot, "facetted_pos_scales") <- object
    plot
}

#' @export
heatmap_add.CoordFlip <- function(object, plot, object_name) {
    cli::cli_abort(
        "Can't add {.var {object_name}} to a {.cls ggheatmap} object"
    )
}
