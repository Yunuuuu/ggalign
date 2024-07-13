#' @keywords internal
anno_setup_data <- function(data, position, heatmap_matrix,
                            object_name) {
    UseMethod("anno_setup_data")
}

#' @export
anno_setup_data.data.frame <- function(data, position, heatmap_matrix,
                                       object_name) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name))
    }
    data
}

#' @export
anno_setup_data.matrix <- function(data, position, heatmap_matrix,
                                   object_name) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name))
    }
    data
}

#' @export
anno_setup_data.NULL <- function(data, position, heatmap_matrix,
                                 object_name) {
    anno_data_from_heatmap(position, heatmap_matrix)
}

#' @export
anno_setup_data.atomic <- function(data, position, heatmap_matrix,
                                   object_name) {
    data <- matrix(matrix, ncol = 1L)
    colnames(data) <- "V1"
    if (rlang::is_named(data)) rownames(data) <- names(matrix)
    anno_setup_data(data, position, heatmap_matrix, object_name)
}

#' @export
anno_setup_data.function <- function(data, position, heatmap_matrix,
                                     object_name) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    data <- data(anno_matrix)
    anno_setup_data(data, position, heatmap_matrix, object_name)
}

anno_data_from_heatmap <- function(position, heatmap_matrix) {
    switch_position(position, heatmap_matrix, t(heatmap_matrix))
}

msg_anno_incompatible_data <- function(object_name) {
    sprintf(
        "%s from %s must have %s",
        style_arg("data"), object_name,
        "compatible observations with the heatmap"
    )
}
