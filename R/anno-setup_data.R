#' @keywords internal
anno_setup_data <- function(data, position, heatmap_matrix,
                            object_name, call) {
    UseMethod("anno_setup_data")
}

#' @export
anno_setup_data.data.frame <- function(data, position, heatmap_matrix,
                                       object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name), call = call)
    }
    data
}

#' @export
anno_setup_data.matrix <- function(data, position, heatmap_matrix,
                                   object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name), call = call)
    }
    data
}

#' @export
anno_setup_data.NULL <- function(data, position, heatmap_matrix,
                                 object_name, call) {
    anno_data_from_heatmap(position, heatmap_matrix)
}

#' @export
anno_setup_data.numeric <- function(data, position, heatmap_matrix,
                                    object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_anno_incompatible_data(object_name), call = call)
    }
    ans <- matrix(data, ncol = 1L)
    colnames(ans) <- "V1"
    if (rlang::is_named(data)) rownames(ans) <- names(data)
    ans
}

#' @export
anno_setup_data.character <- anno_setup_data.numeric

#' @export
anno_setup_data.function <- function(data, position, heatmap_matrix,
                                     object_name, call) {
    anno_matrix <- anno_data_from_heatmap(position, heatmap_matrix)
    fn <- data
    # fix R CMD check note: Found the following calls to data() loading into the
    # global environment
    data <- fn(anno_matrix)
    anno_setup_data(data, position, heatmap_matrix, object_name, call = call)
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
