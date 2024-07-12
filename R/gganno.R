#' Heatmap Annotation
#'
#' @param position Possible values are `"top"`, `"left"`, `"bottom"`, and
#' `"right"`.
#' @inheritParams ggplot2::ggplot
#' @importFrom ggplot2 aes
#' @export
gganno <- function(position, mapping = aes(),
                   data = NULL, size = unit(10, "mm"),
                   active = TRUE, name = NULL, order = NULL) {
    position <- match.arg(position, GGHEAT_ELEMENTS)
    assert_string(name, empty_ok = FALSE, null_ok = TRUE)
    assert_bool(active)
    data <- allow_lambda(data)
    methods::new("ggannotation",
        name = name, position = position, active = active,
        data = data, order = order, size = size,
        annotation = ggplot2::ggplot(mapping = mapping)
    )
}

#' @export
#' @keywords internal
plot.ggannotation <- function(x, ...) {
    cli::cli_abort("You cannot plot {.cls ggannotation} object")
}

#' @export
methods::setMethod("show", "ggannotation", function(object) {
    "A ggannotation object"
})

#' @include ggheat.R
#' @keywords internal
methods::setClass(
    "ggannotation",
    contains = "ggheat",
    list(
        name = "ANY",
        position = "character",
        active = "logical",
        data = "ANY",
        order = "integer",
        size = "ANY",
        annotation = "ggplot"
    )
)

#' @export
#' @rdname gganno
gganno_top <- function(...) gganno(position = "top", ...)
anno_top <- gganno_top

#' @export
#' @rdname gganno
gganno_bottom <- function(...) gganno(position = "bottom", ...)
anno_bottom <- gganno_bottom

#' @export
#' @rdname gganno
gganno_left <- function(...) gganno(position = "left", ...)
anno_left <- gganno_left

#' @export
#' @rdname gganno
gganno_right <- function(...) gganno(position = "right", ...)
anno_right <- gganno_right

#' @keywords internal
gganno_setup_data <- function(data, position, heatmap_matrix,
                              object_name) {
    UseMethod("gganno_setup_data")
}

#' @export
gganno_setup_data.data.frame <- function(data, position, heatmap_matrix,
                                         object_name) {
    anno_matrix <- gganno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_gganno_incompatible_data(object_name))
    }
    data
}

#' @export
gganno_setup_data.matrix <- function(data, position, heatmap_matrix,
                                     object_name) {
    anno_matrix <- gganno_data_from_heatmap(position, heatmap_matrix)
    if (nrow(anno_matrix) != NROW(data)) {
        cli::cli_abort(msg_gganno_incompatible_data(object_name))
    }
    melt_matrix(data)
}

#' @export
gganno_setup_data.NULL <- function(data, position, heatmap_matrix,
                                   object_name) {
    data <- gganno_data_from_heatmap(position, heatmap_matrix)
    melt_matrix(data)
}

#' @export
gganno_setup_data.atomic <- function(data, position, heatmap_matrix,
                                     object_name) {
    data <- matrix(matrix, ncol = 1L)
    colnames(data) <- "V1"
    if (rlang::is_named(data)) rownames(data) <- names(matrix)
    gganno_setup_data(data, position, heatmap_matrix, object_name)
}

#' @export
gganno_setup_data.function <- function(data, position, heatmap_matrix,
                                       object_name) {
    anno_matrix <- gganno_data_from_heatmap(position, heatmap_matrix)
    data <- data(anno_matrix)
    gganno_setup_data(data, position, heatmap_matrix, object_name)
}

gganno_data_from_heatmap <- function(position, heatmap_matrix) {
    switch(position,
        top = ,
        bottom = t(heatmap_matrix),
        left = ,
        right = heatmap_matrix
    )
}

msg_gganno_incompatible_data <- function(object_name) {
    sprintf(
        "%s from %s must have %s",
        style_arg("data"), object_name,
        "compatible observations with the heatmap"
    )
}
