#' @keywords internal
anno_add <- function(anno, object, object_name) {
    UseMethod("anno_add")
}

gganno_add <- function(object, anno, object_name) {
    UseMethod("gganno_add")
}

htanno_add <- function(object, anno, object_name) {
    UseMethod("htanno_add")
}

#' @export
anno_add.gganno <- function(anno, object, object_name) {
    gganno_add(object, anno, object_name)
}

#' @export
anno_add.htanno <- function(anno, object, object_name) {
    htanno_add(object, anno, object_name)
}

#' @export
gganno_add.default <- function(object, anno, object_name) {
    cli::cli_abort(
        "Can't add {.var {object_name}} to a {.cls ggheatmap} annotation"
    )
}

#' @export
htanno_add.default <- gganno_add.default

#' @export
gganno_add.gg <- function(object, anno, object_name) {
    slot(anno, "plot") <- ggplot2::ggplot_add(
        object, slot(anno, "plot"), object_name
    )
    anno
}

#' @export
htanno_add.gg <- function(object, anno, object_name) {
    slot(anno, "htanno")$add_gg(object, object_name)
    anno
}

#' @export
gganno_add.facetted_pos_scales <- function(object, anno, object_name) {
    slot(anno, "facetted_pos_scales") <- object
    anno
}

#' @export
htanno_add.facetted_pos_scales <- gganno_add.facetted_pos_scales
