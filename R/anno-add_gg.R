anno_add_gg <- function(annotation, object, object_name) {
    UseMethod("anno_add_gg")
}

#' @export
anno_add_gg.ggannotation <- function(annotation, object, object_name) {
    slot(annotation, "plot") <- ggplot2::ggplot_add(
        object, slot(annotation, "plot"), object_name
    )
    annotation
}

#' @export
anno_add_gg.htanno <- function(annotation, object, object_name) {
    slot(annotation, "htanno")$add_gg(object, object_name)
    annotation
}
