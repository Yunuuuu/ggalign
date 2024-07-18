#' @keywords internal
anno_and_add <- function(anno, object, object_name) {
    UseMethod("anno_and_add")
}

#' @export
anno_and_add.gganno <- function(anno, object, object_name) {
    gganno_and_add(object, anno, object_name)
}

#' @export
anno_and_add.htanno <- function(anno, object, object_name) {
    htanno_and_add(object, anno, object_name)
}

gganno_and_add <- function(object, anno, object_name) {
    UseMethod("gganno_and_add")
}

htanno_and_add <- function(object, anno, object_name) {
    UseMethod("htanno_and_add")
}

#' @export
gganno_and_add.facetted_pos_scales <- gganno_add.facetted_pos_scales

#' @export
htanno_and_add.facetted_pos_scales <- gganno_and_add.facetted_pos_scales

# htanno will have sub-class, so we'll re-dispatch to call `htanno` and method
#' @export
htanno_and_add.default <- function(object, anno, object_name) {
    slot(anno, "htanno")$and(object, object_name)
    anno
}

#' @export
gganno_and_add.gg <- gganno_add.gg

#' @export
gganno_and_add.labels <- gganno_add.gg
