#' @keywords internal
anno_add <- function(anno, object, object_name) {
    UseMethod("anno_add")
}

#' @export
anno_add.gganno <- function(anno, object, object_name) {
    gganno_add(object, anno, object_name)
}

#' @export
anno_add.htanno <- function(anno, object, object_name) {
    htanno_add(object, anno, object_name)
}

gganno_add <- function(object, anno, object_name) {
    UseMethod("gganno_add")
}

htanno_add <- function(object, anno, object_name) {
    UseMethod("htanno_add")
}

# Following methods are used to add elements to gganno object
#' @export
gganno_add.default <- function(object, anno, object_name) {
    cli::cli_abort(paste(
        "Can't add {.var {object_name}} to a",
        "{.cls gganno} annotation"
    ))
}

#' @export
gganno_add.gg <- function(object, anno, object_name) {
    slot(anno, "plot") <- ggplot2::ggplot_add(
        object, slot(anno, "plot"), object_name
    )
    anno
}

# htanno will have sub-class, so we'll re-dispatch to call `htanno` add method
#' @export
htanno_add.default <- function(object, anno, object_name) {
    slot(anno, "htanno")$add(object, object_name)
    anno
}

#' @export
gganno_add.facetted_pos_scales <- function(object, anno, object_name) {
    slot(anno, "facetted_pos_scales") <- object
    anno
}

#' @export
htanno_add.facetted_pos_scales <- gganno_add.facetted_pos_scales
