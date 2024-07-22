####################################################################
# add annotation into annotation list
new_annotations <- function(list, active) {
    structure(list, active = active, class = c("annotations", "ggheat"))
}

#' @keywords internal
annotations_add <- function(object, annotations, object_name) {
    UseMethod("annotations_add")
}

#' @export
annotations_add.default <- function(object, annotations,
                                    object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} into annotations")
}

#' @export
annotations_add.HtannoProto <- function(object, annotations, object_name) {
    # add annotation -------------------------------------
    new <- list(object)

    # check annotation name is unique --------------------
    if (!is.null(name <- .subset2(object, "name"))) {
        if (any(rlang::names2(annotations) == name)) {
            cli::cli_warn(paste(
                "{object_name}: {name} annotation is already present",
                "in the {position} annotation of the heatmap"
            ))
            annotations[[name]] <- NULL
        }
        names(new) <- name
    }
    if (.subset(.subset2(object, "set_context"), 2L)) {
        anno_active <- length(annotations) + 1L
    } else {
        anno_active <- get_context(annotations)
    }
    new_annotations(c(annotations, new), anno_active)
}

#######################################################
# used to add elements for heatmap
#' @keywords internal
heatmap_add <- function(object, heatmap, object_name) UseMethod("heatmap_add")

#' @export
heatmap_add.gg <- function(object, heatmap, object_name) {
    heatmap@heatmap <- ggplot2::ggplot_add(
        object, slot(heatmap, "heatmap"), object_name
    )
    heatmap
}

#' @export
heatmap_add.labels <- heatmap_add.gg

#' @export
heatmap_add.facetted_pos_scales <- function(object, heatmap, object_name) {
    slot(heatmap, "facetted_pos_scales") <- object
    heatmap
}

####################################################################
# Following methods are used to add elements to `htanno` object
htanno_add <- function(object, htanno, object_name) UseMethod("htanno_add")

#' @export
htanno_add.gg <- function(object, htanno, object_name) {
    if (is.null(plot <- .subset2(htanno, "plot"))) {
        cli::cli_abort(paste(
            "Can't add {.var {object_name}} to a",
            "{.fn {snake_class(htanno)}} annotation"
        ), call = .subset2(htanno, "call"))
    }
    htanno$plot <- ggplot2::ggplot_add(object, plot, object_name)
    htanno
}

#' @export
htanno_add.labels <- htanno_add.gg

#' @export
htanno_add.facetted_pos_scales <- function(object, htanno, object_name) {
    htanno$facetted_pos_scales <- object
    htanno
}
