####################################################################
# add annotation into annotation list
layout_stack <- function(direction = c("horizontal", "vertial"), data) {
    direction <- match.arg(direction)
    methods::new(
        "LayoutStack",
        plots = list(),
        active = NULL,
        direction = direction
    )
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
