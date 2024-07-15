new_annotations <- function(list, active) {
    structure(list, active = active, class = c("annotations", "ggheat"))
}

#' @keywords internal
annotations_add <- function(object, annotations, set_context, object_name) {
    UseMethod("annotations_add")
}

#' @export
annotations_add.default <- function(object, annotations, set_context,
                                    object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} into annotations")
}

#' @export
annotations_add.anno <- function(object, annotations, set_context,
                                 object_name) {
    annotations <- annotations %||% list()

    # add annotation -------------------------------------
    new <- list(object)

    # check annotation name is unique --------------------
    if (!is.null(name <- slot(object, "name"))) {
        if (any(rlang::names2(annotations) == name)) {
            cli::cli_warn(paste(
                "{object_name}: {name} annotation is already present",
                "in the {position} annotation of the heatmap"
            ))
            annotations[[name]] <- NULL
        }
        names(new) <- name
    }
    if (set_context) {
        anno_active <- length(annotations) + 1L
    } else {
        anno_active <- get_context(annotations)
    }
    new_annotations(c(annotations, new), anno_active)
}

annotations_build <- function(annotations, panels, index, scales,
                              facet, position) {
    plots <- lapply(annotations, function(x) {
        # we merge anno `labels` and `labels_nudge`
        # we also extract the expand from the heatmap
        default_scales <- ggheat_default_scale(
            to_coord_axis(position),
            panels = panels, index = index,
            labels = slot(x, "labels"),
            nudge = slot(x, "labels_nudge"),
            lapply(scales, function(scale) scale$expand)
        )
        anno_build(
            x = x,
            panels = panels,
            index = index,
            scales = default_scales,
            facet = facet,
            position = position
        )
    })

    # we reorder annotation based on the `order` slot
    index <- order(vapply(annotations, slot, integer(1L), name = "order"))
    sizes <- lapply(annotations, slot, name = "size")
    sizes <- do.call(unit.c, sizes[index])
    plots <- .subset(plots, index)

    # combine all annotations
    keep <- lengths(plots) > 0L
    plots <- .subset(plots, keep)
    if (length(plots) == 0L) {
        return(list(NULL, NULL))
    }
    sizes <- sizes[keep]
    list(plots, sizes)
}
