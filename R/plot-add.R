#' Add components to `ggheatmap`
#'
#' @param e1 A [ggheatmap][ggheat] object.
#' @param e2 An object to be added to the plot, including
#' [gg][ggplot2::+.gg] elements, [gganno][gganno] object, or [htanno]
#' object.
#' @return A modified `ggheatmap` object.
#' @examples
#' ggheat(matrix(rnorm(81), nrow = 9)) +
#'     gganno(position = "top") +
#'     geom_point(aes(y = value))
#' @name ggheatmap-add
#' @aliases +.ggheatmap
#' @seealso ggheatmap_add
NULL

#' @rdname ggheatmap-add
#' @export
methods::setMethod("+", c("ggheatmap", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    ggheatmap_add(e2, e1, e2name)
})

#' Add custom objects to `ggheatmap`
#'
#' @param heatmap A `ggheatmap` object
#' @inheritParams ggplot2::ggplot_add
#' @inherit ggheatmap-add return
#' @examples
#' ggheatmap_add(
#'     geom_point(aes(y = value)),
#'     ggheat(matrix(rnorm(81), nrow = 9)),
#'     deparse(quote(geom_point(aes(y = value))))
#' )
#' @export
ggheatmap_add <- function(object, heatmap, object_name) {
    UseMethod("ggheatmap_add")
}

#' @export
ggheatmap_add.default <- function(object, heatmap, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} to {.cls ggheatmap}")
}

#' @export
ggheatmap_add.NULL <- function(object, heatmap, object_name) heatmap

#' @export
ggheatmap_add.HtannoProto <- function(object, heatmap, object_name) {
    position <- .subset2(object, "position")
    if (is.null(position)) {
        if (is.null(position <- get_context(heatmap))) {
            cli::cli_abort(c(
                "No active annotation",
                i = paste(
                    "try to provide {.arg position} argument in",
                    "{.code {object_name}}"
                )
            ), call = .subset2(object, "call"))
        }
        object$position <- position
    }

    # setting active position for the plot ---------------
    if (.subset(.subset2(object, "set_context"), 1L)) {
        heatmap <- set_context(heatmap, position)
    }

    # initialize annotation -----------------------------
    # this step the annotation will act with the heatmap
    # group heatmap into panels or reorder heatmap rows/columns
    layout <- initialize_htanno(object, heatmap, object_name)
    axis <- to_matrix_axis(position)
    slot(heatmap, paste0(axis, "_panels")) <- .subset2(layout, 1L)
    slot(heatmap, paste0(axis, "_index")) <- .subset2(layout, 2L)

    # add annotation -----------------------------
    annotations <- slot(heatmap, position) %||% new_annotations(list(), NULL)
    slot(heatmap, position) <- annotations_add(object, annotations, object_name)
    heatmap
}

#' @export
ggheatmap_add.active <- function(object, heatmap, object_name) {
    if (object == "heatmap") {
        set_context(heatmap, NULL)
    } else {
        set_context(heatmap, object)
    }
}

#' @export
ggheatmap_add.list <- function(object, heatmap, object_name) {
    for (o in object) {
        heatmap <- heatmap + o
    }
    heatmap
}

#############################################################
# Add elements for heatmap or annotation
#' @importFrom methods slot slot<-
#' @export
ggheatmap_add.gg <- function(object, heatmap, object_name) {
    # if no active context, we directly add it into the main heatmap
    if (is.null(position <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
        # we check if annotation has been initialized
    } else if (is.null(annotations <- slot(heatmap, position)) ||
        is.null(active_anno <- get_context(annotations))) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}} to {position} annotation",
            i = "Did you forget to initialize it with {.fn gganno_{position}}?"
        ))
    } else {
        anno <- .subset2(annotations, active_anno)
        annotations[[active_anno]] <- htanno_add(object, anno, object_name)
        slot(heatmap, position) <- annotations
    }
    heatmap
}

#' @export
ggheatmap_add.labels <- ggheatmap_add.gg

#' @export
ggheatmap_add.facetted_pos_scales <- ggheatmap_add.gg

##############################################################
# Preventing from adding following elements
#' @export
ggheatmap_add.CoordFlip <- function(object, heatmap, object_name) {
    cli::cli_abort("Can't flip axis in {.cls ggheatmap} object")
}

#' @export
ggheatmap_add.matrix <- function(object, heatmap, object_name) {
    cli::cli_abort("Can't change data in {.cls ggheatmap} object")
}

#' @export
ggheatmap_add.data.frame <- ggheatmap_add.matrix
