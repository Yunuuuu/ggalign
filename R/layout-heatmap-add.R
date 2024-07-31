#' Add components to `LayoutHeatmap`
#'
#' @param e1 A [LayoutHeatmap][layout_heatmap] object.
#' @param e2 An object to be added to the plot, including
#' [gg][ggplot2::+.gg] elements, [gganno][gganno] object, or [htanno]
#' object.
#' @return A modified `LayoutHeatmap` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("t") +
#'     gganno() +
#'     geom_point(aes(y = value))
#' @name heatmap-add
#' @aliases +.heatmap
#' @seealso layout_heatmap_add
NULL

#' @rdname heatmap-add
#' @export
methods::setMethod("+", c("LayoutHeatmap", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_heatmap_add(e2, e1, e2name)
})

#' Add custom objects to `LayoutHeatmap`
#'
#' @param heatmap A `LayoutHeatmap` object
#' @inheritParams ggplot2::ggplot_add
#' @inherit heatmap-add return
#' @examples
#' layout_heatmap_add(
#'     geom_point(aes(y = value)),
#'     ggheatmap(matrix(rnorm(81), nrow = 9))
#' )
#' @export
layout_heatmap_add <- function(object, heatmap,
                               object_name = deparse(substitute(object))) {
    UseMethod("layout_heatmap_add")
}

#' @export
layout_heatmap_add.default <- function(object, heatmap, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} to {.cls ggheatmap}")
}

#' @export
layout_heatmap_add.NULL <- function(object, heatmap, object_name) heatmap

#' @export
layout_heatmap_add.Align <- function(object, heatmap, object_name) {
    if (is.null(position <- get_context(heatmap))) {
        cli::cli_abort(c("No active heatmap annotation",
            i = "try to activate an annotation with {.fn hmanno}"
        ), call = .subset2(object, "call"))
    }
    direction <- to_direction(position)
    # we'll always override the direction of `Align` object when adding it into
    # a layout object.
    object$direction <- direction

    # add annotation -----------------------------
    stack <- layout_stack_add(object, slot(heatmap, position), object_name)
    slot(heatmap, position) <- stack

    # set the panels and index for the heatmap
    axis <- to_coord_axis(direction)
    heatmap <- set_panels(heatmap, axis, get_panels(stack))
    get_panels(heatmap, axis)
    heatmap <- set_index(heatmap, axis, get_index(stack))
    heatmap
}

#' @export
layout_heatmap_add.heatmap_active <- function(object, heatmap, object_name) {
    size <- attr(object, "size")
    if (is.na(object)) object <- get_context(heatmap) %||% "plot"
    if (object == "plot") {
        return(set_context(heatmap, NULL))
    }
    heatmap <- set_context(heatmap, object)
    # initialize the annotation stack ------------
    if (is.null(stack <- slot(heatmap, object))) {
        direction <- to_direction(object)
        axis <- to_coord_axis(direction)
        params <- slot(heatmap, "params")
        stack <- layout_stack(
            switch_direction(
                direction,
                slot(heatmap, "data"),
                t(slot(heatmap, "data"))
            ),
            direction = direction,
            labels = .subset2(params, paste0(axis, "labels")),
            labels_nudge = .subset2(params, paste0(axis, "labels_nudge")),
            guides = NULL
        )
        stack <- set_panels(stack, get_panels(heatmap, axis))
        stack <- set_index(stack, get_index(heatmap, axis))
    }
    if (!is.null(size)) slot(stack, "size") <- size
    slot(heatmap, object) <- stack
    heatmap
}

#' @export
layout_heatmap_add.list <- function(object, heatmap, object_name) {
    for (o in object) {
        heatmap <- heatmap + o
    }
    heatmap
}

#############################################################
# Add elements for heatmap or annotation
#' @importFrom methods slot slot<-
#' @export
layout_heatmap_add.gg <- function(object, heatmap, object_name) {
    # if no active context, we directly add it into the main heatmap
    if (is.null(position <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
    } else {
        slot(heatmap, position) <- layout_stack_add_gg(
            object, slot(heatmap, position), object_name,
            sprintf("%s annotation", position)
        )
    }
    heatmap
}

#' @export
layout_heatmap_add.labels <- layout_heatmap_add.gg

#' @export
layout_heatmap_add.facetted_pos_scales <- layout_heatmap_add.gg

##############################################################
# Preventing from adding following elements
#' @export
layout_heatmap_add.CoordFlip <- function(object, heatmap, object_name) {
    cli::cli_abort("Can't flip axis in {.cls ggheatmap} object")
}

#' @export
layout_heatmap_add.matrix <- function(object, heatmap, object_name) {
    cli::cli_abort("Can't change data in {.cls ggheatmap} object")
}

#' @export
layout_heatmap_add.data.frame <- layout_heatmap_add.matrix

#######################################################
# used to add elements for heatmap
#' @keywords internal
heatmap_add <- function(object, heatmap, object_name) UseMethod("heatmap_add")

#' @export
heatmap_add.gg <- function(object, heatmap, object_name) {
    heatmap@plot <- ggplot2::ggplot_add(
        object, slot(heatmap, "plot"), object_name
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
