# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#
#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @keywords internal
methods::setClass("Layout",
    list(
        active = "ANY", plot_data = "ANY",
        `_namespace` = "ANY"
    ),
    prototype = list(active = NULL, `_namespace` = function() NULL)
)

is.layout <- function(x) methods::is(x, "Layout")

#' Print Layout object
#'
#' @param object A [layout_heatmap()] or [layout_stack()] object.
#' @export
methods::setMethod("show", "Layout", function(object) {
    print(object)
})

#############################################################
#' Add components to `Layout`
#'
#' @param e1 A [layout_heatmap()] or [layout_stack()] object.
#' @param e2 An object to be added to the plot, including [gg][ggplot2::+.gg]
#' elements or [align] object.
#' @return A modified `Layout` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("t") +
#'     ggalign() +
#'     geom_point(aes(y = value))
#' @name layout-add
#' @aliases +.Layout +.LayoutHeatmap +.ggheatmap +.LayoutStack +.ggstack
NULL

#' @rdname layout-add
#' @export
methods::setMethod("+", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_add(e1, e2, e2name)
})

#' @keywords internal
layout_add <- function(layout, object, object_name) {
    UseMethod("layout_add")
}

#' @export
layout_add.LayoutHeatmap <- function(layout, object, object_name) {
    layout_heatmap_add(object, layout, object_name)
}

#' @export
layout_add.LayoutStack <- function(layout, object, object_name) {
    layout_stack_add(object, layout, object_name)
}

#########################################################
#' Add components to all plots
#'
#' @param e1 A [layout_heatmap()] or [layout_stack()] object.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @name layout-and
#' @aliases &.Layout &.LayoutHeatmap &.ggheatmap &.LayoutStack &.ggstack
NULL

#' @rdname layout-and
#' @export
methods::setMethod("&", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_and_add(e1, e2, e2name)
})

#' @keywords internal
layout_and_add <- function(layout, object, object_name) {
    UseMethod("layout_and_add")
}

#' @export
layout_and_add.LayoutHeatmap <- function(layout, object, object_name) {
    layout_heatmap_and_add(object, layout, object_name)
}

#' @export
layout_and_add.LayoutStack <- function(layout, object, object_name) {
    layout_stack_and_add(object, layout, object_name)
}

#########################################################
# utils function ----------------------------------------
get_panels <- function(x, ...) UseMethod("get_panels")

#' @importFrom methods slot
#' @export
get_panels.LayoutHeatmap <- function(x, axis, ...) {
    .subset2(slot(x, "panel_list"), axis)
}

#' @importFrom methods slot
#' @export
get_panels.LayoutStack <- function(x, ...) {
    slot(x, "panels")
}

set_panels <- function(x, ...) UseMethod("set_panels")

#' @export
set_panels.NULL <- function(x, ...) x

#' @export
set_panels.Align <- function(x, ...) x

#' @importFrom methods slot slot<-
#' @export
set_panels.LayoutHeatmap <- function(x, axis, panels, ...) {
    slot(x, "panel_list")[[axis]] <- panels
    if (axis == "x") {
        slot(x, "top") <- set_panels(slot(x, "top"), panels)
        slot(x, "bottom") <- set_panels(slot(x, "bottom"), panels)
    } else {
        slot(x, "left") <- set_panels(slot(x, "left"), panels)
        slot(x, "right") <- set_panels(slot(x, "right"), panels)
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_panels.LayoutStack <- function(x, panels, ...) {
    slot(x, "panels") <- panels
    axis <- to_coord_axis(slot(x, "direction"))
    slot(x, "plots") <- lapply(slot(x, "plots"),
        set_panels,
        axis = axis, panels = panels
    )
    x
}

get_index <- function(x, ...) UseMethod("get_index")

#' @importFrom methods slot
#' @export
get_index.LayoutHeatmap <- function(x, axis, ...) {
    .subset2(slot(x, "index_list"), axis)
}

#' @importFrom methods slot
#' @export
get_index.LayoutStack <- function(x, ...) {
    slot(x, "index")
}

set_index <- function(x, ...) UseMethod("set_index")

#' @export
set_index.NULL <- function(x, ...) x

#' @export
set_index.Align <- function(x, ...) x

#' @importFrom methods slot slot<-
#' @export
set_index.LayoutHeatmap <- function(x, axis, index, ...) {
    slot(x, "index_list")[[axis]] <- index
    if (axis == "x") {
        slot(x, "top") <- set_index(slot(x, "top"), index)
        slot(x, "bottom") <- set_index(slot(x, "bottom"), index)
    } else {
        slot(x, "left") <- set_index(slot(x, "left"), index)
        slot(x, "right") <- set_index(slot(x, "right"), index)
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_index.LayoutStack <- function(x, index, ...) {
    slot(x, "index") <- index
    axis <- to_coord_axis(slot(x, "direction"))
    slot(x, "plots") <- lapply(slot(x, "plots"),
        set_index,
        axis = axis, index = index
    )
    x
}
