# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#
#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @keywords internal
methods::setClass("Layout", list(active = "ANY"),
    prototype = list(active = NULL)
)

is.layout <- function(x) methods::is(x, "Layout")

#' Build `Layout` object for rendering.
#'
#' @param layout A `Layout` object.
#' @examples
#' build_patchwork(ggheatmap(matrix(rnorm(100L), nrow = 10L)))
#' @export
#' @return A `patchwork` object.
build_patchwork <- function(layout) UseMethod("build_patchwork")

#' @export
build_patchwork.default <- function(layout) {
    cli::cli_abort("{.arg x} must be a {.cls Layout} object")
}

# ** Not implemented yet
#' @keywords internal
# methods::setClass(
#     "LayoutGrid",
#     contains = "Layout",
#     list(panel_list = "list", index_list = "list")
# )

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
