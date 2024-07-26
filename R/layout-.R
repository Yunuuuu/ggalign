# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#
#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @keywords internal
methods::setClass("Layout")

# ggalign has three main layouts ----------------------------
# Used to place multiple objects in two axis ----------------
# Not implemented yet
#' @keywords internal
methods::setClass(
    "LayoutGrid",
    contains = "Layout",
    list(panel_list = "list", index_list = "list")
)

# Used to place multiple objects in one axis ----------------
# usually the heatmap annotations
#' @keywords internal
methods::setClass(
    "LayoutStack",
    contains = "Layout",
    list(
        plots = "list",
        active = "ANY",
        direction = "character",
        panels = "ANY",
        index = "ANY"
    )
)

# used to create the heatmap layout -------------------------
#' @keywords internal
methods::setClass(
    "LayoutHeatmap",
    contains = "Layout",
    list(
        data = "matrix",
        plot = "ANY",
        facetted_pos_scales = "ANY",
        params = "list",
        # Used by the layout
        top = "ANY", left = "ANY",
        bottom = "ANY", right = "ANY",
        panel_list = "list", index_list = "list",
        active = "ANY"
    ),
    prototype = list(
        top = NULL, left = NULL,
        bottom = NULL, right = NULL,
        panel_list = NULL, index_list = NULL,
        active = NULL
    )
)
