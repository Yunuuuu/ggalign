# Will ensure serialisation includes a link to the patchwork namespace
# Copied from patchwork
ggalign_namespace_link <- function() NULL

# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
#
#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @keywords internal
methods::setClass("Layout",
    list(active = "ANY", `_namespace` = "ANY"),
    prototype = list(active = NULL, `_namespace` = ggalign_namespace_link)
)

is.layout <- function(x) methods::is(x, "Layout")

#' Print Layout object
#'
#' @param object A [layout_heatmap()] or [layout_stack()] object.
#' @importFrom methods show
#' @export
#' @keywords internal
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
#' @aliases +.Layout +.HeatmapLayout +.ggheatmap +.StackLayout +.ggstack
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
layout_add.HeatmapLayout <- function(layout, object, object_name) {
    layout_heatmap_add(object, layout, object_name)
}

#' @export
layout_add.StackLayout <- function(layout, object, object_name) {
    layout_stack_add(object, layout, object_name)
}

#########################################################
#' Add components to all plots
#'
#' @param e1 A [layout_heatmap()] or [layout_stack()] object.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @name layout-and
#' @aliases &.Layout &.HeatmapLayout &.ggheatmap &.StackLayout &.ggstack
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
layout_and_add.HeatmapLayout <- function(layout, object, object_name) {
    layout_heatmap_and_add(object, layout, object_name)
}

#' @export
layout_and_add.StackLayout <- function(layout, object, object_name) {
    layout_stack_and_add(object, layout, object_name)
}

############################################################
# layout should be one of index, nobs, panel
get_layout <- function(x, layout, ...) UseMethod("get_layout")

#' @importFrom methods slot
#' @export
get_layout.HeatmapLayout <- function(x, layout, axis, ...) {
    .subset2(slot(x, paste(layout, "list", sep = "_")), axis)
}

#' @importFrom methods slot
#' @export
get_layout.StackLayout <- function(x, layout, ...) {
    slot(x, layout)
}

set_layout <- function(x, layout, ..., value) UseMethod("set_layout")

#' @importFrom methods slot slot<-
#' @export
set_layout.HeatmapLayout <- function(x, layout, axis, ..., value) {
    slot(x, paste(layout, "list", sep = "_"))[[axis]] <- value
    if (axis == "x") {
        if (!is.null(top <- slot(x, "top"))) {
            slot(x, "top") <- set_layout(top, layout, value = value)
        }
        if (!is.null(bottom <- slot(x, "bottom"))) {
            slot(x, "bottom") <- set_layout(bottom, layout, value = value)
        }
    } else {
        if (!is.null(left <- slot(x, "left"))) {
            slot(x, "left") <- set_layout(left, layout, value = value)
        }
        if (!is.null(right <- slot(x, "right"))) {
            slot(x, "right") <- set_layout(right, layout, value = value)
        }
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_layout.StackLayout <- function(x, layout, ..., value) {
    slot(x, layout) <- value
    axis <- to_coord_axis(slot(x, "direction"))
    slot(x, "plots") <- lapply(slot(x, "plots"), function(plot) {
        if (is.ggheatmap(plot)) {
            set_layout(plot, layout = layout, axis = axis, value = value)
        } else {
            plot
        }
    })
    x
}

#########################################################
# utils function ----------------------------------------
get_panel <- function(x, ...) UseMethod("get_panel")

#' @importFrom methods slot
#' @export
get_panel.HeatmapLayout <- function(x, axis, ...) {
    .subset2(slot(x, "panel_list"), axis)
}

#' @importFrom methods slot
#' @export
get_panel.StackLayout <- function(x, ...) {
    slot(x, "panel")
}

set_panel <- function(x, ...) UseMethod("set_panel")

#' @importFrom methods slot slot<-
#' @export
set_panel.HeatmapLayout <- function(x, axis, panel, ...) {
    slot(x, "panel_list")[[axis]] <- panel
    if (axis == "x") {
        if (!is.null(top <- slot(x, "top"))) {
            slot(x, "top") <- set_panel(top, panel)
        }
        if (!is.null(bottom <- slot(x, "bottom"))) {
            slot(x, "bottom") <- set_panel(bottom, panel)
        }
    } else {
        if (!is.null(left <- slot(x, "left"))) {
            slot(x, "left") <- set_panel(left, panel)
        }
        if (!is.null(right <- slot(x, "right"))) {
            slot(x, "right") <- set_panel(right, panel)
        }
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_panel.StackLayout <- function(x, panel, ...) {
    slot(x, "panel") <- panel
    axis <- to_coord_axis(slot(x, "direction"))
    slot(x, "plots") <- lapply(slot(x, "plots"), function(plot) {
        if (is.ggheatmap(plot)) {
            set_panel(plot, axis = axis, panel = panel)
        } else {
            plot
        }
    })
    x
}

##################################################################
get_index <- function(x, ...) UseMethod("get_index")

#' @importFrom methods slot
#' @export
get_index.HeatmapLayout <- function(x, axis, ...) {
    .subset2(slot(x, "index_list"), axis)
}

#' @importFrom methods slot
#' @export
get_index.StackLayout <- function(x, ...) {
    slot(x, "index")
}

set_index <- function(x, ...) UseMethod("set_index")

#' @importFrom methods slot slot<-
#' @export
set_index.HeatmapLayout <- function(x, axis, index, ...) {
    slot(x, "index_list")[[axis]] <- index
    if (axis == "x") {
        if (!is.null(top <- slot(x, "top"))) {
            slot(x, "top") <- set_index(top, index)
        }
        if (!is.null(bottom <- slot(x, "bottom"))) {
            slot(x, "bottom") <- set_index(bottom, index)
        }
    } else {
        if (!is.null(left <- slot(x, "left"))) {
            slot(x, "left") <- set_index(left, index)
        }
        if (!is.null(right <- slot(x, "right"))) {
            slot(x, "right") <- set_index(right, index)
        }
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_index.StackLayout <- function(x, index, ...) {
    slot(x, "index") <- index
    axis <- to_coord_axis(slot(x, "direction"))
    slot(x, "plots") <- lapply(slot(x, "plots"), function(plot) {
        if (is.ggheatmap(plot)) {
            set_index(plot, axis = axis, index = index)
        } else {
            plot
        }
    })
    x
}

##########################################################
get_nobs <- function(x, ...) UseMethod("get_nobs")

#' @importFrom methods slot
#' @export
get_nobs.HeatmapLayout <- function(x, axis, ...) {
    .subset2(slot(x, "nobs_list"), axis)
}

#' @importFrom methods slot
#' @export
get_nobs.StackLayout <- function(x, ...) slot(x, "nobs")

set_nobs <- function(x, ...) UseMethod("set_nobs")

#' @importFrom methods slot slot<-
#' @export
set_nobs.HeatmapLayout <- function(x, axis, nobs, ...) {
    slot(x, "nobs_list")[[axis]] <- nobs
    if (axis == "x") {
        if (!is.null(top <- slot(x, "top"))) {
            slot(x, "top") <- set_nobs(top, nobs)
        }
        if (!is.null(bottom <- slot(x, "bottom"))) {
            slot(x, "bottom") <- set_nobs(bottom, nobs)
        }
    } else {
        if (!is.null(left <- slot(x, "left"))) {
            slot(x, "left") <- set_nobs(left, nobs)
        }
        if (!is.null(right <- slot(x, "right"))) {
            slot(x, "right") <- set_nobs(right, nobs)
        }
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_nobs.StackLayout <- function(x, nobs, ...) {
    slot(x, "nobs") <- nobs
    axis <- to_coord_axis(slot(x, "direction"))
    slot(x, "plots") <- lapply(slot(x, "plots"), function(plot) {
        if (is.ggheatmap(plot)) {
            set_nobs(plot, axis = axis, nobs = nobs)
        } else {
            plot
        }
    })
    x
}
