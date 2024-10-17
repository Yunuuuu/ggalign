# Will ensure serialisation includes a link to the ggalign namespace
# Copied from patchwork
namespace_link <- function() NULL

# https://stackoverflow.com/questions/65817557/s3-methods-extending-ggplot2-gg-function
# Here we use S4 object to override the double dispatch of `+.gg` method
# TO-DO: use S7
#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @keywords internal
methods::setClass("Layout",
    list(
        active = "ANY",
        action = "ANY", # used to control the default plot behaviour
        # control the layout, `theme` will also be used by `ggsave`
        titles = "list",
        annotation = "list", # To-DO add `pacth_titles` for layout
        theme = "ANY",
        `_namespace` = "ANY"
    ),
    prototype = list(
        active = NULL, titles = list(), annotation = list(), theme = NULL,
        `_namespace` = namespace_link
    )
)

layout_default <- function(layout) {
    # we always remove panel border from the default theme
    layout@theme <- default_theme() + theme(panel.border = element_blank()) +
        layout@theme
    # we by default, collect all guides
    layout@action["guides"] <- list(
        .subset2(layout@action, "guides") %|w|% "tlbr"
    )
    layout
}

is_layout <- function(x) methods::is(x, "Layout")

#' @export
print.Layout <- print.alignpatches

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.Layout <- grid.draw.alignpatches

#' @export
alignpatch.Layout <- function(x) alignpatch(ggalign_build(x))

#' Print Layout object
#'
#' @param object A `r rd_layout()`.
#' @return The input invisiblely.
#' @importFrom methods show
#' @export
#' @keywords internal
methods::setMethod("show", "Layout", function(object) {
    print(object)
})

#' Subset a `Layout` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A `Layout` object
#' @param name A string of slot name in `Layout` object.
#' @return The slot value.
#' @importFrom methods slot
#' @export
#' @keywords internal
methods::setMethod("$", "Layout", function(x, name) {
    slot(x, name)
})

#############################################################
#' Add components to `Layout`
#'
#' @param e1 A `r rd_layout()`.
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

#' @importFrom utils modifyList
#' @export
#' @rdname layout-add
methods::setMethod("+", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    if (is.null(e2)) return(e1) # styler: off
    if (inherits(e2, "layout_title")) {
        e1@titles <- update_non_waive(e1@titles, e2)
        return(e1)
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
#' Layout operator
#'
#' @details
#' In order to reduce code repetition `ggalign` provides two operators for
#' adding ggplot elements (geoms, themes, facets, etc.) to multiple/all plots in
#' `r rd_layout()`.
#'
#' Like `patchwork`, `&` add the element to all plots in the plot. If the
#' element is a [theme][ggplot2::theme], this will also modify the layout
#' theme.
#'
#' Unlike `patchwork`, the `-` operator adds ggplot2 elements (geoms, themes,
#' facets, etc.) rather than a ggplot plot. The key difference between `&` and
#' `-` is in how they behave in [heatmap_layout()]. The `-` operator only
#' applies the element to the current active context in [heatmap_layout()].
#' Using `-` might seem unintuitive if you think of the operator as "subtract",
#' the underlying reason is that `-` is the only operator in the same precedence
#' group as `+`.
#'
#' @param e1 A `r rd_layout()`.
#' @param e2 An object to be added to the plot.
#' @return A modified `Layout` object.
#' @examples
#' mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(mat) +
#'     hmanno("top") +
#'     align_dendro() &
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#' ggheatmap(mat) +
#'     hmanno("top") +
#'     align_dendro() -
#'     theme(panel.border = element_rect(
#'         colour = "red", fill = NA, linewidth = unit(2, "mm")
#'     ))
#'
#' @name layout-operator
NULL

#' @aliases &.Layout &.HeatmapLayout &.ggheatmap &.StackLayout &.ggstack
#' @rdname layout-operator
#' @export
methods::setMethod("&", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code &} with a single argument.",
            "i" = "Did you accidentally put {.code &} on a new line?"
        ))
    }
    if (is.null(e2)) return(e1) # styler: off
    if (inherits(e2, "layout_title")) {
        cli::cli_abort(c(
            "Cannot use {.code &} to change the layout titles",
            i = "Try to use {.code +} instead"
        ))
    }

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    e1 <- layout_and_add(e1, e2, e2name)

    # to align with `patchwork`, we also modify the layout theme
    # when using `&` to add the theme object.
    if (inherits(e2, "theme")) {
        e1@theme <- e1@theme + e2
    }
    e1
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

#########################################################
#' @aliases -.Layout -.HeatmapLayout -.ggheatmap -.StackLayout -.ggstack
#' @rdname layout-operator
#' @export
methods::setMethod("-", c("Layout", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code -} with a single argument.",
            "i" = "Did you accidentally put {.code -} on a new line?"
        ))
    }
    if (is.null(e2)) return(e1) # styler: off
    if (inherits(e2, "layout_title")) {
        cli::cli_abort(c(
            "Cannot use {.code -} to change the layout titles",
            i = "Try to use {.code +} instead"
        ))
    }

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_subtract(e1, e2, e2name)
})

#' @keywords internal
layout_subtract <- function(layout, object, object_name) {
    UseMethod("layout_subtract")
}

#' @export
layout_subtract.HeatmapLayout <- function(layout, object, object_name) {
    layout_heatmap_subtract(object, layout, object_name)
}

#' @export
layout_subtract.StackLayout <- function(layout, object, object_name) {
    layout_stack_subtract(object, layout, object_name)
}

############################################################
#' Get the statistics from the layout
#'
#' @param x A `r rd_layout()`.
#' @inheritParams rlang::args_dots_used
#' @return The statistics
#' @export
ggalign_stat <- function(x, ...) UseMethod("ggalign_stat")

#' @param position A string of `"top"`, `"left"`, `"bottom"`, or `"right"`.
#' @param what A single number or string of the plot elements in the stack
#' layout.
#' @export
#' @rdname ggalign_stat
ggalign_stat.HeatmapLayout <- function(x, position, ...) {
    ggalign_stat(x = slot(x, position), ...)
}

#' @export
#' @rdname ggalign_stat
ggalign_stat.StackLayout <- function(x, what, ...) {
    if (is.null(ans <- .subset2(x@plots, what))) {
        cli::cli_abort("Cannot find {what} plot in this stack layout")
    }
    ggalign_stat(x = ans, ...)
}

#' @export
ggalign_stat.Align <- function(x, ...) {
    if (...length() > 0L) {
        cli::cli_abort(c(
            "Find unused arguments in {.arg ...}",
            i = "Please check argument{?s}: {c(...)}"
        ))
    }
    .subset2(x, "statistics")
}

############################################################
############################################################
# layout should be one of "index", "nobs", "panel"
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
        if (!is.null(top <- x@top)) {
            x@top <- set_layout(top, layout, value = value)
        }
        if (!is.null(bottom <- x@bottom)) {
            x@bottom <- set_layout(bottom, layout, value = value)
        }
    } else {
        if (!is.null(left <- x@left)) {
            x@left <- set_layout(left, layout, value = value)
        }
        if (!is.null(right <- x@right)) {
            x@right <- set_layout(right, layout, value = value)
        }
    }
    x
}

#' @importFrom methods slot slot<-
#' @export
set_layout.StackLayout <- function(x, layout, ..., value) {
    slot(x, layout) <- value
    axis <- to_coord_axis(x@direction)
    x@plots <- lapply(x@plots, function(plot) {
        if (is_ggheatmap(plot)) {
            set_layout(plot, layout = layout, axis = axis, value = value)
        } else {
            plot
        }
    })
    x
}

get_panel <- function(x, axis) {
    get_layout(x = x, layout = "panel", axis = axis)
}
get_index <- function(x, axis) {
    get_layout(x = x, layout = "index", axis = axis)
}
get_nobs <- function(x, axis) {
    get_layout(x = x, layout = "nobs", axis = axis)
}

set_panel <- function(x, ..., axis, value) {
    set_layout(x = x, layout = "panel", ..., axis = axis, value = value)
}

set_index <- function(x, ..., axis, value) {
    set_layout(x = x, layout = "index", ..., axis = axis, value = value)
}

set_nobs <- function(x, ..., axis, value) {
    set_layout(x = x, layout = "nobs", ..., axis = axis, value = value)
}
