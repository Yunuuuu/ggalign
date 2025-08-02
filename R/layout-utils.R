layout_init <- function(layout) { # setup default value for the layout
    layout@theme <- complete_theme(default_theme() + layout@theme)
    layout@schemes <- scheme_init(layout@schemes)
    layout
}

###########################################################
inherit_parent_layout_schemes <- function(layout, schemes) {
    if (is.null(schemes)) {
        return(layout@schemes)
    }
    scheme_inherit(schemes, layout@schemes)
}

inherit_parent_layout_theme <- function(layout, theme, spacing = NULL) {
    if (is.null(theme)) return(layout@theme) # styler: off
    # parent theme, set the global panel spacing,
    # so that every panel aligns well
    if (is.null(layout@theme)) return(theme) # styler: off
    ans <- theme + layout@theme
    if (is.null(spacing)) return(ans) # styler: off
    switch(spacing,
        x = ans + theme(
            panel.spacing.x = calc_element("panel.spacing.x", theme)
        ),
        y = ans + theme(
            panel.spacing.y = calc_element("panel.spacing.y", theme)
        )
    )
}

#############################################################
#' Reports whether `x` is layout object
#'
#' @param x An object to test.
#' @return A single boolean value.
#' @examples
#' is_layout(ggheatmap(1:10))
#'
#' @importFrom S7 S7_inherits
#' @export
is_layout <- function(x) S7_inherits(x, LayoutProto)

#' @examples
#' # for quad_layout()
#' is_quad_layout(quad_alignb(1:10))
#' is_quad_layout(quad_alignh(1:10))
#' is_quad_layout(quad_alignv(1:10))
#' is_quad_layout(quad_free(mtcars))
#'
#' @importFrom S7 S7_inherits
#' @export
#' @rdname is_layout
is_quad_layout <- function(x) S7_inherits(x, QuadLayout)

#' @examples
#' # for stack_layout()
#' is_stack_layout(stack_discrete("h", 1:10))
#' is_stack_layout(stack_continuous("h", 1:10))
#'
#' @importFrom S7 S7_inherits
#' @export
#' @rdname is_layout
is_stack_layout <- function(x) S7_inherits(x, StackLayout)

#' @importFrom S7 S7_inherits
#' @export
#' @rdname is_layout
is_stack_cross <- function(x) S7_inherits(x, StackCross)

#' @importFrom S7 S7_inherits
#' @export
#' @rdname is_layout
is_circle_layout <- function(x) S7_inherits(x, CircleLayout)

#' @examples
#' # for heatmap_layout()
#' is_heatmap_layout(ggheatmap(1:10))
#' @importFrom S7 S7_inherits
#' @export
#' @rdname is_layout
is_heatmap_layout <- function(x) S7_inherits(x, HeatmapLayout)

#' @examples
#' is_ggheatmap(ggheatmap(1:10))
#' @importFrom S7 S7_inherits
#' @export
#' @rdname is_layout
is_ggheatmap <- is_heatmap_layout

is_cross_layout <- function(x) is_stack_cross(x)
