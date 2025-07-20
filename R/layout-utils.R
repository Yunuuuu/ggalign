###########################################################
setup_layout <- S7::new_generic("setup_layout", "layout")

S7::method(setup_layout, LayoutProto) <- function(layout) {
    attr(layout, "theme") <- complete_theme(default_theme() + layout@theme)

    # we by default, collect all guides
    layout@schemes$scheme_align["guides"] <- list(
        .subset2(.subset2(layout@schemes, "scheme_align"), "guides") %|w|% "tlbr"
    )

    # we by default, use `default_theme()`
    attr(layout, "schemes") <- update_scheme(
        layout@schemes,
        new_scheme_theme(complete_theme(default_theme()))
    )
    layout
}

is_linear <- function(layout) UseMethod("is_linear")

#' @export
is_linear.StackLayout <- function(layout) TRUE

#' @export
is_linear.CircleLayout <- function(layout) FALSE

###########################################################
inherit_parent_layout_schemes <- function(layout, schemes) {
    if (is.null(schemes)) {
        return(layout@schemes)
    }
    inherit_schemes(layout@schemes, schemes)
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

############################################################
#' Get the statistics from the layout
#'
#' @param x A `r rd_layout()`.
#' @inheritParams rlang::args_dots_used
#' @return The statistics
#' @export
ggalign_stat <- function(x, ...) {
    UseMethod("ggalign_stat")
}

#' @param position A string of `r oxford_or(.TLBR)`.
#' @export
#' @rdname ggalign_stat
ggalign_stat.QuadLayout <- function(x, position, ...) {
    ggalign_stat(x = slot(x, position), ...)
}

#' @param what A single number or string of the plot elements in the stack
#' layout.
#' @export
#' @rdname ggalign_stat
ggalign_stat.StackLayout <- function(x, what, ...) {
    plot_list <- x@plot_list
    index <- vec_as_location2(
        what,
        n = length(plot_list),
        names = names(plot_list),
        missing = "error"
    )
    ggalign_stat(x = .subset2(plot_list, index), ...)
}

#' @export
ggalign_stat.CraftBox <- function(x, ...) {
    ggalign_stat(x@craftsman, ...)
}

#' @export
ggalign_stat.CraftAlign <- function(x, ...) {
    rlang::check_dots_empty()
    .subset2(x, "statistics")
}

#' @export
ggalign_stat.default <- function(x, ...) {
    cli_abort(sprintf("no statistics found for %s", object_name(x)))
}

#############################################################
#' Reports whether `x` is layout object
#'
#' @param x An object to test.
#' @return A single boolean value.
#' @examples
#' is_layout(ggheatmap(1:10))
#'
#' @importFrom methods is
#' @export
is_layout <- function(x) is(x, "LayoutProto")

#' @examples
#' # for quad_layout()
#' is_quad_layout(quad_alignb(1:10))
#' is_quad_layout(quad_alignh(1:10))
#' is_quad_layout(quad_alignv(1:10))
#' is_quad_layout(quad_free(mtcars))
#'
#' @export
#' @rdname is_layout
is_quad_layout <- function(x) is(x, "QuadLayout")

#' @examples
#' # for stack_layout()
#' is_stack_layout(stack_discrete("h", 1:10))
#' is_stack_layout(stack_continuous("h", 1:10))
#'
#' @export
#' @rdname is_layout
is_stack_layout <- function(x) is(x, "StackLayout")

#' @export
#' @rdname is_layout
is_stack_cross <- function(x) is(x, "StackCross")

#' @export
#' @rdname is_layout
is_circle_layout <- function(x) is(x, "CircleLayout")

#' @examples
#' # for heatmap_layout()
#' is_heatmap_layout(ggheatmap(1:10))
#' @export
#' @rdname is_layout
is_heatmap_layout <- function(x) is(x, "HeatmapLayout")

#' @examples
#' is_ggheatmap(ggheatmap(1:10))
#' @export
#' @rdname is_layout
is_ggheatmap <- is_heatmap_layout

is_cross_layout <- function(x) is_stack_cross(x)
