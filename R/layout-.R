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
# add suffix "Proto" to avoid conflict with ggplot2
methods::setClass("LayoutProto",
    list(
        active = "ANY", # current active plot
        schemes = "list", # used to provide global parameters for all plots
        # control the layout, `theme` will also be used by `ggsave`
        titles = "list",
        annotation = "list", # To-Do add `pacth_titles` for layout
        theme = "ANY",
        `_namespace` = "ANY"
    ),
    prototype = list(
        active = NULL, titles = list(),
        annotation = list(), theme = NULL,
        `_namespace` = namespace_link
    )
)

#' @export
print.LayoutProto <- print.alignpatches

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.LayoutProto <- grid.draw.alignpatches

#' @export
alignpatch.LayoutProto <- function(x) alignpatch(ggalign_build(x))

#' Print Layout object
#'
#' @param object A `r rd_layout()`.
#' @return The input invisiblely.
#' @importFrom methods show
#' @export
#' @keywords internal
methods::setMethod("show", "LayoutProto", function(object) {
    print(object)
})

#' Subset a `Layout` object
#'
#' Used by [`ggplot_build`][ggplot2::ggplot_build] and
#' [`ggsave`][ggplot2::ggsave]
#'
#' @param x A `Layout` object
#' @param name A string of slot name in `Layout` object.
#' @return The slot value.
#' @importFrom methods slot
#' @export
#' @keywords internal
methods::setMethod("$", "LayoutProto", function(x, name) {
    slot(x, name)
})

###########################################################
default_layout <- function(layout) { # setup default value for the layout
    layout@theme <- complete_theme(default_theme() + layout@theme)

    # we by default, collect all guides
    layout@schemes$scheme_align["guides"] <- list(
        .subset2(.subset2(layout@schemes, "scheme_align"), "guides") %|w|% "tlbr"
    )

    # we by default, use `default_theme()`
    layout@schemes$scheme_theme <- update_scheme(
        .subset2(layout@schemes, "scheme_theme"),
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
    theme <- theme + layout@theme
    if (is.null(spacing)) return(theme) # styler: off
    switch(spacing,
        x = theme + theme(
            panel.spacing.x = calc_element("panel.spacing.x", theme)
        ),
        y = theme + theme(
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
ggalign_stat.ggalign_plot <- function(x, ...) {
    ggalign_stat(x@align, ...)
}

#' @export
ggalign_stat.Align <- function(x, ...) {
    rlang::check_dots_empty()
    .subset2(x, "statistics")
}

#' @export
ggalign_stat.default <- function(x, ...) {
    cli_abort(sprintf("no statistics found for %s", object_name(x)))
}

#' @export
ggalign_stat.AlignGg <- ggalign_stat.default

####################################################
#' Get Data from the Attached Attribute in the Rendering Process
#'
#' @description
#' `ggalign_attr` provides access to supplementary information stored as
#' attributes during the layout rendering process in `r rd_layout()`. These
#' attributes, commonly attached during data transformation by functions like
#' [`fortify_matrix()`] or [`fortify_data_frame()`], can include essential
#' details such as filtered or supplementary data that inform downstream
#' operations.
#'
#' @details
#' Attributes attached to the data are particularly useful when input data is
#' transformed in ways that restrict access to the complete dataset. For
#' instance, [`fortify_matrix.MAF()`] might filter mutation data while adding
#' attributes that retain essential context, such as the total number of
#' observations, for detailed or aggregated analyses.
#'
#' @param x Data used, typically inherited from the layout `r rd_layout()`.
#' @param field A string specifying the particular data to retrieve from the
#' attached attribute. If `NULL`, the entire attached attribute list will be
#' returned.
#'
#' @return The specified data from the attached attribute or `NULL` if it is
#' unavailable.
#'
#' @export
ggalign_attr <- function(x, field = NULL) {
    if (is.null(x <- ggalign_attr_get(x)) || is.null(field)) {
        return(x)
    }
    .subset2(x, field)
}

#' Set or get the Attached Attribute across the Rendering Process
#'
#' @description
#' - `ggalign_attr_set`: Attaches supplementary data to the input, facilitating
#'   downstream use.
#' - `ggalign_attr_get`: Extracts previously attached supplementary data during
#'   the transformation process.
#'
#' @param x Input data for the layout.
#' @param values A list to be attached.
#' @inherit ggalign_attr details
#' @seealso [`ggalign_attr()`]
#' @export
ggalign_attr_set <- function(x, values) {
    attr(x, ".__ggalign_data__") <- values
    x
}

#' @export
#' @rdname ggalign_attr_set
ggalign_attr_get <- function(x) attr(x, ".__ggalign_data__", exact = TRUE)

ggalign_attr_remove <- function(x) ggalign_attr_set(x, NULL)

# we keep a special attribute across all data
# this is used to pass additional annotation informations
ggalign_attr_restore <- function(data, original) {
    if (is.null(ggalign_attr_get(data)) && # no attached attribute
        # the original has attached attribute
        !is.null(ggalign_data <- ggalign_attr_get(original))) {
        ggalign_attr_set(data, ggalign_data)
    } else {
        data
    }
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
