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
#' An additional attribute, which stores the factor levels, can be accessed with
#' `ggalign_lvls`.
#'
#' @details
#' Attributes attached to the data are especially useful when the input data is
#' transformed in ways that limit access to the complete dataset. For example,
#' [`fortify_matrix.MAF()`] might filter mutation data while adding attributes
#' that retain important context, such as the total number of observations, for
#' detailed or aggregated analyses. Additionally, it stores the levels of
#' `Variant_Classification` for further usage.
#'
#' @param x Data used, typically inherited from the layout `r rd_layout()`.
#' @param field A string specifying the particular data to retrieve from the
#' attached attribute. If `NULL`, the entire attached attribute list will be
#' returned.
#' @param check A boolean indicating whether to check if the `field` exists. If
#' `TRUE`, an error will be raised if the specified `field` does not exist.
#' @return
#' - `ggalign_attr`: The specified data from the attached attribute or `NULL` if
#' it is unavailable.
#' - `ggalign_lvls`: The attached levels.
#'
#' @export
ggalign_attr <- function(x, field = NULL, check = TRUE) {
    assert_string(field, allow_null = TRUE)
    if (is.null(x <- ggalign_attr_get(x)) || is.null(field)) {
        return(x)
    }
    if (isTRUE(check) && !rlang::has_name(x, field)) {
        cli_abort("Cannot find {field} in {.arg x}")
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
    attr(x, ".__ggalign_attr__") <- values
    x
}

#' @export
#' @rdname ggalign_attr_set
ggalign_attr_get <- function(x) attr(x, ".__ggalign_attr__", exact = TRUE)

ggalign_attr_remove <- function(x) ggalign_attr_set(x, NULL)

#' @export
#' @rdname ggalign_attr
ggalign_lvls <- function(x) ggalign_lvls_get(x)

#' Set or Get the Attached Levels
#'
#' @description
#' - `ggalign_lvls_set`: Attaches levels to the input, enabling the restoration
#'   of the `value` column when transformed from a matrix to a data frame.
#' - `ggalign_lvls_get`: Extracts previously attached levels during the
#'   transformation process.
#'
#' @param x Input data for the layout.
#' @param lvls A character vector representing the attached levels.
#' @inherit ggalign_lvls details
#' @seealso [`ggalign_lvls()`]
#' @export
ggalign_lvls_set <- function(x, lvls) {
    attr(x, ".__ggalign_levels__") <- lvls
    x
}

#' @export
#' @rdname ggalign_lvls_set
ggalign_lvls_get <- function(x) attr(x, ".__ggalign_levels__", exact = TRUE)

ggalign_lvls_remove <- function(x) ggalign_lvls_set(x, NULL)

# we keep a special attribute across all data
# this is used to pass additional annotation informations
ggalign_data_restore <- function(data, original) {
    if (is.null(ggalign_attr_get(data)) && # no attached attribute
        # the original has attached attribute
        !is.null(value <- ggalign_attr_get(original))) {
        data <- ggalign_attr_set(data, value)
    }

    if (is.null(ggalign_lvls_get(data)) && # no attached levels
        # the original has attached levels
        !is.null(value <- ggalign_lvls_get(original))) {
        data <- ggalign_lvls_set(data, value)
    }
    # to prevent the print of attributes
    if (!is.null(ggalign_attr_get(data)) ||
        !is.null(ggalign_lvls_get(data))) {
        data <- add_class(data, "ggalign_data")
    }
    data
}

ggalign_data_set <- function(.data, ..., .lvls = NULL) {
    if (...length() > 0L) {
        .data <- ggalign_attr_set(.data, list(...))
    }
    if (!is.null(.lvls)) {
        .data <- ggalign_lvls_set(.data, .lvls)
    }
    # to prevent the print of attributes
    if (!is.null(ggalign_attr_get(.data)) ||
        !is.null(ggalign_lvls_get(.data))) {
        .data <- add_class(.data, "ggalign_data")
    }
    .data
}

#' @export
print.ggalign_data <- function(x, ...) {
    print(
        remove_class(
            ggalign_lvls_remove(ggalign_attr_remove(x)),
            "ggalign_data"
        )
    )
    invisible(x)
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
