#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @importFrom ggplot2 theme
#' @include scheme-.R
#' @keywords internal
LayoutProto <- S7::new_class(
    "LayoutProto",
    properties = list(
        data = S7::class_any,
        titles = layout_title,
        schemes = Schemes,
        theme = S7::new_property(
            ggplot2::class_theme,
            default = quote(theme())
        ),
        name = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            default = NA_character_
        )
    ),
    abstract = TRUE
)

local(S7::method(`$`, LayoutProto) <- function(x, name) prop(x, name))

local(S7::method(print, LayoutProto) <- print.patch_ggplot)

#' @importFrom grid grid.draw
local(S7::method(grid.draw, LayoutProto) <- grid.draw.patch_ggplot)

# Used by both `alignpatches`
#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(LayoutProto, alignpatches)) <-
    function(object, plot, objectname) {
        prop(plot, "plots") <- c(prop(plot, "plots"), list(object))
        plot
    }

S7::method(alignpatches_apply, list(LayoutProto, S7::class_any)) <-
    function(plot, object, objectname) {
        layout_apply_all(plot, object, objectname)
    }

# Used by both `circle_layout()` and `stack_layout()`
#' @keywords internal
ChainLayout <- S7::new_class("ChainLayout",
    LayoutProto,
    properties = list(
        current = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single integer number")
                }
            },
            default = NA_integer_
        ),
        box_list = S7::class_list,
        domain = prop_domain(),
        direction = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            }
        )
    )
)

#' @importFrom ggplot2 waiver
#' @importFrom grid is.unit
#' @importFrom S7 convert
#' @importFrom rlang is_atomic
#' @keywords internal
StackLayout <- S7::new_class(
    "StackLayout", ChainLayout,
    properties = list(
        sizes = prop_grid_unit("sizes", validator = validator_size(3L)),
        # used by heatmap annotation
        heatmap = S7::new_property(
            S7::class_list,
            default = quote(list(
                position = NULL,
                free_guides = waiver(),
                # indicate whether or not the data is from the quad-layout
                # matrix
                quad_matrix = FALSE
            ))
        )
    )
)

StackCross <- S7::new_class(
    "StackCross", StackLayout,
    # A list of old domain
    properties = list(
        odomain = S7::class_list,
        cross_points = S7::class_integer,
        break_points = S7::class_integer
    )
)

#' @keywords internal
CircleLayout <- S7::new_class(
    "CircleLayout", ChainLayout,
    properties = list(
        radial = S7::new_property(
            S7::new_union(NULL, S7::new_S3_class("CoordRadial")),
            validator = function(value) {
                if (!is.null(value) && abs(diff(value$arc)) < pi / 2L) {
                    return("must span at least 90 degrees; smaller arcs are not supported")
                }
            }
        ),
        sector_spacing = S7::new_union(NULL, S7::class_numeric)
    )
)

#' @importFrom S7 S7_inherits
prop_stack_layout <- function(property, ...) {
    force(property)
    S7::new_property(
        S7::class_any,
        validator = function(value) {
            if (!is.null(value) && !S7_inherits(value, StackLayout)) {
                return("must be a 'StackLayout' object")
            }
        },
        setter = function(self, value) {
            prop(self, property) <- value
            self
        },
        ...,
        default = NULL
    )
}

# Used to create the QuadLayout
QuadLayout <- S7::new_class(
    "QuadLayout", LayoutProto,
    properties = list(
        current = S7::new_property(
            S7::new_union(NULL, S7::class_integer, S7::class_character),
            validator = function(value) {
                if (!is.null(value) && length(value) != 1L) {
                    return("must be a single integer number or single string")
                }
            }
        ),
        plot = S7::new_union(NULL, ggplot2::class_ggplot),
        body_schemes = Schemes,
        # parameters for main body
        width = prop_grid_unit("width", validator = validator_size(1L)),
        height = prop_grid_unit("height", validator = validator_size(1L)),
        # Used to align axis
        horizontal = prop_domain(),
        vertical = prop_domain(),
        # top, left, bottom, right must be a StackLayout object.
        top = prop_stack_layout("top"),
        left = prop_stack_layout("left"),
        bottom = prop_stack_layout("bottom"),
        right = prop_stack_layout("right"),
        # If we regard `QuadLayout` as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        plot_active = active
    )
)

# used to create the heatmap layout
#' @keywords internal
HeatmapLayout <- S7::new_class(
    "HeatmapLayout", QuadLayout,
    properties = list(filling = S7::class_any) # parameters for heatmap body
)

###########################################################
#' @importFrom ggplot2 complete_theme
S7::method(init_object, LayoutProto) <- function(input) {
    # initialize layout schemes
    input@schemes <- scheme_init(input@schemes)

    # Merge the provided layout theme with the default theme.
    th <- prop(schemes_get(input@schemes, "scheme_theme"), "theme") +
        input@theme

    # Apply the updated theme
    input@theme <- th
    input
}

#' @importFrom S7 S7_dispatch
is_linear <- S7::new_generic(
    "is_linear", "layout",
    function(layout) S7_dispatch()
)

S7::method(is_linear, StackLayout) <- function(layout) TRUE
S7::method(is_linear, CircleLayout) <- function(layout) FALSE

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
`ggalign_stat.ggalign::QuadLayout` <- function(x, position, ...) {
    ggalign_stat(x = prop(x, position), ...)
}

#' @param what A single number or string of the plot elements in the stack
#' layout.
#' @export
#' @rdname ggalign_stat
`ggalign_stat.ggalign::StackLayout` <- function(x, what, ...) {
    box_list <- x@box_list
    index <- vec_as_location2(
        what,
        n = length(box_list),
        names = names(box_list),
        missing = "error"
    )
    ggalign_stat(x = .subset2(box_list, index), ...)
}

#' @export
`ggalign_stat.ggalign::CraftBox` <- function(x, ...) {
    ggalign_stat(prop(x, "craftsman"), ...)
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
