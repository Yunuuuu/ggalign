#' A `Layout` object
#'
#' A `Layout` object defines how to place the plots.
#'
#' @importFrom ggplot2 is_theme
#' @include scheme-.R
#' @keywords internal
LayoutProto <- S7::new_class("LayoutProto",
    properties = list(
        data = S7::class_any,
        current = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be of length 1")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "current"))) {
                    cli_abort("'@current' is read-only.")
                }
                prop(self, "current") <- value
                self
            },
            default = NA_integer_
        ),
        schemes = prop_schemes("schemes"),
        titles = prop_layout_title("titles"),
        theme = prop_layout_theme("theme"),
        name = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "name"))) {
                    cli_abort("'@name' is read-only.")
                }
                prop(self, "name") <- value
                self
            },
            default = NA_character_
        )
    )
)

#' Subset a `Layout` object
#'
#' Used by [`ggplot_build`][ggplot2::ggplot_build] and
#' [`ggsave`][ggplot2::ggsave]
#'
#' @param x A `Layout` object
#' @param name A string of slot name in `Layout` object.
#' @return The slot value.
#' @importFrom S7 prop
#' @keywords internal
local(S7::method(`$`, LayoutProto) <- function(x, name) prop(x, name))

local(S7::method(print, LayoutProto) <- print.alignpatches)

#' @importFrom grid grid.draw
local(S7::method(grid.draw, LayoutProto) <- grid.draw.alignpatches)

local(S7::method(alignpatch, LayoutProto) <- function(x) {
    alignpatch(ggalign_build(x))
})

# Used by both `circle_layout()` and `stack_layout()`
#' @keywords internal
ChainLayout <- S7::new_class("ChainLayout",
    LayoutProto,
    properties = list(
        box_list = S7::new_property(
            S7::class_list,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "box_list"))) {
                    cli_abort("'@box_list' is read-only.")
                }
                prop(self, "box_list") <- value
                self
            },
            default = list()
        ),
        domain = prop_domain("domain"),
        direction = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "direction"))) {
                    cli_abort("'@direction' is read-only.")
                }
                prop(self, "direction") <- value
                self
            }
        )
    )
)

#' @importFrom ggplot2 waiver
#' @keywords internal
StackLayout <- S7::new_class(
    "StackLayout", ChainLayout,
    properties = list(
        sizes = S7::class_any, # used by stack layout
        # used by heatmap annotation
        heatmap = S7::new_property(
            S7::class_list,
            setter = function(self, value) {
                if (!is.null(prop(self, "heatmap"))) {
                    cli_abort("'@heatmap' is read-only.")
                }
                prop(self, "heatmap") <- value
                self
            },
            default = list(
                position = NULL,
                free_guides = waiver(),
                # indicate whether or not the data is from the quad-layout
                # matrix
                quad_matrix = FALSE
            )
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
            S7::class_any,
            validator = function(value) {
                if (!is.null(value) && !inherits(value, "CoordRadial")) {
                    return("must be created with `coord_circle()`")
                }
                if (!is.null(radial) && abs(diff(radial$arc)) < pi / 2L) {
                    return("circle of acute angle < 90 is not allowed")
                }
            }
        ),
        sector_spacing = S7::class_any
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
        plot = S7::new_property(
            S7::new_union(S7::class_any),
            validator = function(value) {
                if (!is.null(value) && !is_ggplot(value)) {
                    return("must be a 'ggplot' object")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "plot"))) {
                    cli_abort("'@plot' is read-only")
                }
                prop(self, "plot") <- value
                self
            },
            default = NULL
        ),
        body_schemes = prop_schemes("body_schemes"),
        # parameters for main body
        width = prop_grid_unit("width"),
        height = prop_grid_unit("height"),
        # Used to align axis
        horizontal = prop_domain("horizontal"),
        vertical = prop_domain("vertical"),
        # top, left, bottom, right must be a StackLayout object.
        top = prop_stack_layout("top"),
        left = prop_stack_layout("left"),
        bottom = prop_stack_layout("bottom"),
        right = prop_stack_layout("right"),
        # If we regard `QuadLayout` as a plot, and put it into the stack
        # layout, we need following arguments to control it's behavour
        plot_active = S7::new_property(
            active,
            setter = function(self, value) {
                if (!is.null(prop(self, "plot_active"))) {
                    cli_abort("'@plot_active' is read-only; use the '+' operator to update it.")
                }
                prop(self, "plot_active") <- value
                self
            }
        )
    )
)

# used to create the heatmap layout
#' @keywords internal
HeatmapLayout <- S7::new_class(
    "HeatmapLayout", QuadLayout,
    properties = list(filling = S7::class_any) # parameters for heatmap body
)

is_linear <- S7::new_generic(
    "is_linear", "layout",
    function(layout) S7::S7_dispatch()
)

S7::method(is_linear, StackLayout) <- function(layout) TRUE

S7::method(is_linear, CircleLayout) <- function(layout) TRUE
