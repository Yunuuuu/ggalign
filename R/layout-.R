#' layout_context class - represents the context for layout properties.
#' Context will be inherited by child elements, such as child layouts or child
#' plots.
#' @include scheme-.R
layout_context <- S7::new_class(
    "layout_context",
    properties = list(
        theme = S7::new_property(
            ggplot2::class_theme,
            default = quote(theme())
        ),
        schemes = Schemes
    )
)

LayoutContextUnion <- rlang::inject(S7::new_union(
    # theme should be a `layout_theme` object
    layout_theme,
    !!!lapply(prop(layout_context, "properties")[
        setdiff(names(prop(layout_context, "properties")), "theme")
    ], .subset2, "class")
))

S7::method(ggalign_init, layout_context) <- function(x) {
    # `schemes_complete` ensures that all missing schemes are completed.
    # `ggalign_init` initializes the schemes property, setting the initial value
    # for the layout schemes in the `input` object.
    prop(x, "schemes", check = FALSE) <- ggalign_init(schemes_complete(
        prop(x, "schemes")
    ))

    # We take the `scheme_theme` from the schemes and treat it as the default
    # theme.
    prop(x, "theme", check = FALSE) <- ggalign_update(
        prop(schemes_get(prop(x, "schemes"), "scheme_theme"), "theme"),
        prop(x, "theme")
    )
    x
}

#' Prevents the direct update of layout_context properties.
#'
#' Direct updates to the layout context could break the inheritance structure,
#' leading to inconsistent visual results across child elements. By disallowing
#' this update, we ensure that properties can only be inherited or merged,
#' preserving the layout's consistency.
#' @noRd
S7::method(ggalign_update, list(layout_context, layout_context)) <-
    function(x, object, ...) {
        cli_abort(c(
            "Cannot update the context using {.fn ggalign_update}.",
            "i" = "Context properties can only be inherited."
        ))
    }

S7::method(ggalign_update, list(layout_context, layout_theme)) <-
    function(x, object, ...) {
        prop(x, "theme", check = FALSE) <- ggalign_update(
            prop(x, "theme"), prop(object, "theme"), ...
        )
        x
    }

S7::method(
    ggalign_update,
    list(layout_context, S7::new_union(Schemes, Scheme))
) <-
    function(x, object, ...) {
        prop(x, "schemes", check = FALSE) <- ggalign_update(
            prop(x, "schemes"), object, ...
        )
        x
    }

#' The 'align' argument allows further control over how `spacing` should be
#' inherited, ensuring uniformity in the layout.
#' @importFrom ggplot2 theme calc_element
#' @importFrom S7 prop prop<-
#' @noRd
S7::method(ggalign_inherit, list(layout_context, layout_context)) <-
    function(x, object, ..., align = NULL) {
        rlang::check_dots_empty()
        prop(x, "schemes", check = FALSE) <- ggalign_inherit(
            prop(x, "schemes"), prop(object, "schemes"), ...
        )
        prop(x, "theme", check = FALSE) <- prop(object, "theme") +
            prop(x, "theme")
        if (is.null(align)) return(x) # styler: off

        # Align panel spacing based on 'align' argument (x, y, or xy)
        prop(x, "theme", check = FALSE) <- prop(x, "theme") +
            switch(align,
                x = theme(
                    panel.spacing.x = calc_element(
                        "panel.spacing.x",
                        prop(object, "theme")
                    )
                ),
                y = theme(
                    panel.spacing.y = calc_element(
                        "panel.spacing.y",
                        prop(object, "theme")
                    )
                ),
                xy = theme(
                    panel.spacing.x = calc_element(
                        "panel.spacing.x",
                        prop(object, "theme")
                    ),
                    panel.spacing.y = calc_element(
                        "panel.spacing.y",
                        prop(object, "theme")
                    )
                ),
                cli_abort("Invalid {.arg align} provided: {align}")
            )
        x
    }

S7::method(ggalign_update, list(ggplot2::class_ggplot, layout_context)) <-
    function(x, object, objectname, ..., schemes) {
        context <- ggalign_inherit(
            layout_context(x$theme, schemes),
            object,
            ...
        )
        x <- ggalign_update(x, prop(context, "schemes"))
        x$theme <- prop(context, "theme")
        x
    }

S7::method(ggalign_update, list(Graph, layout_context)) <-
    function(x, object, objectname, ...) {
        plot <- prop(x, "plot")
        context <- ggalign_inherit(
            layout_context(plot$theme, prop(x, "schemes")),
            object,
            ...
        )
        prop(x, "schemes") <- prop(context, "schemes")
        plot$theme <- prop(context, "theme")
        prop(x, "plot") <- plot
        x
    }

###############################################################
#' layout_control class - integrates properties that cannot be inherited.
#' Whenever a new property is added to this class, we must ensure
#' `LayoutControlUnion` is updated. This ensures that the `ggalign_update`
#' method is automatically implemented for the new property.
layout_control <- S7::new_class(
    "layout_control",
    properties = list(titles = layout_title)
)

LayoutControlUnion <- rlang::inject(S7::new_union(
    !!!lapply(prop(layout_control, "properties"), .subset2, "class")
))

S7::method(ggalign_init, layout_control) <- function(x) x
S7::method(ggalign_update, list(layout_control, layout_title)) <-
    function(x, object, ...) {
        prop(x, "titles", check = FALSE) <- ggalign_update(
            prop(x, "titles"), object, ...
        )
        x
    }

###############################################################
#' @importFrom S7 new_object S7_object
LayoutProto <- S7::new_class(
    "LayoutProto",
    properties = list(
        name = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single character string")
                }
            },
            setter = function(self, value) {
                if (!is.null(prop(self, "name"))) {
                    cli_abort("'@name' is read-only")
                }
                prop(self, "name") <- value
                self
            },
            default = NA_character_
        ),
        # when `data` is modified, the internal domain will need modified too.
        # so we prevent modify the initial `data`
        data = S7::new_property(
            S7::class_any,
            setter = function(self, value) {
                if (!is.null(prop(self, "data"))) {
                    cli_abort("'@data' is read-only")
                }
                prop(self, "data") <- value
                self
            }
        ),
        context = layout_context,
        control = layout_control
    ),
    abstract = TRUE
)

###############################################################
local(S7::method(`$`, LayoutProto) <- function(x, name) prop(x, name))

local(S7::method(print, LayoutProto) <- print.patch_ggplot)

#' @importFrom grid grid.draw
local(S7::method(grid.draw, LayoutProto) <- grid.draw.patch_ggplot)

# Used by `alignpatches`
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

############################################################
S7::method(ggalign_init, LayoutProto) <- function(x) {
    prop(x, "context", check = FALSE) <- ggalign_init(prop(x, "context"))
    prop(x, "control", check = FALSE) <- ggalign_init(prop(x, "control"))
    x
}

S7::method(ggalign_update, list(LayoutProto, LayoutContextUnion)) <-
    function(x, object, ...) {
        prop(x, "context", check = FALSE) <- ggalign_update(
            prop(x, "context"), object, ...
        )
        x
    }

S7::method(ggalign_update, list(LayoutProto, LayoutControlUnion)) <-
    function(x, object, ...) {
        prop(x, "control", check = FALSE) <- ggalign_update(
            prop(x, "control"), object, ...
        )
        x
    }

############################################################
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
