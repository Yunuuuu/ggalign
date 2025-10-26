#' box_orientation Class
#'
#' The **`box_orientation`** class defines the intrinsic context for aligning
#' CraftBox objects. It is used to specify layout properties like alignment,
#' direction, and positioning of boxes within a layout. These properties are
#' essential for controlling the placement and alignment of the CraftBox
#' elements.
#'
#' @format An S7 class with the following properties:
#'   - axis: A character string indicating the alignment axis of the box, either
#'     "x" or "y".
#'   - is_linear: A logical value indicating whether the box should be arranged
#'     in a linear fashion.
#'   - direction: A character string specifying the direction of the box layout.
#'     Possible values are "horizontal" or "vertical". The default is `NA`,
#'     indicating no direction specified.
#'   - is_horizontal: A logical getter indicating whether the direction is
#'     horizontal. Returns `TRUE` if `direction` is set to "horizontal",
#'     otherwise `FALSE`.
#'   - is_vertical: A logical getter indicating whether the direction is
#'     vertical. Returns `TRUE` if `direction` is set to "vertical", otherwise
#'     `FALSE`.
#'   - position: A character string that determines the position of the box.
#'     Valid values come from the `.TLBR` set (Top, Left, Bottom, Right). The
#'     default is `NA`, indicating no position.
#'   - is_annotation: A logical getter that checks if the box is an annotation.
#'     Returns `TRUE` if `position` is `NA` (i.e., no position is specified).
#'   - inherit_quad_matrix: A logical value indicating whether the box inherits
#'     a quad matrix layout. The default is `FALSE`.
#'
BoxOrientation <- S7::new_class(
    "BoxOrientation",
    properties = list(
        axis = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L || is.na(value) ||
                    !any(value == c("x", "y"))) {
                    return(sprintf(
                        "can only be a string of %s",
                        oxford_or(c("x", "y"), code = FALSE)
                    ))
                }
            }
        ),
        is_linear = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
            }
        ),
        direction = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L ||
                    (!is.na(value) &&
                        !any(value == c("horizontal", "vertical")))) {
                    return(sprintf(
                        "can only be a string of %s",
                        oxford_or(c("horizontal", "vertical"), code = FALSE)
                    ))
                }
            },
            default = NA_character_
        ),
        is_horizontal = S7::new_property(
            getter = function(self) {
                !is.na(direction <- prop(self, "direction")) &&
                    is_horizontal(direction)
            }
        ),
        is_vertical = S7::new_property(
            getter = function(self) {
                !is.na(direction <- prop(self, "direction")) &&
                    is_vertical(direction)
            }
        ),
        position = S7::new_property(
            S7::class_character,
            validator = function(value) {
                if (length(value) != 1L ||
                    (!is.na(value) && !any(value == .TLBR))) {
                    return(sprintf(
                        "can only be a string of %s",
                        oxford_or(.TLBR, code = FALSE)
                    ))
                }
            },
            default = NA_character_
        ),
        is_annotation = S7::new_property(
            getter = function(self) is.na(prop(self, "position"))
        ),
        inherit_quad_matrix = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
            },
            default = FALSE
        )
    )
)

#' @importFrom S7 prop prop<-
prop_orientation <- function(property) {
    force(property)
    # intrincic attribvutes of layout cannot be modified
    S7::new_property(
        BoxOrientation,
        setter = function(self, value) {
            if (!is.null(prop(self, property))) {
                cli_abort(sprintf("'@%s' is read-only", property))
            }
            prop(self, property) <- value
            self
        }
    )
}

#' @importFrom S7 S7_inherits
#' @keywords internal
CraftBoxes <- S7::new_class(
    "CraftBoxes",
    properties = list(
        active = S7::new_property(
            S7::class_integer,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single integer number")
                }
            },
            default = NA_integer_
        ),
        box_list = S7::new_property(
            S7::class_list,
            validator = function(value) {
                for (fn in value) {
                    if (!S7_inherits(fn, BoxUnion)) {
                        return("must be a list of CraftBox")
                    }
                }
            }
        )
    )
)

S7::method(ggalign_update, list(CraftBoxes, CraftBox)) <-
    function(x, object, objectname, ...) {
        # check the name is unique
        box_list <- prop(x, "box_list")
        if (!is.na(name <- prop(object, "name"))) {
            active <- which(names(box_list) == name)
            if (length(active)) {
                cli_warn(
                    "Adding {.var {objectname}} will replace existing {.field {name}} plot"
                )
            } else {
                active <- length(box_list) + 1L
            }
            box_list[[name]] <- box
        } else {
            box_list <- c(box_list, list(box))
            active <- length(box_list)
        }
        prop(x, "box_list", check = FALSE) <- box_list
        if (!is.null(prop(object, "graph"))) {
            prop(x, "active", check = FALSE) <- active
        }
        x
    }

S7::method(ggalign_update, list(CraftBoxes, S7::class_any)) <-
    function(x, object, objectname, ...) {
        if (is.na(active <- prop(x, "active"))) {
            cli_abort(c(
                "Cannot add {.var {objectname}}",
                i = "No active plot component",
                i = paste(
                    "Did you forget to initialize a {.cls ggplot} object",
                    "with {.fn ggalign} or {.fn ggfree}?"
                )
            ))
        }
        prop(x, "box_list")[[active]] <- ggalign_update(
            .subset2(prop(x, "box_list"), active),
            object, objectname, ...
        )
        x
    }

# S7::method(layout_add, list(CraftBoxes, CraftBox)) <-
#     function(layout, object, ...) {
#         if (is.na(active <- layout@active) ||
#             is_craftbox(box <- .subset2(layout@box_list, active))) {
#             # initialize the necessary parameters for `Craftsman` object
#             if (is_stack_layout(layout)) {
#                 orientation <- BoxOrientation(
#                     switch_direction(layout@direction, "y", "x"),
#                     is_linear = FALSE,
#                     direction = layout@direction,
#                     position = .subset2(layout@heatmap, "position")
#                 )
#             } else if (is_circle_layout(layout)) {
#                 orientation <- BoxOrientation("x", is_linear = FALSE)
#             }

#             designer <- prop(object, "designer")
#             prop(object, "params") <- designer$setup_params(
#                 prop(object, "params"), orientation
#             )
#             graph <- prop(object, "graph")
#             prop(graph, "schemes") <- designer$setup_schemes(
#                 prop(graph, "schemes"), prop(object, "params"), orientation
#             )
#             layout <- designer$setup_layout(
#                 layout, prop(object, "params"), orientation
#             )
#             prop(object, "statistics") <- designer$compute(
#                 prop(object, "params"), orientation
#             )

#             # this step, the object will act with the stack layout
#             # group rows into panel or reorder rows, we can also
#             # initialize object data
#             new_domain <- designer$setup_domain(layout@domain)

#             # Initialize the plot object by retrieving it via the property
#             # accessor.
#             plot <- prop(graph, "plot")

#             # Execute all initialization hooks in sequence, passing the current
#             # plot and the designer's direction to each hook function. Each
#             # hook may modify and return a new version of the plot.
#             for (hook in prop(object, "init_hooks")) {
#                 plot <- hook(plot, orientation)
#             }

#             # Finally, initialize the plot using the craftsman-specific
#             # initializer, and update the object's plot attribute directly to
#             # bypass setter checks.
#             plot <- designer$init_plot(plot)
#             prop(graph, "plot") <- plot
#             prop(object, "plot") <- graph

#             layout <- chain_add_box(layout, object, object@active, ...)
#         } else { # should be a QuadLayout object
#             box <- layout_add(box, object, ...)
#             layout@box_list[[active]] <- box
#             new_domain <- prop(box, layout@direction)
#         }
#         layout_update_domain(layout, domain = new_domain, ...)
#     }
