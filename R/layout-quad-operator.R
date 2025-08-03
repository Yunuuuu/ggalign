# Preventing from adding following elements
#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, S7::class_data.frame)) <-
    function(layout, object, objectname) {
        cli_abort(sprintf("Can't change data of %s", object_name(layout)))
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, S3_layout_title)) <-
    function(layout, object, objectname) {
        layout@titles <- layout_title_update(layout@titles, object)
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, S7::class_list)) <-
    function(layout, object, objectname) {
        for (o in object) layout <- layout_add(layout, o, object_name)
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, S3_class_ggplot)) <-
    function(layout, object, objectname) {
        layout_add(layout, ggfree(data = object), objectname)
    }

# Add elements for the main body or the annotation
#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, S7::class_any)) <-
    function(layout, object, objectname) {
        if (is.matrix(object)) {
            cli_abort(sprintf("Can't change data of %s", object_name(layout)))
        }
        if (is.null(position <- layout@current)) {
            layout <- quad_body_add(object, layout, objectname)
        } else if (is.null(stack <- prop(layout, position))) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "the {.field {position}} annotation stack is not initialized",
                i = "Try to use {.code quad_anno(initialize = TRUE)} or you can add a {.code stack_layout()} manually"
            ))
        } else {
            prop(layout, position) <- layout_add(stack, object, objectname)
        }
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_add, list(QuadLayout, QuadScope)) <-
    function(layout, object, objectname) {
        old <- layout@current
        contexts <- quad_scope_contexts(object, old)
        if (is.null(contexts)) contexts <- list(NULL)
        object_name <- prop(object, "object_name")
        object <- prop(object, "object")
        for (active in contexts) {
            layout@current <- active
            layout <- layout_add(layout, object, object_name)
        }
        layout@current <- old
        layout
    }

##################################################################
#' @include layout-.R
#' @include layout-operator.R
S7::method(
    layout_add,
    list(QuadLayout, S7::new_S3_class("quad_active"))
) <- function(layout, object, objectname) {
    if (!is.null(width <- .subset2(object, "width"))) {
        layout@width <- width
    }
    if (!is.null(height <- .subset2(object, "height"))) {
        layout@height <- height
    }
    layout@current <- NULL
    layout
}

#' @include layout-.R
#' @include layout-operator.R
S7::method(
    layout_add,
    list(QuadLayout, S7::new_S3_class("quad_anno"))
) <- function(layout, object, objectname) {
    position <- .subset2(object, "position")
    initialize <- .subset2(object, "initialize")
    stack <- prop(layout, position)
    if (is.null(stack) && !isFALSE(initialize)) {
        # try to initialize the annotation stack with the layout data
        direction <- to_direction(position)
        layout_domain <- prop(layout, direction)
        # for the annotation stack, we try to take the data from the
        # quad layout
        quad_data <- layout@data
        data <- waiver() # use waiver() to indicate data is not initialized
        quad_matrix <- FALSE # the default value for `quad_matrix` in the stack
        if (!is_discrete_domain(layout_domain)) { # the stack need a data frame
            if (!is.data.frame(quad_data)) {
                if (is.null(initialize)) {
                    cli_warn(paste(
                        "`data` in {.fn {layout@name}} is",
                        "{.obj_type_friendly {quad_data}}, but the",
                        "{.field {position}} annotation stack need a",
                        "{.cls data.frame}, won't initialize",
                        "the {.field {position}} annotation stack"
                    ))
                } else {
                    data <- NULL
                }
            } else {
                data <- quad_data
            }
        } else if (is.matrix(quad_data)) { # the stack need a matrix
            if (is_horizontal(direction)) {
                data <- quad_data
            } else {
                data <- ggalign_data_restore(t(quad_data), quad_data)
            }
            quad_matrix <- TRUE
        } else {
            if (is.null(initialize)) {
                cli_warn(paste(
                    "`data` in {.fn {layout@name}} is",
                    "{.obj_type_friendly {quad_data}}, but the",
                    "{.field {position}} annotation stack need a",
                    "{.cls data.frame}, won't initialize",
                    "the {.field {position}} annotation stack"
                ))
            } else {
                data <- NULL
            }
        }
        if (!is.waive(data)) { # initialize the annotation stack
            stack <- new_stack_layout(
                data = data,
                direction = direction,
                # the layout parameters should be the same with `quad_layout()`
                domain = layout_domain,
                # we'll inherit the action data function when
                schemes = default_schemes(if (is.null(data)) NULL else waiver())
            )
            stack@heatmap$position <- position
            stack@heatmap$quad_matrix <- quad_matrix
        }
    } else if (!is.null(stack) && isTRUE(initialize)) {
        cli_abort(c(
            "Cannot initialize the {position} annotation stack",
            i = "{position} annotation stack has already been initialized"
        ))
    }

    if (!is.null(stack)) {
        # update parameters
        if (!is.null(size <- .subset2(object, "size"))) {
            stack@sizes <- size
        }
        if (!is.waive(free_guides <- .subset2(object, "free_guides"))) {
            stack@heatmap["free_guides"] <- list(free_guides)
        }
        stack <- switch_chain_plot(
            stack, .subset2(object, "what"), quote(quad_anno())
        )
        prop(layout, position) <- stack
    }
    layout@current <- position
    layout
}

#' @include layout-.R
#' @include layout-operator.R
S7::method(
    layout_add,
    list(QuadLayout, QuadLayout)
) <- function(layout, object, objectname) {
    cli_abort(c(
        sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
        i = "Did you mean to place multiple {.fn quad_layout} elements inside a {.fn stack_layout}?"
    ))
}

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, StackLayout)) <-
    function(layout, object, objectname) {
        # we check if there is an active annotation
        if (is.null(position <- layout@current)) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "no active annotation stack",
                i = "try to activate an annotation stack with {.fn anno_*}"
            ))
        }
        # check the annotation stack is not initialized
        if (!is.null(prop(layout, position))) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "{position} annotation stack already exists"
            ))
        }
        # cannot contain nested layout
        if (!all(vapply(object@box_list, is_craftbox, logical(1L),
                    USE.NAMES = FALSE))) { # styler: off
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "annotation stack cannot contain nested layout"
            ))
        }
        # check quad layout is compatible with stack layout
        if (!identical(direction <- to_direction(position), object@direction)) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "only {.field {direction}} stack is allowed in {position} annotation"
            ))
        }
        if (length(object@sizes) > 1L) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "{.arg sizes} must be of length one to use the stack as an annotation"
            ))
        }
        quad_domain <- prop(layout, direction)
        if (is_cross_layout(object) &&
            any(position == c("bottom", "right")) &&
            !is_empty(object@cross_points)) {
            # if there are cross points in bottom or right annotation,
            # use the first domain
            stack_domain <- .subset2(object@odomain, 1L)
        } else {
            stack_domain <- object@domain
        }
        # check if we can align in this direction
        # `stack_layout()` is free from aligning obervations in this axis
        if (!is_discrete_domain(stack_domain)) {
            if (is_discrete_domain(quad_domain)) {
                cli_abort(c(
                    sprintf(
                        "Cannot add {.var {objectname}} to %s",
                        object_name(layout)
                    ),
                    i = sprintf(
                        "%s cannot align continuous variable in {.field {direction}} direction",
                        object_name(layout)
                    )
                ))
            }
            layout_domain <- stack_domain
        } else if (is_discrete_domain(quad_domain)) {
            layout_domain <- discrete_domain_update(
                quad_domain, stack_domain,
                old_name = object_name(layout),
                new_name = objectname
            )
        } else {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = sprintf(
                    "%s cannot align discrete variable in {.field {direction}} direction",
                    object_name(layout)
                )
            ))
        }
        object@heatmap$position <- position
        prop(layout, position) <- object
        layout_update_domain(layout,
            direction = direction,
            domain = layout_domain,
            objectname = objectname
        )
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_add, list(QuadLayout, CraftBox)) <-
    function(layout, object, objectname) {
        if (is.null(position <- layout@current)) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "no active annotation stack",
                i = "try to activate an annotation stack with {.fn anno_*}"
            ))
        }
        if (is.null(stack <- prop(layout, position))) {
            cli_abort(c(
                sprintf("Cannot add {.var {objectname}} to %s", object_name(layout)),
                i = "the {.field {position}} annotation stack is not initialized",
                i = "Try to use {.code quad_anno(initialize = TRUE)} or you can add a {.code stack_layout()} manually"
            ))
        }

        # add annotation -----------------------------
        stack <- layout_add(stack, object, objectname)
        prop(layout, position) <- stack

        # if there are cross points in bottom or right annotation, we use
        # the first domain
        if (is_cross_layout(stack) &&
            any(position == c("bottom", "right")) &&
            !is_empty(stack@cross_points)) {
            new_domain <- .subset2(stack@odomain, 1L)
        } else {
            new_domain <- stack@domain
        }
        layout_update_domain(
            layout,
            direction = to_direction(position),
            domain = new_domain,
            objectname = objectname
        )
    }

#######################################################
# used to add elements for main body
#' @keywords internal
quad_body_add <- function(object, quad, objectname) {
    UseMethod("quad_body_add")
}

#' @importFrom ggplot2 ggplot_add
#' @export
quad_body_add.default <- function(object, quad, objectname) {
    quad@plot <- ggplot_add(object, ggfun("plot_clone")(quad@plot), objectname)
    quad
}

#' @export
quad_body_add.Coord <- function(object, quad, objectname) {
    if (!inherits(object, "CoordCartesian")) {
        cli_warn(c(
            "only {.field cartesian coordinate} is supported",
            i = "will discard {.var {objectname}} directly"
        ))
        return(quad)
    }
    NextMethod() # call default method
}

#' @export
quad_body_add.layout_theme <- function(object, quad, objectname) {
    attr(quad, "theme") <- layout_theme_update(quad@theme, object)
    quad
}

#' @export
`quad_body_add.ggalign::Scheme` <- function(object, quad, objectname) {
    quad@body_schemes <- scheme_update(quad@body_schemes, object, objectname)
    quad
}


##############################################################
# `subtract` operates at layout-level
#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_subtract, list(QuadLayout, S7::class_any)) <-
    function(layout, object, objectname) {
        if (is.null(context <- layout@current)) context <- c(.TLBR, list(NULL))
        for (active in context) {
            if (is.null(active)) {
                layout <- quad_body_add(object, layout, objectname)
            } else if (!is.null(prop(layout, active))) {
                prop(layout, active) <- layout_subtract(
                    prop(layout, active), object, objectname
                )
            }
        }
        layout
    }

# for object can set at layout level
#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_subtract, list(QuadLayout, Scheme)) <-
    function(layout, object, objectname) {
        if (is.null(context <- layout@current)) {
            layout <- update_layout_schemes(object, layout, objectname)
        } else {
            prop(layout, context) <- update_layout_schemes(
                object, prop(layout, context), objectname
            )
        }
        layout
    }

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_subtract, list(QuadLayout, QuadScope)) <-
    function(layout, object, objectname) {
        inner_name <- prop(object, "object_name")
        inner <- prop(object, "object")
        contexts <- quad_scope_contexts(object, layout@current)
        # `subtract` operates at layout-level
        if (is.null(contexts)) {
            layout <- layout_subtract(layout, inner, inner_name)
        } else {
            for (active in contexts) {
                if (is.null(active)) {
                    layout <- quad_body_add(inner, layout, inner_name)
                } else if (!is.null(prop(layout, active))) {
                    prop(layout, active) <- layout_subtract(
                        prop(layout, active), inner, inner_name
                    )
                }
            }
        }
        layout
    }

###############################################################
#' @include layout-.R
#' @include layout-operator.R
#' @include layout-quad-scope.R
S7::method(layout_and_add, list(QuadLayout, QuadScope)) <-
    function(layout, object, objectname) {
        object <- prop(object, "object")
        object_name <- prop(object, "object_name")
        layout_and_add(object, layout, object_name)
    }

quad_and_add <- function(layout, object, objectname) {
    layout <- quad_body_add(object, layout, objectname)
    for (position in .TLBR) {
        stack <- prop(layout, position)
        if (is.null(stack)) next # no annotation
        prop(layout, position) <- layout_and_add(stack, object, objectname)
    }
    layout
}

#' @include layout-.R
#' @include layout-operator.R
S7::method(layout_and_add, list(QuadLayout, S7::class_any)) <- quad_and_add

#' @include layout-.R
#' @include layout-operator.R
#' @importFrom S7 super
S7::method(layout_and_add, list(QuadLayout, S3_class_theme)) <-
    function(layout, object, objectname) {
        ans <- quad_and_add(layout, object, objectname)
        # to align with `patchwork`, we also modify the layout theme
        # when using `&` to add the theme object.
        ans@theme <- ggfun("add_theme")(ans@theme, object)
        ans
    }
