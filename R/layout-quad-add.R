#' @keywords internal
quad_layout_add <- function(object, quad, object_name) {
    UseMethod("quad_layout_add")
}

#############################################################
#' @export
quad_layout_add.layout_title <- function(object, quad, object_name) {
    quad@titles <- update_layout_title(quad@titles, object)
    quad
}

##############################################################
# Preventing from adding following elements
#' @export
quad_layout_add.matrix <- function(object, quad, object_name) {
    cli_abort(sprintf("Can't change data of %s", object_name(quad)))
}

#' @export
quad_layout_add.data.frame <- quad_layout_add.matrix

#############################################################
# Add elements for the main body or the annotation
#' @importFrom methods slot slot<-
#' @export
quad_layout_add.default <- function(object, quad, object_name) {
    if (is.null(position <- quad@active)) {
        quad <- quad_body_add(object, quad, object_name)
    } else if (is.null(stack <- slot(quad, position))) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "the {.field {position}} annotation stack is not initialized",
            i = "Try to use {.code quad_anno(initialize = TRUE)} or you can add a {.code stack_layout()} manually"
        ))
    } else {
        slot(quad, position) <- chain_layout_add(object, stack, object_name)
    }
    quad
}

#' @export
quad_layout_add.list <- function(object, quad, object_name) {
    for (o in object) quad <- quad_layout_add(o, quad, object_name)
    quad
}

#' @export
quad_layout_add.NULL <- function(object, quad, object_name) {
    quad
}

#' @export
quad_layout_add.ggalign_with_quad <- function(object, quad, object_name) {
    old <- quad@active
    contexts <- quad_operated_context(object, old, "+") %||%
        list(NULL) # we wrap `NULL` to a list for `for loop`.
    object <- .subset2(object, "object")
    object_name <- .subset2(object, "object_name")
    for (active in contexts) {
        quad@active <- active
        quad <- quad_layout_add(object, quad, object_name)
    }
    quad@active <- old
    quad
}

##################################################################
#' @export
quad_layout_add.quad_active <- function(object, quad, object_name) {
    if (!is.null(width <- .subset2(object, "width"))) {
        quad@width <- width
    }
    if (!is.null(height <- .subset2(object, "height"))) {
        quad@height <- height
    }
    quad@active <- NULL
    quad
}

#' @importFrom methods slot
#' @export
quad_layout_add.quad_anno <- function(object, quad, object_name) {
    position <- .subset2(object, "position")
    initialize <- .subset2(object, "initialize")
    stack <- slot(quad, position)
    if (is.null(stack) && !isFALSE(initialize)) {
        # try to initialize the annotation stack with the layout data
        direction <- to_direction(position)
        layout_design <- slot(quad, direction)
        # for the annotation stack, we try to take the data from the
        # quad layout
        quad_data <- quad@data
        data <- waiver() # use waiver() to indicate data is not initialized
        quad_matrix <- FALSE # the default value for `quad_matrix` in the stack
        if (is_continuous_design(layout_design)) { # the stack need a data frame
            if (!is.data.frame(quad_data)) {
                if (is.null(initialize)) {
                    cli_warn(paste(
                        "`data` in {.fn {quad@name}} is",
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
                    "`data` in {.fn {quad@name}} is",
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
                design = layout_design,
                # we'll inherit the action data function when
                schemes = new_schemes(if (is.null(data)) NULL else waiver())
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
        slot(quad, position) <- stack
    }
    quad@active <- position
    quad
}

#' @export
quad_layout_add.QuadLayout <- function(object, quad, object_name) {
    cli_abort(c(
        sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
        i = "Did you mean to place multiple {.fn quad_layout} elements inside a {.fn stack_layout}?"
    ))
}

#' @export
quad_layout_add.StackLayout <- function(object, quad, object_name) {
    # we check if there is an active annotation
    if (is.null(position <- quad@active)) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "no active annotation stack",
            i = "try to activate an annotation stack with {.fn anno_*}"
        ))
    }
    # check the annotation stack is not initialized
    if (!is.null(slot(quad, position))) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "{position} annotation stack already exists"
        ))
    }
    # cannot contain nested layout
    if (!all(vapply(object@plot_list, is_craftbox, logical(1L),
                    USE.NAMES = FALSE))) { # styler: off
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "annotation stack cannot contain nested layout"
        ))
    }
    # check quad layout is compatible with stack layout
    if (!identical(direction <- to_direction(position), object@direction)) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "only {.field {direction}} stack is allowed in {position} annotation"
        ))
    }
    if (length(object@sizes) > 1L) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "{.arg sizes} must be of length one to use the stack as an annotation"
        ))
    }
    quad_design <- slot(quad, direction)
    if (is_cross_layout(object) &&
        any(position == c("bottom", "right")) &&
        !is_empty(object@cross_points)) {
        # if there are cross points in bottom or right annotation,
        # use the first design
        stack_design <- .subset2(object@odesign, 1L)
    } else {
        stack_design <- object@design
    }
    # check if we can align in this direction
    # `stack_layout()` is free from aligning obervations in this axis
    if (is_continuous_design(stack_design)) {
        if (!is_continuous_design(quad_design)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(quad)
                ),
                i = sprintf(
                    "%s cannot align continuous variable in {.field {direction}} direction",
                    object_name(quad)
                )
            ))
        }
        layout_design <- stack_design
    } else if (is_discrete_design(quad_design)) {
        layout_design <- melt_discrete_design(
            quad_design, stack_design,
            old_name = object_name(quad),
            new_name = object_name
        )
    } else {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = sprintf(
                "%s cannot align discrete variable in {.field {direction}} direction",
                object_name(quad)
            )
        ))
    }
    object@heatmap$position <- position
    slot(quad, position) <- object
    update_design(quad,
        direction = direction,
        design = layout_design,
        object_name = object_name
    )
}

#' @importFrom methods slot slot<-
#' @export
quad_layout_add.CraftBox <- function(object, quad, object_name) {
    if (is.null(position <- quad@active)) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "no active annotation stack",
            i = "try to activate an annotation stack with {.fn anno_*}"
        ))
    }
    if (is.null(stack <- slot(quad, position))) {
        cli_abort(c(
            sprintf("Cannot add {.var {object_name}} to %s", object_name(quad)),
            i = "the {.field {position}} annotation stack is not initialized",
            i = "Try to use {.code quad_anno(initialize = TRUE)} or you can add a {.code stack_layout()} manually"
        ))
    }

    # add annotation -----------------------------
    stack <- chain_layout_add(object, stack, object_name)
    slot(quad, position) <- stack

    # if there are cross points in bottom or right annotation, we use
    # the first design
    if (is_cross_layout(stack) &&
        any(position == c("bottom", "right")) &&
        !is_empty(stack@cross_points)) {
        new_design <- .subset2(stack@odesign, 1L)
    } else {
        new_design <- stack@design
    }

    update_design(
        quad,
        direction = to_direction(position),
        design = new_design,
        object_name = object_name
    )
}

#######################################################
# used to add elements for main body
#' @keywords internal
quad_body_add <- function(object, quad, object_name) {
    UseMethod("quad_body_add")
}

#' @importFrom ggplot2 ggplot_add
#' @export
quad_body_add.default <- function(object, quad, object_name) {
    quad@plot <- ggplot_add(object, ggfun("plot_clone")(quad@plot), object_name)
    quad
}

#' @export
quad_body_add.Coord <- function(object, quad, object_name) {
    if (!inherits(object, "CoordCartesian")) {
        cli_warn(c(
            "only {.field cartesian coordinate} is supported",
            i = "will discard {.var {object_name}} directly"
        ))
        return(quad)
    }
    NextMethod() # call default method
}

#' @export
quad_body_add.layout_annotation <- function(object, quad, object_name) {
    update_layout_annotation(object, quad, object_name)
}

#' @export
quad_body_add.ggalign_scheme <- function(object, quad, object_name) {
    name <- ggalign_scheme_name(object)
    quad@body_schemes[name] <- list(update_scheme(
        object, .subset2(quad@body_schemes, name), object_name
    ))
    quad
}
