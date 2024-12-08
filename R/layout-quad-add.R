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
    cli_abort("Can't change data of {.fn {quad@name}}")
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
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "the {.field {position}} annotation stack is not initialized",
            i = "Try to use {.code quad_anno(initialize = TRUE)} or you can add a {.code stack_layout()} manually"
        ))
    } else {
        slot(quad, position) <- stack_layout_add(object, stack, object_name)
    }
    quad
}

#' @export
quad_layout_add.list <- function(object, quad, object_name) {
    for (o in object) quad <- quad_layout_add(o, quad, object_name)
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
        layout_coords <- slot(quad, direction)
        # for the annotation stack, we try to take the data from the
        # quad layout
        quad_data <- quad@data
        data <- waiver()
        if (is.null(layout_coords)) { # the stack need a data frame
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
                data <- ggalign_attr_restore(t(quad_data), quad_data)
            }
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
                layout = layout_coords,
                # we'll inherit the action data function when
                schemes = new_schemes(
                    scheme_data(if (is.null(data)) NULL else waiver())
                )
            )
            stack@heatmap$position <- position
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
            stack@heatmap$size <- size
        }
        if (!is.waive(free_guides <- .subset2(object, "free_guides"))) {
            stack@heatmap$free_guides <- free_guides
        }
        stack <- update_stack_active(
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
        "Cannot add {.var {object_name}} to {.fn {quad@name}}.",
        i = "Did you mean to place multiple {.fn quad_layout} elements inside a {.fn stack_layout}?"
    ))
}

#' @export
quad_layout_add.StackLayout <- function(object, quad, object_name) {
    # we check if there is an active annotation
    if (is.null(position <- quad@active)) {
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "no active annotation stack",
            i = "try to activate an annotation stack with {.fn anno_*}"
        ))
    }
    # check the annotation stack is not initialized
    if (!is.null(slot(quad, position))) {
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "{position} annotation stack already exists"
        ))
    }
    # cannot contain nested layout
    if (!all(vapply(object@plot_list, is_ggalign_plot, logical(1L),
                    USE.NAMES = FALSE))) { # styler: off
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "annotation stack cannot contain nested layout"
        ))
    }
    # check quad layout is compatible with stack layout
    if (!identical(direction <- to_direction(position), object@direction)) {
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "only {.field {direction}} stack is allowed in {position} annotation"
        ))
    }
    quad_coords <- slot(quad, direction)
    stack_coords <- object@layout
    # check if we can align in this direction
    # `stack_layout()` is free from aligning obervations in this axis
    if (is.null(stack_coords)) {
        layout_coords <- NULL
    } else if (!is.null(quad_coords)) {
        # both `quad_layout()` and `stack_layout()` need align observations
        # if there are cross points in bottom or right annotation, the index
        # should be the first index in the `index_list`
        if (any(position == c("bottom", "right")) &&
            is_cross_layout(object) &&
            !is_empty(object@cross_points)) {
            stack_coords["index"] <- list(.subset2(object@index_list, 1L))
        }
        layout_coords <- check_layout_coords(
            quad_coords, stack_coords,
            old_name = object_name(quad),
            new_name = object_name
        )
    } else {
        cli_abort(c(
            "Cannot add {.var {object_name}} to a {.fn {quad@name}}",
            i = "{.fn {quad@name}} cannot align observations in {.field {direction}} direction"
        ))
    }
    object@heatmap$position <- position
    slot(quad, position) <- object
    update_layout_coords(quad,
        direction = direction,
        coords = layout_coords,
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
    quad@plot <- ggplot_add(object, quad@plot, object_name)
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
