#' @keywords internal
quad_layout_add <- function(object, quad, object_name) {
    UseMethod("quad_layout_add")
}

# Add elements for the main body or the annotation
#' @importFrom methods slot slot<-
#' @export
quad_layout_add.default <- function(object, quad, object_name) {
    # if no active context, we directly add it into the main body
    if (is.null(position <- quad@active)) {
        quad <- quad_body_add(object, quad, object_name)
    } else {
        slot(quad, position) <- stack_layout_add(
            object, slot(quad, position), object_name
        )
    }
    quad
}

#' @export
quad_layout_add.ggalign_option <- quad_layout_add.default

#' @export
quad_layout_add.list <- function(object, quad, object_name) {
    for (o in object) quad <- quad_layout_add(o, quad, object_name)
    quad
}

#' @export
quad_layout_add.layout_title <- function(object, quad, object_name) {
    quad@titles <- update_non_waive(quad@titles, object)
    quad
}

##############################################################
# Preventing from adding following elements
#' @export
quad_layout_add.matrix <- function(object, quad, object_name) {
    cli::cli_abort("Can't change data of {.fn {quad@name}}")
}

#' @export
quad_layout_add.data.frame <- quad_layout_add.matrix

##################################################################
#' @export
quad_layout_add.align <- function(object, quad, object_name) {
    if (is.null(position <- quad@active)) {
        cli::cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "no active annotation stack",
            i = "try to activate an annotation stack with {.fn anno_*}"
        ))
    }
    direction <- to_direction(position)
    # check if we can align in this direction
    if (is.null(slot(quad, direction))) {
        cli::cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = paste(
                "{.fn {quad@name}} cannot align observations",
                "in {.field {direction}} direction"
            )
        ))
    }

    # add annotation -----------------------------
    stack <- stack_layout_add(object, slot(quad, position), object_name)
    slot(quad, position) <- stack
    update_layout_params(quad, direction = direction, params = stack@layout)
}

#' @export
quad_layout_add.free_gg <- function(object, quad, object_name) {
    if (is.null(position <- quad@active)) {
        cli::cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "no active annotation stack",
            i = "try to activate an annotation stack with {.fn anno_*}"
        ))
    }

    # add annotation -----------------------------
    stack <- stack_layout_add(object, slot(quad, position), object_name)
    slot(quad, position) <- stack
    quad
}

#' @export
quad_layout_add.ggplot <- quad_layout_add.free_gg

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
    if (is.null(stack <- slot(quad, position))) {
        # try to initialize the annotation stack with the layout data
        direction <- to_direction(position)
        layout <- slot(quad, direction)
        # for the annotation stack, we try to take the data from the
        # quad layout
        if (is.function(quad_data <- quad@data) || is.null(quad_data)) {
            cli::cli_abort(c(
                "{.arg data} of {.fn {quad@name}} is not initialized",
                i = "Did you want to add {.fn {quad@name}} to a stack layout?"
            ))
        }
        data <- quad_data
        if (is.null(layout)) { # the stack need a data frame
            if (!is.data.frame(quad_data)) {
                cli::cli_abort(c(
                    "Cannot initialize the {.field {position}} annotation stack.",
                    i = paste(
                        "`data` in {.fn {quad@name}} is",
                        "{.obj_type_friendly {quad_data}},",
                        "but we need a {.cls data.frame}."
                    ),
                    i = "Try using {.fn quad_init} to initialize the {position} annotation with self-customized data."
                ))
            }
        } else if (is.matrix(quad_data)) { # the stack need a matrix
            if (!is_horizontal(direction)) {
                data <- restore_attr_ggalign(t(data), data)
            }
        } else { # this shouldn't occur
            cli::cli_abort(c(
                "Cannot initialize the {.field {position}} annotation stack.",
                i = paste(
                    "`data` in {.fn {quad@name}} is",
                    "{.obj_type_friendly {quad_data}},",
                    "but we need a {.cls matrix}."
                ),
                i = "Try using {.fn quad_init} to initialize the {position} annotation with self-customized data."
            ))
        }
        stack <- new_stack_layout( # initialize the annotation stack
            name = if (is.null(layout)) "stack_free" else "stack_align",
            data = data,
            direction = direction,
            # restore the alinged parameters from the QuadLayout
            layout = layout,
            # we'll inherit the action data function
            controls = new_controls(plot_data(waiver()))
        )
        stack@heatmap$position <- position
    }

    # update parameters --------------------------
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
    quad@active <- position
    quad
}

#' @importFrom rlang inject
#' @export
quad_layout_add.quad_init <- function(object, quad, object_name) {
    position <- .subset2(object, "position")
    if (!is.null(slot(quad, position))) {
        cli::cli_abort(c(
            "Cannot initialize the {position} annotation",
            i = "{position} annotation stack has already been initialized"
        ))
    }
    direction <- to_direction(position)
    layout <- slot(quad, direction)
    stack_data <- .subset2(object, "data")
    if (is.waive(stack_data) || is.function(stack_data)) {
        if (is.function(quad_data <- quad@data) || is.null(quad_data)) {
            cli::cli_abort(c(
                "{.arg data} of {.fn {quad@name}} is not initialized",
                i = "Did you want to add {.fn {quad@name}} to a stack layout?"
            ))
        } else if (is.matrix(quad_data)) {
            if (!is_horizontal(direction)) {
                quad_data <- restore_attr_ggalign(t(quad_data), quad_data)
            }
        }
        if (is.function(stack_data)) {
            action_data <- NULL
            # we'll apply with the layout data
            stack_data <- stack_data(quad_data)
            # check the returned data satisfied
            if (is.null(layout)) { # the stack need a data frame
                if (!is.data.frame(stack_data)) {
                    cli::cli_abort(paste(
                        "{.arg data} in {.var {object_name}} must return",
                        "a {.cls data.frame}"
                    ))
                }
            } else if (!is.matrix(stack_data)) { # the stack need a matrix
                cli::cli_abort(paste(
                    "{.arg data} in {.var {object_name}} must return",
                    "a {.cls matrix}"
                ))
            } else {
                layout_nobs <- .subset2(layout, "nobs")
                # we check the observations is compatible with the
                # [quad_layout()]
                if (!is.null(layout_nobs) && nrow(stack_data) != layout_nobs) {
                    cli::cli_abort(paste(
                        "number of observations of {.arg data} in {.var {object_name}}",
                        "is not compatible with the parent {.fn {quad@name}}"
                    ))
                }
                layout$nobs <- nrow(stack_data)
            }
        } else {
            stack_data <- quad_data
            action_data <- waiver()
            # check the inherited data satisfied
            if (is.null(layout)) { # the stack need a data frame
                if (!is.data.frame(stack_data)) {
                    cli::cli_abort(c(
                        "Cannot initialize the {.field {position}} annotation stack",
                        i = paste(
                            "`data` in {.fn {quad@name}} is",
                            "{.obj_type_friendly {quad_data}}",
                            "but we need a {.cls data.frame}"
                        )
                    ))
                }
            } else if (!is.matrix(stack_data)) { # the stack need a matrix
                cli::cli_abort(c(
                    "Cannot initialize the {.field {position}} annotation stack",
                    i = paste(
                        "`data` in {.fn {quad@name}} is",
                        "{.obj_type_friendly {quad_data}}",
                        "but we need a {.cls matrix}"
                    )
                ))
            }
        }
    } else if (is.null(stack_data)) {
        action_data <- NULL
    } else if (is.null(layout)) { # the stack need a data frame
        stack_data <- inject(fortify_data_frame(
            data = stack_data,
            !!!.subset2(object, "params")
        ))
        action_data <- NULL
    } else { # the stack need a matrix
        stack_data <- inject(fortify_matrix(
            data = stack_data,
            !!!.subset2(object, "params")
        ))
        action_data <- NULL
        layout_nobs <- .subset2(layout, "nobs")
        # we check the observations is compatible with the [quad_layout()]
        if (!is.null(layout_nobs) && nrow(stack_data) != layout_nobs) {
            cli::cli_abort(paste(
                "number of observations of {.arg data} in {.var {object_name}}",
                "is not compatible with the parent {.fn {quad@name}}"
            ))
        }
        layout$nobs <- nrow(stack_data)
    }
    slot(quad, position) <- new_stack_layout( # initialize the annotation stack
        name = if (is.null(layout)) "stack_free" else "stack_align",
        data = stack_data,
        direction = direction,
        # restore the alinged parameters from the QuadLayout
        layout = layout,
        # we'll inherit the action data function
        controls = new_controls(plot_data(action_data))
    )
    if (!is.null(layout)) {
        quad <- update_layout_params(quad,
            direction = direction, params = layout
        )
    }
    quad@active <- position
    quad
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
        cli::cli_warn(c(
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
quad_body_add.ggalign_option <- function(object, quad, object_name) {
    name <- ggalign_option_name(object)
    quad@body_controls[name] <- list(update_option(
        object, .subset2(quad@body_controls, name), object_name
    ))
    quad
}
