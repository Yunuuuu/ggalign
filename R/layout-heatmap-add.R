#' @keywords internal
layout_heatmap_add <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_add")
}

# Add elements for heatmap or annotation
#' @importFrom methods slot slot<-
#' @export
layout_heatmap_add.default <- function(object, heatmap, object_name) {
    # if no active context, we directly add it into the main heatmap
    if (is.null(position <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
    } else {
        slot(heatmap, position) <- layout_stack_add(
            object, slot(heatmap, position), object_name
        )
    }
    heatmap
}

##################################################################
#' @export
layout_heatmap_add.Align <- function(object, heatmap, object_name) {
    if (is.null(position <- get_context(heatmap))) {
        cli::cli_abort(c(
            "Cannot add {.var {object_name}}",
            i = "No active heatmap annotation",
            i = "try to activate an active annotation stack with {.fn hmanno}"
        ))
    }

    # add annotation -----------------------------
    stack <- layout_stack_add(object, slot(heatmap, position), object_name)
    slot(heatmap, position) <- stack

    # set the panel and index for the heatmap
    direction <- to_direction(position)
    axis <- to_coord_axis(direction)
    heatmap <- set_panel(heatmap, axis = axis, value = get_panel(stack))
    heatmap <- set_index(heatmap, axis = axis, value = get_index(stack))
    heatmap <- set_nobs(heatmap, axis = axis, value = get_nobs(stack))
    heatmap
}

#' @export
layout_heatmap_add.heatmap_active <- function(object, heatmap, object_name) {
    position <- .subset2(object, "position")
    heatmap <- set_context(heatmap, position)
    if (is.null(position)) {
        if (!is.null(width <- .subset2(object, "width"))) {
            heatmap@width <- width
        }
        if (!is.null(height <- .subset2(object, "height"))) {
            heatmap@height <- height
        }
        heatmap@action <- update_action(
            heatmap@action, .subset2(object, "action")
        )
        return(heatmap)
    }

    direction <- to_direction(position)
    axis <- to_coord_axis(direction)

    if (is.null(get_nobs(heatmap, axis))) {
        cli::cli_abort(c(
            "{.arg data} of heatmap layout is not initialized",
            i = "Did you want to add this heatmap in a stack layout?"
        ))
    }

    # initialize the annotation stack ------------
    if (is.null(stack <- slot(heatmap, position))) {
        data <- heatmap@data
        if (!is_horizontal(direction)) {
            data <- restore_attr_ggalign(t(data), data)
        }
        stack <- .stack_layout(data,
            # if inherit from the parent layout data, we'll inherit
            # the action data function, but now, no parent layout
            action_data = waiver(),
            direction = direction
        )
        stack@heatmap$position <- position
        stack <- set_panel(stack, value = get_panel(heatmap, axis))
        stack <- set_index(stack, value = get_index(heatmap, axis))
    }
    # update parameters --------------------------
    if (!is.null(size <- .subset2(object, "size"))) {
        stack@heatmap$size <- size
    }
    if (!is.waive(free_guides <- .subset2(object, "free_guides"))) {
        stack@heatmap$free_guides <- free_guides
    }
    stack@action <- update_action(stack@action, .subset2(object, "action"))
    slot(heatmap, position) <- stack
    heatmap
}

#' @export
layout_heatmap_add.list <- function(object, heatmap, object_name) {
    for (o in object) {
        heatmap <- layout_heatmap_add(o, heatmap, object_name)
    }
    heatmap
}

##############################################################
# Preventing from adding following elements
#' @export
layout_heatmap_add.matrix <- function(object, heatmap, object_name) {
    cli::cli_abort("Can't change data in {.fn ggheatmap} object")
}

#' @export
layout_heatmap_add.data.frame <- layout_heatmap_add.matrix

#' @export
layout_heatmap_add.ggplot <- function(object, heatmap, object_name) {
    cli::cli_abort(c(
        "Cannot add {.var {object_name}} into the heatmap layout",
        i = "try to use {.fn ggalign} to initialize the {.cls ggplot}"
    ))
}

#######################################################
# used to add elements for heatmap
#' @keywords internal
heatmap_add <- function(object, heatmap, object_name) UseMethod("heatmap_add")

#' @importFrom ggplot2 ggplot_add
#' @export
heatmap_add.default <- function(object, heatmap, object_name) {
    heatmap@plot <- ggplot_add(object, heatmap@plot, object_name)
    heatmap
}

#' @export
heatmap_add.Coord <- function(object, heatmap, object_name) {
    if (!inherits(object, "CoordCartesian")) {
        cli::cli_warn(c(
            "only {.field cartesian coordinate} is supported",
            i = "will discard {.var {object_name}} directly"
        ))
        return(heatmap)
    }
    NextMethod() # call default method
}

#' @export
heatmap_add.layout_annotation <- function(object, heatmap, object_name) {
    heatmap@annotation <- update_non_waive(
        heatmap@annotation, .subset2(object, "annotation")
    )
    heatmap@theme <- update_layout_theme(
        heatmap@theme, .subset2(object, "theme")
    )
    heatmap
}

#' @export
heatmap_add.plot_action <- function(object, heatmap, object_name) {
    heatmap@body_action <- update_action(heatmap@body_action, object)
    heatmap
}
