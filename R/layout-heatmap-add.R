#' @keywords internal
layout_heatmap_add <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_add")
}

#' @export
layout_heatmap_add.default <- function(object, heatmap, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} to {.cls ggheatmap}")
}

#' @export
layout_heatmap_add.NULL <- function(object, heatmap, object_name) heatmap

#' @export
layout_heatmap_add.Align <- function(object, heatmap, object_name) {
    if (is.null(position <- get_context(heatmap))) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}}",
            i = "No active heatmap annotation",
            i = "try to activate an annotation with {.fn hmanno}"
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
    if (is.na(object)) {
        heatmap <- set_context(heatmap, NULL)
        if (!identical(guides <- attr(object, "guides"), NA)) {
            heatmap@params$guides <- guides
        }
        if (!identical(free_labs <- attr(object, "free_labs"), NA)) {
            heatmap@params$free_labs <- free_labs
        }
        if (!identical(free_spaces <- attr(object, "free_spaces"), NA)) {
            heatmap@params$free_spaces <- free_spaces
        }
        if (!is.null(width <- attr(object, "width"))) {
            heatmap@params$width <- width
        }
        if (!is.null(height <- attr(object, "height"))) {
            heatmap@params$height <- height
        }
        if (!identical(plot_data <- attr(object, "plot_data"), NA)) {
            heatmap@params$plot_data <- plot_data
        }
        if (!is.null(theme <- attr(object, "theme"))) {
            heatmap@theme <- heatmap@theme + theme
        }
        return(heatmap)
    }

    heatmap <- set_context(heatmap, object)
    direction <- to_direction(object)
    axis <- to_coord_axis(direction)

    if (is.null(get_nobs(heatmap, axis))) {
        cli::cli_abort(c(
            "{.arg data} of heatmap layout is not initialized",
            i = "Did you want to add this heatmap in a stack layout?"
        ))
    }

    # initialize the annotation stack ------------
    if (is.null(stack <- slot(heatmap, object))) {
        data <- heatmap@data
        if (!is_horizontal(direction)) data <- t(data)
        stack <- layout_stack(data = data, direction = direction)
        stack <- set_panel(stack, value = get_panel(heatmap, axis))
        stack <- set_index(stack, value = get_index(heatmap, axis))

        # Initialize the stack size
        # it is the total size of the stack in the stack direction.
        # In `params` slot, we'll also have a `sizes` indicates the relative
        # size of the vertical direction with this stack, which won't be used by
        # heatmap annotation, since heatmap annotation only allow `Align`
        # object.
        stack@params$size <- unit(NA, "null")
    }

    if (!is.null(size <- attr(object, "size"))) {
        stack@params$size <- size
    }
    if (!identical(guides <- attr(object, "guides"), NA)) {
        stack@params$guides <- guides
    }
    if (!identical(free_labs <- attr(object, "free_labs"), NA)) {
        stack@params$free_labs <- free_labs
    }
    if (!identical(free_spaces <- attr(object, "free_spaces"), NA)) {
        stack@params$free_spaces <- free_spaces
    }
    if (!identical(plot_data <- attr(object, "plot_data"), NA)) {
        stack@params$plot_data <- plot_data
    }
    slot(heatmap, object) <- stack
    heatmap
}

#' @export
layout_heatmap_add.list <- function(object, heatmap, object_name) {
    for (o in object) {
        heatmap <- heatmap + o
    }
    heatmap
}

#############################################################
# Add elements for heatmap or annotation
#' @importFrom methods slot slot<-
#' @export
layout_heatmap_add.gg <- function(object, heatmap, object_name) {
    # if no active context, we directly add it into the main heatmap
    if (is.null(position <- get_context(heatmap))) {
        heatmap <- heatmap_add(object, heatmap, object_name)
    } else {
        slot(heatmap, position) <- stack_add_ggelement(
            object, slot(heatmap, position), object_name,
            sprintf("the heatmap %s annotation", position)
        )
    }
    heatmap
}

#' @export
layout_heatmap_add.labels <- layout_heatmap_add.gg

#' @export
layout_heatmap_add.facetted_pos_scales <- layout_heatmap_add.gg

##############################################################
# Preventing from adding following elements
#' @export
layout_heatmap_add.matrix <- function(object, heatmap, object_name) {
    cli::cli_abort("Can't change data in {.cls ggheatmap} object")
}

#' @export
layout_heatmap_add.data.frame <- layout_heatmap_add.matrix

#' @export
layout_heatmap_add.ggplot <- function(object, heatmap, object_name) {
    cli::cli_abort(c(
        "Cannot add {.code {object_name}} into the heatmap layout",
        i = "try to use {.fn ggalign} to initialize a {.cls ggplot} object"
    ))
}

#######################################################
# used to add elements for heatmap
#' @keywords internal
heatmap_add <- function(object, heatmap, object_name) UseMethod("heatmap_add")

#' @export
heatmap_add.gg <- function(object, heatmap, object_name) {
    heatmap@plot <- ggplot2::ggplot_add(
        object, heatmap@plot, object_name
    )
    heatmap
}

#' @export
heatmap_add.labels <- heatmap_add.gg

#' @export
heatmap_add.facetted_pos_scales <- function(object, heatmap, object_name) {
    assert_facetted_scales(object, object_name, "the heatmap layout")
    heatmap@facetted_pos_scales <- object
    heatmap
}
