#' @keywords internal
layout_heatmap_add <- function(object, heatmap, object_name) {
    UseMethod("layout_heatmap_add")
}

#' @export
layout_heatmap_add.default <- function(object, heatmap, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} to {.cls ggheatmap}")
}

#' @export
layout_heatmap_add.Align <- function(object, heatmap, object_name) {
    if (is.null(position <- get_context(heatmap))) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}}",
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
            heatmap@params$width <- width
        }
        if (!is.null(height <- .subset2(object, "height"))) {
            heatmap@params$height <- height
        }
        heatmap <- layout_add_active(
            .subset2(object, "active"), heatmap, object_name
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
        if (!is_horizontal(direction)) data <- t(data)
        stack <- stack_layout(data = data, direction = direction)
        stack <- set_panel(stack, value = get_panel(heatmap, axis))
        stack <- set_index(stack, value = get_index(heatmap, axis))
    }
    if (!is.null(size <- .subset2(object, "size"))) {
        stack@params$size <- size
    }
    slot(heatmap, position) <- layout_stack_add.stack_active(
        .subset(object, c("what", "active")), stack, object_name
    )
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
