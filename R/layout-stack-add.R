#' Add components to `LayoutStack`
#'
#' @param e1 A [LayoutStack][layout_stack] object.
#' @param e2 An object to be added to the plot, including [gg][ggplot2::+.gg]
#' elements, [align] object, or [layout_heatmap] object.
#' @return A modified `LayoutStack` object.
#' @examples
#' layout_stack(matrix(rnorm(81), nrow = 9)) +
#'     ggalign() +
#'     geom_point(aes(x = value))
#' @name stack-add
#' @aliases +.ggstack +.LayoutStack
#' @seealso layout_stack_add
NULL

#' @rdname stack-add
#' @export
methods::setMethod("+", c("LayoutStack", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli::cli_abort(c(
            "Cannot use {.code +} with a single argument.",
            "i" = "Did you accidentally put {.code +} on a new line?"
        ))
    }
    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    layout_stack_add(e2, e1, e2name)
})

#' @keywords internal
layout_stack_add <- function(object, stack, object_name) {
    UseMethod("layout_stack_add")
}

#' @importFrom methods slot slot<-
#' @export
layout_stack_add.Align <- function(object, stack, object_name) {
    new <- list(object)
    plots <- slot(stack, "plots")

    # check annotation name is unique --------------------
    if (!is.null(name <- .subset2(object, "name"))) {
        if (any(names(plots) == name)) {
            cli::cli_warn("{object_name}: {name} plot is already present")
            plots[[name]] <- NULL
        }
        names(new) <- name
    }
    if (.subset2(object, "set_context")) {
        active_index <- length(plots) + 1L
    } else {
        active_index <- get_context(stack)
    }

    # make layout ----------------------------------------
    # this step the object will act with the stack layout
    # group rows into panels or reorder rows
    params <- slot(stack, "params")
    layout <- initialize_align(
        object,
        slot(stack, "direction"),
        layout_data = slot(stack, "data"),
        layout_labels = .subset2(params, "labels"),
        layout_nudge = .subset2(params, "labels_nudge"),
        layout_panels = get_panels(stack),
        layout_index = get_index(stack)
    )

    # add annotation -------------------------------------
    slot(stack, "plots") <- c(plots, new)
    slot(stack, "active") <- active_index

    # set the layout -------------------------------------
    stack <- set_panels(stack, .subset2(layout, 1L))
    stack <- set_index(stack, .subset2(layout, 2L))
    stack
}

#' @importFrom methods slot
#' @export
layout_stack_add.LayoutHeatmap <- function(object, stack, object_name) {
    axis <- to_coord_axis(slot(stack, "direction"))

    heatmap_panels <- get_panels(object, axis)
    stack_panels <- get_panels(stack)
    if (is.null(heatmap_panels)) {
        panels <- stack_panels
    } else if (is.null(stack_panels)) {
        panels <- heatmap_panels
    } else if (!identical(heatmap_panels, stack_panels)) {
        cli::cli_abort(paste(
            "{.code {object_name}} disrupt the previously",
            "established layout panels of the stack"
        ))
    } else {
        panels <- stack_panels
    }
    heatmap_index <- get_index(object, axis)
    stack_index <- get_index(stack)
    if (is.null(heatmap_index)) {
        index <- stack_index
    } else if (is.null(stack_index)) {
        index <- heatmap_index
    } else if (!identical(heatmap_index, stack_index)) {
        cli::cli_abort(paste(
            "{.code {object_name}} disrupt the previously",
            "established layout index of the stack"
        ))
    } else {
        index <- stack_index
    }

    # add annotation -------------------------------------
    if (identical(slot(object, "params")$guides, "collect") &&
        identical(slot(stack, "params")$guides, "collect")) {
        slot(object, "params")$guides <- NULL
    }
    # we won't change the active context for heatmap
    slot(stack, "plots") <- c(slot(stack, "plots"), list(object))

    # set the layout -------------------------------------
    stack <- set_panels(stack, panels)
    stack <- set_index(stack, index)
    stack
}

stack_add_ggelement <- function(object, stack, object_name, name) {
    if (is.null(active_plot <- get_context(stack))) {
        cli::cli_abort(c(
            sprintf("Cannot add {.code {object_name}} to %s", name),
            i = "No active {.cls ggplot} object",
            i = paste(
                "Did you forget to initialize a {.cls ggplot} object",
                "with {.fn align_*}?"
            )
        ))
    }
    plot <- slot(stack, "plots")[[active_plot]]
    if (is.ggheatmap(plot)) {
        plot <- layout_heatmap_add(object, plot, object_name)
    } else {
        plot <- align_add(object, plot, object_name)
    }
    slot(stack, "plots")[[active_plot]] <- plot
    stack
}

# Not used currently
stack_add_heatmap_element <- function(object, stack, object_name) {
    if (is.null(active_plot <- get_context(stack)) ||
        !is.ggheatmap(plot <- slot(stack, "plots")[[active_plot]])) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}}",
            i = "No active {.cls LayoutHeatmap} object",
            i = "Did you forget to add a {.fn layout_heatmap}?"
        ))
    }
    plot <- layout_heatmap_add(object, plot, object_name)
    slot(stack, "plots")[[active_plot]] <- plot
    # we should also set up the layout for the stack
    stack
}

#' @export
layout_stack_add.gg <- function(object, stack, object_name) {
    stack_add_ggelement(object, stack, object_name, "the stack layout")
}

#' @export
layout_stack_add.labels <- layout_stack_add.gg

#' @export
layout_stack_add.facetted_pos_scales <- layout_stack_add.gg

#' @export
layout_stack_add.NULL <- function(object, stack, object_name) stack

#' @export
layout_stack_add.default <- function(object, stack, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} into stack layout")
}
