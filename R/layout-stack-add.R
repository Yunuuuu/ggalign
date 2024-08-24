#' @keywords internal
layout_stack_add <- function(object, stack, object_name) {
    UseMethod("layout_stack_add")
}

# `Align` can be added for both heatmap and stack layout
#' @importFrom methods slot slot<-
#' @export
layout_stack_add.Align <- function(object, stack, object_name) {
    stack_add_heatmap_element(
        object, stack, object_name, FALSE,
        stack_add_align
    )
}

#' @importFrom methods slot
#' @export
layout_stack_add.stack_active <- function(object, stack, object_name) {
    stack <- set_context(stack, object)
    if (!is.null(sizes <- attr(object, "sizes"))) {
        slot(stack, "params")$sizes <- sizes
    }
    if (!identical(guides <- attr(object, "guides"), NA)) {
        slot(stack, "params")$guides <- guides
    }
    if (!is.null(align_axis_title <- attr(object, "align_axis_title"))) {
        slot(stack, "params")$align_axis_title <- align_axis_title
    }
    if (!identical(plot_data <- attr(object, "plot_data"), NA)) {
        slot(stack, "params")$plot_data <- plot_data
    }
    stack
}

#' @importFrom methods slot
#' @export
layout_stack_add.heatmap_active <- function(object, stack, object_name) {
    stack_add_heatmap_element(object, stack, object_name, TRUE)
}

#' @importFrom methods slot
#' @export
layout_stack_add.HeatmapLayout <- function(object, stack, object_name) {
    direction <- slot(stack, "direction")
    axis <- to_coord_axis(direction)

    # setup heatmap data ----------------------------------
    heatmap_data <- slot(object, "data")
    if (is.null(heatmap_data) || is.function(heatmap_data)) {
        if (is.null(stack_data <- slot(stack, "data"))) {
            cli::cli_abort(c(
                paste(
                    "You must provide {.arg data} argument in",
                    style_code(object_name)
                ),
                i = "No data was found in the stack layout"
            ))
        }
        # always convert into a matrix
        stack_data <- as.matrix(stack_data)
        data <- switch_direction(direction, stack_data, t(stack_data))
        if (is.function(heatmap_data)) {
            data <- heatmap_data(data)
            call <- current_call()
            data <- tryCatch(
                as.matrix(data),
                error = function(cnd) {
                    cli::cli_abort(paste(
                        "{.arg data} in {.code {object_name}} must return",
                        "a matrix-like object"
                    ), call = call)
                }
            )
        }
        slot(object, "data") <- data
        # we should sync the `nobs` of the vertical axis
        if (is_horizontal(direction)) {
            object <- set_nobs(object, "x", ncol(data))
        } else {
            object <- set_nobs(object, "y", nrow(data))
        }
    }

    # check the observations is compatible ----------------
    stack_nobs <- get_nobs(stack)
    heatmap_nobs <- get_nobs(object, axis)
    if (is.null(heatmap_nobs)) {
        nobs <- stack_nobs
    } else if (is.null(stack_nobs)) {
        nobs <- heatmap_nobs
    } else if (!identical(heatmap_nobs, stack_nobs)) {
        cli::cli_abort(sprintf(
            "%s (%d) is not compatible with the %s stack layout (%d)",
            style_code(object_name), heatmap_nobs,
            style_field(direction), stack_nobs
        ))
    } else {
        nobs <- stack_nobs
    }

    # check panel and index ------------------------------
    heatmap_panel <- get_panel(object, axis)
    stack_panel <- get_panel(stack)
    if (is.null(heatmap_panel)) {
        panel <- stack_panel
    } else if (is.null(stack_panel)) {
        panel <- heatmap_panel
    } else if (!identical(heatmap_panel, stack_panel)) {
        cli::cli_abort(paste(
            "{.code {object_name}} disrupt the previously",
            "established layout panel of the stack"
        ))
    } else {
        panel <- stack_panel
    }

    heatmap_index <- get_index(object, axis)
    stack_index <- get_index(stack)
    index <- heatmap_index %||% stack_index
    if (!is.null(panel) && !is.null(index)) {
        index <- reorder_index(panel, index)
    }

    # we always prevent from reordering the layout twice.
    if (!is.null(stack_index)) {
        if (!all(stack_index == index)) {
            cli::cli_abort(sprintf(
                "{.code {object_name}} disrupt the previously %s %s-axis",
                "established layout order of the stack",
                axis
            ))
        }
    }
    # let the stack determine how to collect the guides
    slot(object, "params")$guides <- slot(object, "params")$guides %|w|% 
        .subset2(slot(stack, "params"), "guides")

    # set up context index ------------------------------
    plots <- slot(stack, "plots")
    if (slot(object, "set_context")) {
        active_index <- length(plots) + 1L
    } else {
        active_index <- get_context(stack)
    }

    # check heatmap name is unique ----------------------
    if (!is.na(name <- slot(object, "name"))) {
        if (any(names(plots) == name)) {
            cli::cli_warn("{object_name}: {name} plot is already present")
        }
        plots[[name]] <- object
    } else {
        plots <- c(plots, list(object))
    }

    # add heatmap ---------------------------------------
    slot(stack, "plots") <- plots
    slot(stack, "active") <- active_index

    # set the layout ------------------------------------
    stack <- set_panel(stack, panel)
    stack <- set_index(stack, index)
    stack <- set_nobs(stack, nobs)
    stack
}

###################################################################
#' @export
layout_stack_add.gg <- function(object, stack, object_name) {
    stack_add_ggelement(object, stack, object_name, "the stack layout")
}

#' @export
layout_stack_add.ggplot <- function(object, stack, object_name) {
    cli::cli_abort(c(
        "Cannot add {.code {object_name}} into the stack layout",
        i = "try to use {.fn ggalign} to initialize a {.cls ggplot} object"
    ))
}

#' @export
layout_stack_add.labels <- layout_stack_add.gg

#' @export
layout_stack_add.facetted_pos_scales <- layout_stack_add.gg

#' @export
layout_stack_add.NULL <- function(object, stack, object_name) stack

#' @export
layout_stack_add.default <- function(object, stack, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} into the stack layout")
}

#################################################################
stack_add_align <- function(object, stack, object_name) {
    plots <- slot(stack, "plots")

    # set up context index ------------------------------
    if (.subset2(object, "set_context")) {
        active_index <- length(plots) + 1L
    } else {
        active_index <- get_context(stack)
    }

    # check annotation name is unique --------------------
    if (!is.null(name <- .subset2(object, "name"))) {
        if (any(names(plots) == name)) {
            cli::cli_warn("{object_name}: {name} plot is already present")
        }
        plots[[name]] <- object
    } else {
        plots <- c(plots, list(object))
    }

    # make layout ----------------------------------------
    # this step the object will act with the stack layout
    # group rows into panel or reorder rows
    layout <- initialize_align(
        object,
        slot(stack, "direction"),
        layout_data = slot(stack, "data"),
        layout_panel = get_panel(stack),
        layout_index = get_index(stack),
        nobs = get_nobs(stack),
        object_name = object_name
    )

    # add annotation -------------------------------------
    slot(stack, "plots") <- plots
    slot(stack, "active") <- active_index

    # set the layout -------------------------------------
    stack <- set_panel(stack, .subset2(layout, 1L))
    stack <- set_index(stack, .subset2(layout, 2L))
    stack <- set_nobs(stack, .subset2(layout, 3L))
    stack
}

stack_add_ggelement <- function(object, stack, object_name, layout_name) {
    if (is.null(active_index <- get_context(stack))) {
        cli::cli_abort(c(
            sprintf("Cannot add {.code {object_name}} into %s", layout_name),
            i = "No active {.cls ggplot} object",
            i = paste(
                "Did you forget to initialize a {.cls ggplot} object",
                "with {.fn ggalign}?"
            )
        ))
    }
    plot <- slot(stack, "plots")[[active_index]]
    if (is.ggheatmap(plot)) {
        plot <- layout_heatmap_add(object, plot, object_name)
    } else {
        plot <- align_add(object, plot, object_name)
    }
    slot(stack, "plots")[[active_index]] <- plot
    stack
}

#' @param force A boolean value if this element can only apply for ggheatmap?
#' @noRd
stack_add_heatmap_element <- function(object, stack, object_name, force,
                                      stack_add_fun) {
    if (!is.null(active_index <- get_context(stack)) &&
        is.ggheatmap(plot <- .subset2(slot(stack, "plots"), active_index))) {
        plot <- layout_heatmap_add(object, plot, object_name)
        slot(stack, "plots")[[active_index]] <- plot
        axis <- to_coord_axis(slot(stack, "direction"))
        stack <- set_panel(stack, get_panel(plot, axis))
        stack <- set_index(stack, get_index(plot, axis))
        stack <- set_nobs(stack, get_nobs(plot, axis))
    } else if (force) {
        cli::cli_abort(c(
            "Cannot add {.code {object_name}}",
            i = "No active {.cls HeatmapLayout} object",
            i = "Did you forget to add a {.fn ggheatmap}?"
        ))
    } else {
        stack <- stack_add_fun(object, stack, object_name)
    }
    stack
}
