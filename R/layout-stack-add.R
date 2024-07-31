#' Add components to `LayoutStack`
#'
#' @param e1 A [LayoutStack][layout_stack] object.
#' @param e2 An object to be added to the plot, including
#' [gg][ggplot2::+.gg] elements, [gganno][gganno] object, or [htanno]
#' object.
#' @return A modified `LayoutStack` object.
#' @examples
#' layout_stack(matrix(rnorm(81), nrow = 9)) +
#'     gganno() +
#'     geom_point(aes(y = value))
#' @name stack-add
#' @aliases +.stack
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
    stack <- set_panels(stack, .subset2(layout, 1L))
    stack <- set_index(stack, .subset2(layout, 2L))

    # add annotation -------------------------------------
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
    slot(stack, "plots") <- c(plots, new)
    slot(stack, "active") <- active_index
    stack
}

layout_stack_add.gg <- function(object, stack, object_name) {
    layout_stack_add_gg(object, stack, object_name, "the stack layout")
}

layout_stack_add_gg <- function(object, stack, object_name, name) {
    if (is.null(active_plot <- get_context(stack))) {
        cli::cli_abort(c(
            sprintf("Cannot add {.code {object_name}} to %s", name),
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

#' @export
layout_stack_add.labels <- layout_stack_add.gg

#' @export
layout_stack_add.facetted_pos_scales <- layout_stack_add.gg

#' @export
layout_stack_add.default <- function(object, stack, object_name) {
    cli::cli_abort("Cannot add {.code {object_name}} into stack layout")
}
