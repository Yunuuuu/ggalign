# Use S4 to override the double dispatch problem of ggplot2
# And it's easy to convert a S4 Class to a S7 Class
methods::setClass(
    "ggalign_plot",
    list(
        plot = "ANY", # To avoid modify in place, we put plot in a slot
        active = "ANY",
        size = "ANY",
        schemes = "ANY",
        align = "ANY" # `AlignProto` object
    )
)

#' Show `ggalign_plot` information
#' @param object A `ggalign_plot` object.
#' @return The input invisiblely.
#' @keywords internal
methods::setMethod("show", "ggalign_plot", function(object) {
    print(object)
})

#' @export
print.ggalign_plot <- function(x, ...) {
    oo <- summary(x)
    cli::cli_inform(c(
        sprintf("%s object:", object_name(x)),
        " " = sprintf(
            "  {.field plot}: %s",
            if (oo[1L]) "yes" else "no"
        ),
        " " = sprintf(
            "  {.field reorder}: %s",
            if (oo[2L]) "yes" else "no"
        ),
        " " = sprintf(
            "  {.field split}: %s",
            if (oo[3L]) "yes" else "no"
        )
    ))
    invisible(x)
}

#' Summary the action of `ggalign_plot`
#'
#' @param object A `ggalign_plot` object
#' @return A logical vector of length 3, indicating:
#' - Whether the object adds a plot.
#' - Whether the object reorders the observations.
#' - Whether the object splits the observations into groups.
#' @export
#' @keywords internal
summary.ggalign_plot <- function(object, ...) {
    c(!is.null(object@plot), summary(object@align))
}

#' @importFrom methods new
new_ggalign_plot <- function(align = NULL, ...,
                             plot = NULL, active = NULL, size = NULL,
                             schemes = NULL, class = "ggalign_plot",
                             call = caller_call()) {
    assert_active(active, allow_null = FALSE, call = call)
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    new(
        class,
        # `call`: used to provide error message
        align = ggproto(NULL, align %||% AlignProto, ..., call = call),
        schemes = schemes %||% default_schemes(),
        plot = plot, active = active, size = size
    )
}

#' @export
plot.ggalign_plot <- function(x, ...) {
    cli_abort(sprintf("Cannot plot %s object directly", object_name(x)))
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.ggalign_plot <- plot.ggalign_plot

#' Add custom objects to ggalign plot
#' @keywords internal
methods::setMethod("+", c("ggalign_plot", "ANY"), function(e1, e2) {
    if (missing(e2)) {
        cli_abort(c(
            "Cannot use {.code {.Generic}} with a single argument.",
            "i" = "Did you accidentally put {.code {.Generic}} on a new line?"
        ))
    }

    if (is.null(e2)) return(e1) # styler: off

    # Get the name of what was passed in as e2, and pass along so that it
    # can be displayed in error messages
    e2name <- deparse(substitute(e2))
    switch(.Generic, # nolint
        `+` = plot_add(e1, e2, e2name),
        stop_incompatible_op(.Generic, e1, e2)
    )
})

#' @importFrom methods is
is_ggalign_plot <- function(x) is(x, "ggalign_plot")

#' Summary the action of `AlignProto`
#'
#' @param object A `AlignProto` object
#' @return A logical vector of length 2, indicating:
#' - Whether the object reorders the observations.
#' - Whether the object splits the observations into groups.
#' @export
#' @keywords internal
summary.AlignProto <- function(object, ...) c(FALSE, FALSE)

#' @importFrom ggplot2 ggproto
AlignProto <- ggproto("AlignProto",
    locked = TRUE,
    # A boolean value indicates whether this plot is free
    # if `FALSE`, we'll check whether the layout can add this object
    free_align = FALSE,
    lock = function(self) {
        assign("locked", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("locked", value = FALSE, envir = self)
    },

    ############################################################
    # when added to the `Layout` object, will call following methods
    layout = function(self, layout_data, layout_coords, layout_name) {
        layout_coords
    },
    setup_plot = function(self, plot) plot,
    finish_layout = function(self, layout) layout,

    ##############################################################
    build_plot = function(self, plot, coords, extra_coords, previous_coords) {
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot_add_schemes(plot, schemes) + theme_recycle()
    }
)

#' @importFrom rlang inject
align_inject <- function(method, params) {
    inject(method(
        !!!params[intersect(align_method_params(method), names(params))]
    ))
}

ggproto_formals <- function(x) formals(environment(x)$f)

align_method_params <- function(f, remove = character()) {
    vec_set_difference(names(ggproto_formals(f)), c("self", remove))
}

# Used to lock the `AlignProto` object
#' @export
`$<-.AlignProto` <- function(x, name, value) {
    if (x$locked) {
        cli_abort(c(
            sprintf("Cannot modify %s", object_name(x)),
            i = sprintf("%s is locked", object_name(x))
        ), call = x$call)
    }
    NextMethod()
    x
}

###########################################################
#' @export
stack_layout_add.ggalign_plot <- function(object, stack, object_name) {
    align <- object@align
    if (is_cross(align) && !is_cross_layout(stack)) {
        # we prevent from adding `cross_*` to normal `stack_layout()`
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s",
                object_name(stack)
            ),
            i = "Did you want to use {.fn cross_align} instead?"
        ))
    }
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        if (is.null(old_coords <- stack@layout) && !align$free_align) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(stack)
                ),
                i = sprintf("%s cannot align observations", object_name(stack))
            ))
        } else {
            # unlock the object
            align$unlock()

            # initialize the necessary parameters for `AlignProto` object
            align$direction <- stack@direction
            align$position <- .subset2(stack@heatmap, "position")
            align$object_name <- object_name

            # this step, the object will act with the stack layout
            # group rows into panel or reorder rows
            new_coords <- align$layout(
                layout_data = stack@data, # must be a matrix
                layout_coords = old_coords,
                layout_name = object_name(stack)
            )

            # initialize the plot object
            object@plot <- align$setup_plot(object@plot)

            # finally, we let the object do some changes in the layout
            stack <- align$finish_layout(stack)

            # we lock the Align object to prevent user from modifying this
            # object in `$build` method, we shouldn't do any calculations in
            # `$build` method
            align$lock()
            stack <- stack_add_plot(stack, object, object@active, object_name)
        }
    } else { # should be a QuadLayout object
        plot <- quad_layout_add(object, plot, object_name)
        stack@plot_list[[active_index]] <- plot
        new_coords <- slot(plot, stack@direction)
    }
    stack <- update_layout_coords(
        stack,
        coords = new_coords,
        object_name = object_name
    )
    stack
}

#' @importFrom methods slot slot<-
#' @export
quad_layout_add.ggalign_plot <- function(object, quad, object_name) {
    if (is.null(position <- quad@active)) {
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "no active annotation stack",
            i = "try to activate an annotation stack with {.fn anno_*}"
        ))
    }
    if (is.null(stack <- slot(quad, position))) {
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = "the {.field {position}} annotation stack is not initialized",
            i = "Try to use {.code quad_anno(initialize = TRUE)} or you can add a {.code stack_layout()} manually"
        ))
    }

    # check if we can align in this direction
    direction <- to_direction(position)
    align <- object@align
    if (!align$free_align && is.null(slot(quad, direction))) {
        cli_abort(c(
            sprintf(
                "Cannot add {.var {object_name}} to %s", object_name(quad)
            ),
            i = sprintf(
                "%s cannot align observations in {.field {direction}} direction", object_name(quad)
            )
        ))
    }

    # add annotation -----------------------------
    stack <- stack_layout_add(object, stack, object_name)
    slot(quad, position) <- stack

    new_coords <- stack@layout
    # if there are cross points in bottom or right annotation, the index
    # should be the first index in the `index_list`
    if (any(position == c("bottom", "right")) &&
        is_cross_layout(stack) &&
        !is_empty(stack@cross_points)) {
        new_coords["index"] <- list(.subset2(stack@index_list, 1L))
    }
    quad <- update_layout_coords(
        quad,
        direction = direction,
        coords = new_coords,
        object_name = object_name
    )
    quad
}
