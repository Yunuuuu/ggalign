# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
new_align_plot <- function(..., no_axes = NULL, align = AlignProto,
                           class = NULL, call = caller_call()) {
    assert_bool(no_axes, allow_null = TRUE, call = call)
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)
    # used to provide error message
    align$call <- call
    new_ggalign_plot(
        align = align,
        no_axes = no_axes,
        ...,
        class = class %||% "ggalign_align_plot",
        call = call
    )
}

#' @include plot-.R
methods::setClass(
    "ggalign_align_plot",
    contains = "ggalign_plot",
    list(align = "ANY", no_axes = "ANY")
)

#' Summary the action of `ggalign_align_plot`
#' @param object A `ggalign_align_plot` object
#' @return A logical vector of length 3, indicating:
#' - Whether the object adds a plot.
#' - Whether the object reorders the observations.
#' - Whether the object splits the observations into groups.
#' @export
#' @keywords internal
summary.ggalign_align_plot <- function(object, ...) {
    c(!is.null(object@plot), summary(object@align))
}

#' @export
summary.AlignProto <- function(object, ...) c(FALSE, FALSE)

#' @importFrom ggplot2 ggproto
AlignProto <- ggproto("AlignProto",
    lock = function(self) {
        assign("isLock", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("isLock", value = FALSE, envir = self)
    },

    ############################################################
    # when added to the `Layout` object, will call `$align` method
    # must have fixed parameters
    layout = function(self, direction, position, object_name,
                      layout_data, layout_coords, layout_name) {
        cli_abort(sprintf(
            "%s, has not implemented a {.fn align} method",
            "{.fn {snake_class(self)}}"
        ))
    },
    setup_plot = function(self, plot, direction, position, object_name,
                          layout_data, layout_coords, layout_name) {
        cli_abort(sprintf(
            "%s, has not implemented a {.fn setup_plot} method",
            "{.fn {snake_class(self)}}"
        ))
    },
    finish_layout = function(self, layout, direction, position, object_name,
                             layout_data, layout_coords, layout_name) {
        layout
    },

    ##############################################################
    build = function(plot, schemes, direction, position,
                     coords, extra_coords, previous_coords) {
        plot
    },
    add_schemes = function(plot, schemes) plot_add_schemes(plot, schemes),
    finish_plot = function(plot) plot
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

# Used to lock the `Align` object
#' @export
`$<-.AlignProto` <- function(x, name, value) {
    if (isTRUE(.subset2(x, "isLock"))) {
        cli_abort(
            "{.fn {snake_class(x)}} is locked",
            call = .subset2(x, "call")
        )
    }
    assign(x = name, value = value, envir = x)
    invisible(x)
}

#' @importFrom rlang inject
#' @export
plot_build.ggalign_align_plot <- function(plot, ..., direction, schemes) {
    # let `Align` to determine how to build the plot
    align <- plot@align # `AlignProto` object

    # we lock the Align object to prevent user from modifying this object
    # in `$build` method, we shouldn't do any calculations in `$build` method
    align$lock()
    on.exit(align$unlock())

    # schemes
    # coords
    # extra_coords
    # previous_coords
    # direction
    # position
    ans <- align$build(
        plot = plot@plot,
        ...,
        direction = direction
    )

    # remove axis titles, text, ticks used for alignment
    if (isTRUE(plot@no_axes)) {
        schemes$scheme_theme <- .subset2(schemes, "scheme_theme") +
            theme_no_axes(switch_direction(direction, "y", "x"))
    }
    align$finish_plot(align$add_schemes(ans, schemes))
}

#' @export
stack_layout_add.ggalign_align_plot <- function(object, stack, object_name) {
    if (is.null(active_index <- stack@active) ||
        is_ggalign_plot(plot <- .subset2(stack@plot_list, active_index))) {
        if (is.null(old_coords <- stack@layout)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {object_name}} to %s",
                    object_name(stack)
                ),
                i = sprintf("%s cannot align observations", object_name(stack))
            ))
        } else {
            align <- object@align
            params <- list(
                direction = stack@direction,
                position = .subset2(stack@heatmap, "position"),
                object_name = object_name,
                layout_data = stack@data, # must be a matrix
                layout_coords = old_coords,
                layout_name = object_name(stack)
            )
            # this step, the object will act with the stack layout
            # group rows into panel or reorder rows
            new_coords <- inject(align$layout(!!!params))

            # initialize the plot object
            if (!is.null(object@plot)) {
                object@plot <- inject(align$setup_plot(
                    !!!c(list(plot = object@plot), params)
                ))
            }

            # finally, we let the object do some changes in the layout
            stack <- inject(align$finish_layout(!!!c(list(stack), params)))

            stack <- stack_add_plot(stack, object, object@active, object_name)
        }
    } else { # should be a QuadLayout object
        plot <- quad_layout_add(object, plot, object_name)
        stack@plot_list[[active_index]] <- plot
        new_coords <- slot(plot, stack@direction)
    }
    update_layout_coords(
        stack,
        coords = new_coords,
        object_name = object_name
    )
}

#' @importFrom methods slot slot<-
#' @export
quad_layout_add.ggalign_align_plot <- function(object, quad, object_name) {
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
    direction <- to_direction(position)

    # check if we can align in this direction
    if (is.null(slot(quad, direction))) {
        cli_abort(c(
            "Cannot add {.var {object_name}} to {.fn {quad@name}}",
            i = paste(
                "{.fn {quad@name}} cannot align observations",
                "in {.field {direction}} direction"
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
    update_layout_coords(
        quad,
        direction = direction,
        coords = new_coords,
        object_name = object_name
    )
}
