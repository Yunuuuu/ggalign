# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
new_align_plot <- function(..., no_axes = NULL, align_class = AlignProto,
                           class = character(),
                           call = caller_call()) {
    assert_bool(no_axes, allow_null = TRUE, call = call)
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)
    # used to provide error message
    align_class$call <- call
    new_ggalign_plot(
        workflow = align_class, no_axes = no_axes,
        ...,
        class = c(class, "ggalign_align_plot"),
        call = call
    )
}

#' @importFrom ggplot2 ggproto
AlignProto <- ggproto("AlignProto",
    lock = function(self) {
        assign("isLock", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("isLock", value = FALSE, envir = self)
    },
    params = list(),
    # All parameters in `$initialize()` method can be used in
    # `$ggplot()`, and `$finish()` methods
    initialize = function(self, direction, position, object_name,
                          layout_data, layout_coords, layout_name) {
        cli_abort(sprintf(
            "%s, has not implemented a {.fn initialize} method",
            "{.fn {snake_class(self)}}"
        ))
    },
    ggplot = function(self) {
        cli_abort(sprintf(
            "%s, has not implemented a {.fn ggplot} method",
            "{.fn {snake_class(self)}}"
        ))
    },
    finish = function(layout) layout,
    build = function(plot, composer, controls, coords, extra_coords,
                     direction, position) {
        plot
    }
)

# Used to lock the `Align` object
#' @export
`$<-.AlignProto` <- function(x, name, value) {
    if (isTRUE(.subset2(x, "isLock"))) {
        cli_abort("{.fn {snake_class(x)}} is locked",
            call = .subset2(x, "call")
        )
    }
    assign(x = name, value = value, envir = x)
    invisible(x)
}

#' @importFrom rlang inject
#' @export
plot_build.ggalign_align_plot <- function(plot, ..., direction, controls) {
    # let `Align` to determine how to build the plot
    workflow <- .subset2(plot, "workflow") # `Align` object

    # we lock the Align object to prevent user from modifying this object
    # in `$build` method, we shouldn't do any calculations in `$build` method
    workflow$lock()
    on.exit(workflow$unlock())
    dots <- list(..., direction = direction, controls = controls)
    ans <- inject(workflow$build(plot = .subset2(plot, "plot"), !!!dots[
        vec_set_intersect(
            names(dots),
            align_method_params(workflow$build, character())
        )
    ]))

    # remove axis titles, text, ticks used for alignment
    if (isTRUE(.subset2(plot, "no_axes"))) {
        controls$plot_theme <- .subset2(controls, "plot_theme") +
            theme_no_axes(switch_direction(direction, "y", "x"))
    }
    plot_add_controls(ans, controls)
}

ggproto_formals <- function(x) formals(environment(x)$f)

align_method_params <- function(f, remove = c("panel", "index")) {
    vec_set_difference(names(ggproto_formals(f)), c("self", remove))
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
            workflow <- .subset2(object, "workflow")
            dots <- list(
                direction = stack@direction,
                position = .subset2(stack@heatmap, "position"),
                object_name = object_name,
                layout_data = stack@data, # must be a matrix
                layout_coords = old_coords,
                layout_name = object_name(stack)
            )
            # this step the object will act with the stack layout
            # group rows into panel or reorder rows
            new_coords <- inject(workflow$initialize(!!!dots[
                vec_set_intersect(
                    names(dots),
                    align_method_params(workflow$initialize, character())
                )
            ]))

            # initialize the plot object
            object$plot <- inject(workflow$ggplot(
                !!!workflow$params[
                    vec_set_intersect(
                        names(workflow$params),
                        align_method_params(workflow$ggplot, character())
                    )
                ],
                !!!dots[
                    vec_set_intersect(
                        names(dots),
                        align_method_params(workflow$ggplot, character())
                    )
                ]
            ))
            # finally, we let the object do some changes in the layout
            stack <- inject(workflow$finish(stack, !!!dots[
                vec_set_intersect(
                    names(dots),
                    align_method_params(workflow$finish, character())
                )
            ]))
            stack <- stack_add_plot(
                stack, object,
                .subset2(object, "active"),
                object_name
            )
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
    # skip the updating of layout coords if there are cross points in
    # bottom or right annotation
    if (any(position == c("top", "left")) ||
        !is_cross_layout(stack) ||
        is_empty(stack@cross_points)) {
        opposite <- opposite_pos(position)
        if (!is.null(opposite_stack <- slot(quad, opposite))) {
            slot(quad, opposite) <- update_layout_coords(
                opposite_stack,
                coords = stack@layout,
                object_name = object_name
            )
        }
        slot(quad, direction) <- stack@layout
    }
    quad
}
