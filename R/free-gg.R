#' Add ggplot to layout
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `free_gg()` function allows you to incorporate a ggplot object into your
#' layout. Unlike `align_gg()`, which aligns every axis value precisely,
#' `free_gg()` focuses on layout integration without enforcing strict axis
#' alignment. `ggfree()` is an alias for `free_gg`.
#'
#' @param ... Additional arguments passed to [`fortify_data_frame()`].
#' @param data A dataset used to initialize a [`ggplot`][ggplot2::ggplot]
#' object. By default, it will inherit from the parent layout if applicable.
#' Alternatively, a pre-defined [`ggplot`][ggplot2::ggplot] object can be
#' provided directly.
#' @inheritParams align
#' @examples
#' ggheatmap(matrix(rnorm(56), nrow = 7)) +
#'     anno_top() +
#'     align_dendro() +
#'     ggfree(mtcars, aes(wt, mpg)) +
#'     geom_point()
#' @export
free_gg <- function(data = waiver(), ..., size = NULL, active = NULL) {
    rlang::check_dots_used()
    UseMethod("free_gg", data)
}

#' @usage NULL
#' @export
#' @rdname free_gg
ggfree <- free_gg

#' @inheritParams ggplot2::ggplot
#' @importFrom ggplot2 ggplot
#' @export
#' @rdname free_gg
free_gg.default <- function(data = waiver(), mapping = aes(), ...,
                            size = NULL, active = NULL) {
    data <- fortify_data_frame(data = data, ...)
    new_free_gg(
        plot = ggplot(data = NULL, mapping = mapping),
        data = data,
        size = size,
        active = active
    )
}

#' @export
free_gg.uneval <- function(data = waiver(), ...) {
    cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
    ))
}

#' @export
free_gg.ggplot <- function(data = waiver(), ..., size = NULL, active = NULL) {
    plot <- data
    # In ggplot2, `waiver()` was regard to no data
    data <- .subset2(plot, "data") %|w|% NULL
    plot["data"] <- list(waiver())
    new_free_gg(plot, data, size = size, active = active)
}

new_free_gg <- function(plot, data, size, active,
                        call = caller_call()) {
    assert_active(active, allow_null = TRUE, call = call)
    active <- update_active(active, new_active(use = TRUE))
    free(
        FreeGg,
        # new field for FreeGg
        input_data = data,
        # slots for the plot
        plot = plot,
        size = size,
        active = active,
        schemes = default_schemes(data),
        call = call
    )
}

#' @importFrom ggplot2 ggproto
FreeGg <- ggproto("FreeGg", Free,
    layout = function(self, layout_data, layout_coords) {
        if (is.waive(input_data <- self$input_data)) { # inherit from the layout
            data <- layout_data
        } else if (is.function(input_data)) {
            if (is.null(layout_data)) {
                object_name <- .subset2(self, "object_name")
                cli_abort(c(
                    "{.arg data} in {.var {object_name}} cannot be a function",
                    i = sprintf("no data was found in %s", object_name(layout))
                ))
            }
            data <- input_data(layout_data)
        } else {
            data <- input_data
        }
        self$data <- ggalign_attr_restore(fortify_data_frame(data), layout_data)
        layout_coords
    },
    build_plot = function(self, plot, coords, extra_coords, previous_coords) {
        if (is.function(data <- self$data)) {
            data <- waiver()
        } else if (is.null(data)) {
            # `ggplot2::fortify()` will convert `NULL` to `waiver()`
            data <- waiver()
        }
        plot$data <- data
        plot
    }
)

#' @export
stack_layout_add.ggplot <- function(object, stack, object_name) {
    stack_layout_add(free_gg(data = object), stack, object_name)
}
