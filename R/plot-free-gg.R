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
#' @return A `ggalign_free_gg` object.
#' @examples
#' ggheatmap(matrix(rnorm(56), nrow = 7)) +
#'     anno_top() +
#'     align_dendro() +
#'     ggfree(aes(wt, mpg), data = mtcars) +
#'     geom_point()
#' @export
free_gg <- function(..., data = waiver(), size = NULL, active = NULL) {
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
free_gg.default <- function(mapping = aes(), ...,
                            data = waiver(), size = NULL, active = NULL) {
    data <- fortify_data_frame(data = data, ...)
    new_free_gg(
        plot = ggplot(data = NULL, mapping = mapping),
        data = data,
        size = size,
        active = active
    )
}

#' @export
#' @rdname free_gg
free_gg.ggplot <- function(..., data = waiver(), size = NULL, active = NULL) {
    plot <- data
    # In ggplot2, `waiver()` was regard to no data
    data <- .subset2(plot, "data") %|w|% NULL
    plot["data"] <- list(waiver())
    new_free_gg(plot, data, size = size, active = active)
}

new_free_gg <- function(plot, data, size, active,
                        call = caller_call()) {
    active <- update_active(active, new_active(
        order = NA_integer_, use = TRUE, name = NA_character_
    ))
    new_free_plot(
        plot = plot, data = data,
        size = size, active = active,
        controls = new_controls(
            new_plot_data(if (is.waive(data)) waiver() else NULL)
        ),
        class = "ggalign_free_gg",
        call = call
    )
}

#' @export
plot_initialize.ggalign_free_gg <- function(object, layout, object_name) {
    input_data <- .subset2(object, "data")
    layout_data <- layout@data
    if (is.waive(input_data)) { # inherit from the layout
        data <- layout_data
    } else if (is.function(input_data)) {
        if (is.null(layout_data)) {
            cli_abort(c(
                "{.arg data} in {.var {object_name}} cannot be a function",
                i = sprintf(
                    "no data was found in %s",
                    object_name(layout)
                )
            ))
        }
        data <- input_data(layout_data)
    } else {
        data <- input_data
    }
    data <- fortify_data_frame(data)

    # convert the data into a data frame
    object$plot <- free_gg_build_plot(.subset2(object, "plot"), data)
    object
}

#' @export
print.ggalign_free_gg <- function(x, ...) {
    p <- free_gg_build_plot(.subset2(x, "plot"), .subset2(x, "data"))
    print(p)
    invisible(x)
}

free_gg_build_plot <- function(plot, data) {
    if (is.function(data)) {
        data <- waiver()
    } else if (is.null(data)) {
        # `ggplot2::fortify()` will convert `NULL` to `waiver()`
        data <- waiver()
    }
    plot$data <- data
    plot
}

# For patchwork
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.ggalign_free_gg <- function(object, plot, object_name) {
    object <- free_gg_build_plot(
        .subset2(object, "plot"),
        .subset2(object, "data")
    )
    ggplot_add(object, plot, object_name)
}
