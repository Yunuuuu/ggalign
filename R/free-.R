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
#' @return A `free_gg` object.
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
    new_free_gg(ggplot(data = NULL, mapping = mapping), data,
        size = size, active = active
    )
}

#' @export
#' @rdname free_gg
free_gg.ggplot <- function(..., data = waiver(), size = NULL, active = NULL) {
    plot <- data
    data <- .subset2(plot, "data")
    plot["data"] <- list(NULL)
    new_free_gg(plot, data, size = size, active = active)
}

new_free_gg <- function(plot, data, size, active,
                        call = caller_call()) {
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    assert_active(active, call = call)
    active <- update_active(active, new_active(
        order = NA_integer_, use = TRUE, name = NA_character_
    ))
    structure(
        list(
            plot = plot, data = data,
            size = size, active = active,
            controls = new_controls(
                new_plot_data(if (is.waive(data)) waiver() else NULL)
            )
        ),
        class = "free_gg"
    )
}

is_free <- function(x) inherits(x, "free_gg")

#' @export
print.free_gg <- function(x, ...) {
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
ggplot_add.free_gg <- function(object, plot, object_name) {
    object <- free_gg_build_plot(
        .subset2(object, "plot"),
        .subset2(object, "data")
    )
    ggplot_add(object, plot, object_name)
}

free_add <- function(object, free, object_name) UseMethod("free_add")

#' @importFrom ggplot2 ggplot_add
#' @export
free_add.default <- function(object, free, object_name) {
    free$plot <- ggplot_add(object, .subset2(free, "plot"), object_name)
    free
}

#' @export
free_add.ggalign_option <- function(object, free, object_name) {
    name <- ggalign_option_name(object)
    free$controls[name] <- list(update_option(
        object, .subset2(free$controls, name), object_name
    ))
    free
}

free_build <- function(x, controls) {
    plot <- plot_add_controls(.subset2(x, "plot"), controls)
    list(plot = plot, size = .subset2(x, "size"))
}
