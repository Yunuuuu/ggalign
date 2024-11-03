#' Add ggplot to layout
#'
#' `ggfree` is an alias for `free_gg`.
#'
#' @param x A dataset used to initialize a [`ggplot`][ggplot2::ggplot] object.
#' Alternatively, a pre-defined [`ggplot`][ggplot2::ggplot] object can be
#' provided directly. By default, it will inherit from the parent layout if
#' applicable.
#' @param ... Additional arguments passed to [`ggplot()`][ggplot2::ggplot].
#' @inheritParams align
#' @return A `free_gg` object.
#' @examples
#' ggheatmap(matrix(rnorm(56), nrow = 7)) +
#'     anno_top() +
#'     ggfree(mtcars, aes(wt, mpg)) +
#'     geom_point()
#' @export
free_gg <- function(x = waiver(), ..., size = NULL, active = NULL) {
    rlang::check_dots_used()
    UseMethod("free_gg")
}

#' @usage NULL
#' @export
#' @rdname free_gg
ggfree <- free_gg

#' @importFrom ggplot2 ggplot
#' @export
free_gg.default <- function(x = waiver(), ..., size = NULL, active = NULL) {
    data <- fortify_data_frame(x)
    new_free_gg(ggplot(data = NULL, ...), data,
        size = size, active = active
    )
}

#' @export
free_gg.ggplot <- function(x = waiver(), ..., size = NULL, active = NULL) {
    data <- .subset2(x, "data")
    x["data"] <- list(NULL)
    new_free_gg(x, data, size = size, active = active)
}

new_free_gg <- function(plot, data, size, active,
                        call = caller_call()) {
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    assert_active(active)
    active <- update_active(active, new_active(
        order = NA_integer_, use = TRUE, name = NA_character_
    ))
    structure(
        list(
            plot = plot, data = data,
            size = size, active = active,
            controls = new_controls()
        ),
        class = "free_gg"
    )
}

is_free <- function(x) inherits(x, "free_gg")

#' @export
print.free_gg <- function(x, ...) {
    print(.subset2(x, "plot"))
    invisible(x)
}

# For patchwork
#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.free_gg <- function(object, plot, object_name) {
    object$plot <- ggplot_add(.subset2(object, "plot"), plot, object_name)
    object
}

free_add <- function(object, free, object_name) UseMethod("free_add")

#' @importFrom ggplot2 ggplot_add
#' @export
free_add.default <- function(object, free, object_name) {
    free$plot <- ggplot_add(object, .subset2(free, "plot"), object_name)
    free
}

#' @export
free_add.ggalign_controls <- function(object, free, object_name) {
    name <- attr(object, "name")
    free$controls[name] <- list(update_option(
        object, .subset2(free$controls, name), object_name
    ))
    free
}

free_build <- function(x, controls, direction) {
    plot <- plot_add_controls(.subset2(x, "plot"), controls)
    list(plot = plot, size = .subset2(x, "size"))
}
