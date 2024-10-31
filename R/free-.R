#' Add ggplot to layout
#'
#' `ggfree` is an alias for `free_gg`.
#'
#' @param x Default dataset to initialize the [`ggplot`][ggplot2::ggplot].
#' Alternatively, you can provide a [`ggplot`][ggplot2::ggplot] object directly;
#' you can use [`ggwrap()`] to convert any graphic into a ggplot.
#' @param ... Additional arguments passed to [`ggplot()`][ggplot2::ggplot].
#' @param size Plot size, specified as a [`unit`][grid::unit] object.
#' @param action A [`plot_action()`] object used for the plot. By default,
#' it inherits from the parent layout.
#' @inheritParams quad_free
#' @return A `free_gg` object.
#' @examples
#' ggheatmap(matrix(rnorm(56), nrow = 7)) +
#'     anno_top() +
#'     ggfree(mtcars, aes(wt, mpg)) +
#'     geom_point()
#' @export
free_gg <- function(x, ..., size = NULL, action = NULL, context = NULL) {
    rlang::check_dots_used()
    UseMethod("free_gg")
}

#' @usage NULL
#' @export
#' @rdname free_gg
ggfree <- free_gg

#' @importFrom ggplot2 ggplot
#' @export
free_gg.default <- function(x, ..., size = NULL,
                            action = NULL, context = NULL) {
    data <- fortify_data_frame(x)
    new_free_gg(ggplot(data = NULL, ...), data,
        size = size, action = action, context = context
    )
}

#' @export
free_gg.ggplot <- function(x, ..., size = NULL,
                           action = NULL, context = NULL) {
    data <- .subset2(x, "data")
    x["data"] <- list(NULL)
    new_free_gg(x, data, size = size, action = action, context = context)
}

new_free_gg <- function(plot, data, size, action, context,
                        call = caller_call()) {
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    action <- check_action(action, NULL, call = call)
    assert_s3_class(context, "plot_context", null_ok = TRUE, call = call)
    context <- update_context(context, new_context(
        order = NA_integer_, active = TRUE, name = NA_character_
    ))
    structure(
        list(
            plot = plot, data = data,
            size = size, action = action, context = context
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
free_add.plot_action <- function(object, free, object_name) {
    free$action <- update_action(free$action, object)
    free
}

free_build <- function(x, action, direction) {
    plot <- plot_add_action(.subset2(x, "plot"), action, call = quote(ggfree()))
    list(plot = plot, size = .subset2(x, "size"))
}
