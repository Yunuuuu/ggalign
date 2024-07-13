#' Heatmap Annotation
#'
#' @param position Possible values are `"top"`, `"left"`, `"bottom"`, and
#' `"right"`.
#' @inheritParams ggplot2::ggplot
#' @importFrom ggplot2 aes
#' @importFrom grid is.unit unit
#' @export
gganno <- function(mapping = aes(), data = NULL,
                   position = NULL, size = unit(10, "mm"),
                   active = TRUE, name = NULL, order = NULL) {
    new_gganno("ggannotation",
        data = data,
        order = order,
        size = size,
        plot = ggplot2::ggplot(mapping = mapping),
        # following attributes were used by `ggheatmap_add`
        name = name, position = position, active = active
    )
}

#' @export
#' @keywords internal
plot.ggannotation <- function(x, ...) {
    cli::cli_abort("You cannot plot {.cls ggannotation} object")
}

#' @include anno-.R
#' @keywords internal
methods::setClass(
    "ggannotation",
    contains = "gganno",
    list(plot = "ANY")
)

#' @export
methods::setMethod("show", "ggannotation", function(object) {
    print("A ggannotation object")
    invisible(object)
})

#' @export
#' @rdname gganno
gganno_top <- function(...) gganno(position = "top", ...)

#' @export
#' @rdname gganno
gganno_bottom <- function(...) gganno(position = "bottom", ...)

#' @export
#' @rdname gganno
gganno_left <- function(...) gganno(position = "left", ...)

#' @export
#' @rdname gganno
gganno_right <- function(...) gganno(position = "right", ...)
