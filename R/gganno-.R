#' Heatmap Annotation
#'
#' @param position Possible values are `"top"`, `"left"`, `"bottom"`, and
#' `"right"`. If `NULL`, the active context of the ggheatmap will be used.
#' @inheritParams ggplot2::ggplot
#' @importFrom ggplot2 aes
#' @importFrom grid is.unit unit
#' @inheritParams new_anno
#' @export
gganno <- function(mapping = aes(), data = NULL,
                   position = NULL, size = NULL,
                   active = TRUE, name = NULL, order = NULL) {
    new_anno("gganno",
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
plot.gganno <- function(x, ...) {
    cli::cli_abort("You cannot plot {.cls gganno} object")
}

#' @include anno-.R
#' @keywords internal
methods::setClass(
    "gganno",
    contains = "anno",
    list(plot = "ANY")
)

#' @export
methods::setMethod("show", "gganno", function(object) {
    print("A gganno object")
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
