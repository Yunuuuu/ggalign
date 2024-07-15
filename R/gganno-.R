#' Heatmap annotation of `gganno`
#'
#' @inheritParams ggplot2::ggplot
#' @importFrom ggplot2 aes
#' @inheritParams anno
#' @return A `gganno` object.
#' @importFrom rlang caller_call current_call
#' @export
gganno <- function(mapping = aes(), data = NULL,
                   position = NULL, size = NULL,
                   labels = NULL, labels_nudge = NULL,
                   set_context = TRUE, order = NULL, name = NULL) {
    call <- caller_call()
    if (anno_override_call(call)) {
        call <- current_call()
    }
    anno("gganno",
        data = data,
        order = order,
        size = size,
        plot = ggplot2::ggplot(mapping = mapping),
        name = name, position = position, set_context = set_context,
        labels = labels, labels_nudge = labels_nudge,
        call = call
    )
}

#' @include anno-.R
#' @keywords internal
methods::setClass("gganno", contains = "anno", list(plot = "ANY"))

#' @param ... Additional arguments passed to `gganno`.
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
