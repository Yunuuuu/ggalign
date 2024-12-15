# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
free <- function(free, ..., call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_ggalign_plot(align = free, ..., call = call)
}

is_free_plot <- function(x) is_ggalign_plot(x) && is_free(x@align)

is_free <- function(x) inherits(x, "FALSE")

#' @export
summary.Free <- function(object, ...) c(FALSE, FALSE)

#' @importFrom ggplot2 ggproto ggplot
#' @include plot-.R
Free <- ggproto("Free", AlignProto, free_align = TRUE)
