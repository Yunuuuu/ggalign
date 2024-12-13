# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
cross <- function(cross, ..., call = caller_call()) {
    new_align_plot(
        align = ggproto(NULL, cross),
        ...,
        class = "ggalign_cross",
        call = call
    )
}

#' @include plot-align-.R
methods::setClass("ggalign_cross", contains = "ggalign_align_plot")

#' @importFrom methods is
is_cross_plot <- function(x) is(x, "ggalign_cross")
