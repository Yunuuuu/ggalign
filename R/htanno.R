#' Determine the context of subsequent manipulations
#' 
#' @param position Possible values are "heatmap", "top", "left", "bottom", and
#'   "right". 
#' @param what A string of the name the annotation.
htanno <- function(position = NULL, what = NULL) {
    position <- match.arg(position, GGHEAT_ELEMENTS)
    new_htanno(position, what)
}

is_htanno <- function(x) inherits(x, "htanno")

new_htanno <- function(position, what) {
    structure(
        list(position = position, what = what),
        class = c("htanno", "ggheat")
    )
}
