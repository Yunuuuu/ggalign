#' @importFrom grid gpar
#' @export
grid::gpar

#' @importFrom grid unit
#' @export
grid::unit

#' @importFrom grid is.unit
set_size <- function(x) {
    if (is.null(x)) {
        x <- unit(1, "null")
    } else if (!is.unit(x)) {
        x <- unit(x, "null")
    }
    x
}
