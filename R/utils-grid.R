#' @importFrom grid gpar
#' @export
grid::gpar

#' @importFrom grid unit
#' @export
grid::unit

#' @importFrom grid unitType absolute.size
is_absolute_unit <- function(x) {
    unitType(absolute.size(x)) != "null"
}

#' @importFrom grid unitType
is_null_unit <- function(x) identical(unitType(x), "null")

get_width <- function(x) UseMethod("get_width")
get_height <- function(x) UseMethod("get_height")

#' @importFrom grid grobWidth
#' @export
get_width.default <- function(x) grobWidth(x)

#' @importFrom grid grobHeight
#' @export
get_height.default <- function(x) grobHeight(x)

#' @importFrom gtable gtable_width
get_width.gtable <- function(x) gtable_width(x)

#' @importFrom gtable gtable_height
#' @export
get_height.gtable <- function(x) gtable_height(x)
