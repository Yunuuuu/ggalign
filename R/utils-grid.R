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
is_null_unit <- function(x) unitType(x) == "null"
