#' Used by both gganno and htanno ------------------------------
new_gganno <- function(Class, ..., data = NULL,
                       position = NULL, size = unit(10, "mm"),
                       order = NULL) {
    data <- allow_lambda(data)
    if (!is.null(position)) position <- match.arg(position, GGHEAT_ELEMENTS)
    if (is.na(size) || is.null(size)) size <- unit(1, "null")
    if (!is.unit(size)) size <- unit(size, "null")
    order <- order %||% NA_integer_
    methods::new(Class,
        data = data,
        order = order,
        size = size,
        position = position,
        ...
    )
}

#' @include ggheat.R
#' @keywords internal
methods::setClass(
    "gganno",
    contains = "ggheat",
    list(
        data = "ANY",
        order = "integer",
        size = "ANY",
        name = "ANY",
        position = "character",
        active = "logical"
    )
)

#' @importFrom grid is.unit
#' @importFrom rlang is_string is_bool
methods::setValidity("gganno", function(object) {
    active <- slot(object, "active")
    if (length(active) != 1L && length(active) != 2L) {
        cli::cli_abort("@active must be of length 1 or 2")
    }
    name <- slot(object, "name")
    if (!is.null(name) && (!is_string(name) || name == "")) {
        cli::cli_abort("@name must be a single non-empty string")
    }
    position <- slot(object, "position")
    if (!is.null(position) &&
        (!is_string(position) || !any(position == GGHEAT_ELEMENTS))) {
        cli::cli_abort(sprintf(
            "@position must be a string of %s",
            oxford_comma(GGHEAT_ELEMENTS, final = "or")
        ))
    }
    size <- slot(object, "size")
    if (!is_scalar(size)) {
        cli::cli_abort("{@size} must be a single {.cls unit} object.")
    } else if (!is.unit(size)) {
        cli::cli_abort("{@size} must be a single {.cls unit} object.")
    }
    TRUE
})
