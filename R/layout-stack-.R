####################################################################
# add annotation into annotation list
#' Put plots horizontally or vertically
#'
#' `ggstack` is an alias of `layout_stack`.
#'
#' @param data A numeric or character vector, a data frame, or a matrix.
#' @param direction A string of `"horizontal"` or `"vertical"`, indicates the
#' direction of the stack layout.
#' @param ... Not used currently.
#' @return A `StackLayout` object.
#' @examples
#' ggstack(matrix(rnorm(100L), nrow = 10L)) + align_dendro()
#' @export
layout_stack <- function(data, direction = NULL, ...) {
    if (missing(data)) {
        .layout_stack(
            data = NULL, nobs = NULL,
            direction = direction,
            call = current_call()
        )
    } else {
        UseMethod("layout_stack")
    }
}

#' @export
print.StackLayout <- function(x, ...) {
    if (!is.null(p <- alignpatch(x))) print(p, ...)
    invisible(x)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.StackLayout <- function(x, recording = TRUE) {
    grid.draw(alignpatch(x), recording = recording)
}

# Used to place multiple objects in one axis
# usually the heatmap annotations
#' @keywords internal
#' @importFrom grid unit
methods::setClass(
    "StackLayout",
    contains = "Layout",
    list(
        data = "ANY",
        plots = "list",
        params = "list",
        direction = "character",
        panel = "ANY",
        index = "ANY",
        nobs = "ANY"
    ),
    prototype = list(panel = NULL, index = NULL, nobs = NULL)
)

#' @export
#' @rdname layout_stack
ggstack <- layout_stack

#' @export
layout_stack.matrix <- function(data, ...) {
    .layout_stack(
        data = data, nobs = nrow(data), ...,
        call = current_call()
    )
}

#' @export
layout_stack.data.frame <- layout_stack.matrix

#' @export
layout_stack.numeric <- function(data, ...) {
    .layout_stack(
        data = as.matrix(data), nobs = length(data), ...,
        call = current_call()
    )
}

#' @export
layout_stack.character <- layout_stack.numeric

#' @export
layout_stack.NULL <- function(data, ...) {
    .layout_stack(data = data, nobs = NULL, ..., call = current_call())
}

#' @importFrom grid unit
.layout_stack <- function(data, nobs, direction = NULL,
                          call = caller_call()) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    methods::new("StackLayout",
        data = data, direction = direction,
        params = list(
            sizes = unit(rep_len(NA, 3L), "null"),
            guides = waiver(), plot_data = waiver(),
            free_labs = waiver(), free_spaces = waiver()
        ),
        nobs = nobs,
        # following parameters are used by ggsave
        theme = default_theme()
    )
}

#' @export
layout_stack.default <- function(data, ...) {
    cli::cli_abort(c(
        paste(
            "{.arg data} must be a numeric or character vector,",
            "a data frame, or a matrix."
        ),
        i = "You have provided {.obj_type_friendly {data}}"
    ))
}

#' Reports whether `x` is a `StackLayout` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggstack(ggstack(1:10))
#' @export
is.ggstack <- function(x) methods::is(x, "StackLayout")
