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
#' @inheritParams layout_heatmap
#' @return A `StackLayout` object.
#' @examples
#' ggstack(matrix(rnorm(100L), nrow = 10L)) + align_dendro()
#' @export
layout_stack <- function(data, direction = NULL, ...,
                         environment = parent.frame()) {
    if (missing(data)) {
        .layout_stack(
            data = NULL, nobs = NULL,
            direction = direction,
            environment = environment,
            call = current_call()
        )
    } else {
        UseMethod("layout_stack")
    }
}

#' @export
print.StackLayout <- function(x, ...) {
    p <- ggalign_build(x)
    if (!is.null(p)) print(p, ...)
    invisible(x)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.StackLayout <- function(x, recording = TRUE) {
    grid.draw(ggalign_build(x), recording = recording)
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
layout_stack.matrix <- function(data, ..., environment = parent.frame()) {
    .layout_stack(
        data = data, nobs = nrow(data), ...,
        environment = environment,
        call = current_call()
    )
}

#' @export
layout_stack.data.frame <- layout_stack.matrix

#' @export
layout_stack.numeric <- function(data, ..., environment = parent.frame()) {
    .layout_stack(
        data = as.matrix(data), nobs = length(data), ...,
        environment = environment,
        call = current_call()
    )
}

#' @export
layout_stack.character <- layout_stack.numeric

#' @export
layout_stack.NULL <- function(data, ..., environment = parent.frame()) {
    .layout_stack(
        data = data, nobs = NULL, ...,
        environment = environment,
        call = current_call()
    )
}

#' @importFrom grid unit
.layout_stack <- function(data, nobs, direction = NULL,
                          environment = parent.frame(),
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
        # following parameters are used by ggplot methods
        # like `ggsave` and `ggplot_build`
        theme = default_theme(),
        plot_env = environment
    )
}

#' @export
layout_stack.default <- function(data, ..., environment = parent.frame()) {
    cli::cli_abort(c(
        paste(
            "{.arg data} must be a numeric or character vector,",
            "a data frame, or a matrix."
        ),
        i = "You have provided {.obj_type_friendly {data}}"
    ))
}

#' Subset a `StackLayout` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A `StackLayout` object
#' @param name A string of slot name in `StackLayout` object.
#' @importFrom methods slot
#' @keywords internal
methods::setMethod("$", "StackLayout", function(x, name) {
    # https://github.com/tidyverse/ggplot2/issues/6002
    if (name == "theme") {
        p <- ggplot2::ggplot()
        p$theme
    } else if (name == "plot_env") {
        p <- ggplot2::ggplot()
        p$plot_env
    } else {
        cli::cli_abort(c(
            "`$` is just for internal ggplot2 methods",
            i = "try to use `@` function instead"
        ))
    }
})

#' Reports whether `x` is a `StackLayout` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggstack(ggstack(1:10))
#' @export
is.ggstack <- function(x) methods::is(x, "StackLayout")
