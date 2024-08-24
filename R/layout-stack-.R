####################################################################
# add annotation into annotation list
#' Put plots horizontally or vertically
#'
#' `ggstack` is an alias of `layout_stack`.
#'
#' @param data A numeric or character vector, a data frame, or a matrix.
#' @param direction A string of `"horizontal"` or `"vertical"`, indicates the
#' direction of the stack layout.
#' @param sizes A numeric or [unit][grid::unit] object of length `3` indicates
#' the relative widths (`direction = "horizontal"`) / heights (`direction =
#' "vertical"`).
#' @inheritParams layout_heatmap
#' @return A `StackLayout` object.
#' @export
layout_stack <- function(data = NULL, direction = NULL,
                         sizes = NULL, guides = waiver(),
                         free_labs = waiver(),
                         plot_data = waiver()) {
    UseMethod("layout_stack")
}

#' @export
print.StackLayout <- function(x, ...) {
    p <- build_alignpatches(x)
    if (!is.null(p)) print(p, ...)
    invisible(x)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.StackLayout <- function(x, ...) {
    print(x, ...)
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
    .layout_stack(data = data, nobs = nrow(data), ..., call = current_call())
}

#' @export
layout_stack.data.frame <- layout_stack.matrix

#' @export
layout_stack.numeric <- function(data, ...) {
    .layout_stack(
        data = as.matrix(data),
        nobs = length(data), ...,
        call = current_call()
    )
}

#' @export
layout_stack.character <- layout_stack.numeric

#' @export
layout_stack.NULL <- function(data = NULL, ...) {
    .layout_stack(data = data, nobs = NULL, ..., call = current_call())
}

#' @importFrom grid unit
.layout_stack <- function(data, nobs, direction = NULL,
                          sizes = NULL, guides = waiver(),
                          free_labs = waiver(),
                          plot_data = waiver(),
                          call = caller_call()) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    if (is.null(sizes)) {
        sizes <- unit(rep_len(1L, 3L), "null")
    } else {
        sizes <- check_stack_sizes(sizes, call = call)
    }
    plot_data <- check_plot_data(plot_data, call = call)
    methods::new("StackLayout",
        data = data, direction = direction,
        params = list(
            sizes = set_size(sizes),
            guides = guides, plot_data = plot_data,
            free_labs = free_labs
        ),
        nobs = nobs
    )
}

#' @export
layout_stack.default <- function(data, direction = NULL,
                                 sizes = NULL, guides = waiver(),
                                 free_labs = waiver(),
                                 plot_data = waiver()) {
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
