####################################################################
# add annotation into annotation list
#' Put plots horizontally or vertically
#'
#' `ggstack` is an alias of `layout_stack`.
#'
#' @param data A numeric or character vector, a data frame, or a matrix.
#' @param direction A string of `"horizontal"` or `"vertical"`, indicates the
#' direction of the stack layout.
#' @param rel_sizes A numeric or [unit][grid::unit] object of length `3`
#' indicates the relative widths (`direction = "horizontal"`) / heights
#' (`direction = "vertical"`).
#' @inheritParams layout_heatmap
#' @return A `LayoutStack` object.
#' @export
layout_stack <- function(data, direction = NULL,
                         rel_sizes = NULL, guides = NULL,
                         align_axis_title = NULL,
                         plot_data = waiver()) {
    UseMethod("layout_stack")
}

#' @export
print.LayoutStack <- function(x, ...) {
    p <- build_patchwork(x)
    print(p, ...)
}

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.LayoutStack <- function(x, ...) {
    print(x, ...)
}

#' @importFrom ggplot2 ggplot_build
#' @export
ggplot_build.LayoutStack <- function(plot) {
    plot <- build_patchwork(plot)
    ggplot_build(plot)
}

# Used to place multiple objects in one axis
# usually the heatmap annotations
#' @keywords internal
#' @importFrom grid unit
methods::setClass(
    "LayoutStack",
    contains = "Layout",
    list(
        data = "ANY",
        plots = "list",
        params = "list",
        direction = "character",
        panels = "ANY",
        index = "ANY",
        size = "ANY" # used by `layout_heatmap` only
    ),
    prototype = list(
        panels = NULL, index = NULL,
        size = unit(1, "null")
    )
)

#' @export
#' @rdname layout_stack
ggstack <- layout_stack

#' @export
layout_stack.matrix <- function(data, direction = NULL,
                                rel_sizes = NULL, guides = NULL,
                                align_axis_title = NULL,
                                plot_data = waiver()) {
    direction <- match.arg(direction, c("horizontal", "vertical"))
    if (is.null(rel_sizes)) {
        rel_sizes <- rep_len(1L, 3L)
    } else if (length(rel_sizes) != 3L ||
        (!is.unit(rel_sizes) && !is.numeric(rel_sizes))) {
        cli::cli_abort(paste(
            "{.arg rel_sizes} must be",
            "a numeric or unit object of length 3"
        ))
    }
    plot_data <- allow_lambda(plot_data)
    if (!is.waive(plot_data) &&
        !is.null(plot_data) &&
        !is.function(plot_data)) {
        cli::cli_abort("{.arg plot_data} must be a function")
    }

    methods::new("LayoutStack",
        data = data, direction = direction,
        params = list(
            rel_sizes = rel_sizes, guides = guides,
            align_axis_title = align_axis_title
        ),
        plot_data = plot_data
    )
}

#' @export
layout_stack.data.frame <- layout_stack.matrix

#' @export
layout_stack.numeric <- function(data, direction = NULL,
                                 rel_sizes = NULL, guides = NULL,
                                 align_axis_title = NULL,
                                 plot_data = waiver()) {
    ans <- matrix(data, ncol = 1L)
    colnames(ans) <- "V1"
    if (rlang::is_named(data)) rownames(ans) <- names(data)
    layout_stack(
        data = ans, direction = direction,
        rel_sizes = rel_sizes, guides = guides,
        align_axis_title = align_axis_title,
        plot_data = plot_data
    )
}

#' @export
layout_stack.character <- layout_stack.numeric

#' @export
layout_stack.NULL <- function(data, direction = NULL,
                              rel_sizes = NULL, guides = NULL,
                              align_axis_title = NULL,
                              plot_data = waiver()) {
    cli::cli_abort("{.arg data} must be a matrix-like object instead of `NULL`")
}

#' Subset a `LayoutStack` object
#'
#' Used by [ggplot_build][ggplot2::ggplot_build] and [ggsave][ggplot2::ggsave]
#'
#' @param x A `LayoutStack` object
#' @param name A string of slot name in `LayoutStack` object.
#' @importFrom methods slot
#' @keywords internal
methods::setMethod("$", "LayoutStack", function(x, name) {
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
            i = "try to use `@` method instead"
        ))
    }
})

#' Reports whether `x` is a `LayoutStack` object
#'
#' @param x An object to test
#' @return A boolean value
#' @examples
#' is.ggstack(ggstack(1:10))
#' @export
is.ggstack <- function(x) methods::is(x, "LayoutStack")
