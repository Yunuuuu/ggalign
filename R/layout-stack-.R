####################################################################
# add annotation into annotation list
#' Stack layout
#'
#' `ggstack` is an alias of `layout_stack`.
#'
#' @inheritParams layout_heatmap
#' @param direction A string of `"horizontal"` or `"vertical"`, indicates the
#' direction of the stack layout.
#' @param labels,labels_nudge Default `labels`/`labels_nudge` for axis
#' parallelly with the layout.
#' @param rel_sizes A numeric or [unit][grid::unit] object of length `3`
#' indicates the relative widths (`direction = "vertical"`) / heights
#' (`direction = "vertical"`).
#' @return A `LayoutStack` object.
#' @export
layout_stack <- function(data, direction = NULL,
                         labels = waiver(), labels_nudge = waiver(),
                         rel_sizes = NULL, guides = "collect") {
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
                                labels = waiver(), labels_nudge = waiver(),
                                rel_sizes = NULL, guides = "collect") {
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
    methods::new("LayoutStack",
        data = data, direction = direction,
        params = list(
            labels = labels, labels_nudge = labels_nudge,
            rel_sizes = rel_sizes, guides = guides
        )
    )
}

#' @export
layout_stack.data.frame <- layout_stack.matrix

#' @export
layout_stack.numeric <- function(data, direction = NULL,
                                 labels = waiver(), labels_nudge = waiver(),
                                 rel_sizes = NULL, guides = "collect") {
    ans <- matrix(data, ncol = 1L)
    colnames(ans) <- "V1"
    if (rlang::is_named(data)) rownames(ans) <- names(data)
    layout_stack(
        data = ans, direction = direction,
        labels = labels, labels_nudge = labels_nudge,
        rel_sizes = rel_sizes, guides = guides
    )
}

#' @export
layout_stack.character <- layout_stack.numeric

#' @export
layout_stack.NULL <- function(data, direction = NULL,
                              labels = waiver(), labels_nudge = waiver(),
                              rel_sizes = NULL, guides = "collect") {
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
