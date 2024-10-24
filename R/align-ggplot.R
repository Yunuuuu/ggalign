#' Add ggplot to layout
#'
#' @param plot A [`ggplot`][ggplot2::ggplot] object, you can use [ggwrap()] to
#' convert any graphic into ggplot.
#' @inheritParams align
#' @inheritParams rlang::args_dots_empty
#' @return A `AlignGgplot` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     gplot(ggplot(mtcars, aes(wt, mpg))) +
#'     geom_point()
#' @export
align_ggplot <- function(plot, size = NULL, action = NULL,
                         set_context = TRUE, order = NULL, name = NULL,
                         ...) {
    UseMethod("align_ggplot")
}

#' @rdname align_ggplot
#' @export
gplot <- align_ggplot

#' @inheritParams rlang::args_dots_empty
#' @export
align_ggplot.ggplot <- function(plot,
                                size = NULL, action = NULL,
                                set_context = TRUE, order = NULL, name = NULL,
                                ...) {
    rlang::check_dots_empty()
    align(AlignGgplot,
        params = list(plot = plot),
        size = size, data = NULL, action = action %||% waiver(),
        set_context = set_context, order = order, name = name,
        limits = FALSE, facet = FALSE
    )
}

#' @export
align_ggplot.default <- function(plot,
                                 size = NULL, action = NULL,
                                 set_context = TRUE, order = NULL, name = NULL,
                                 ...) {
    cli::cli_abort(c(
        "{.arg plot} must be a {.cls ggplot}",
        i = "try to use {.fn ggwrap} to convert it into a {.cls ggplot}"
    ))
}

#' @importFrom ggplot2 ggproto
AlignGgplot <- ggproto("AlignGgplot", Align,
    ggplot = function(self, plot) plot,
    draw = function(self, panel, index, extra_panel, extra_index) {
        .subset2(self, "plot")
    }
)
