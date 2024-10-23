#' Wrap Arbitrary Graphics to layout
#'
#' @param plot A [ggplot][ggplot2::ggplot] object or any graphic that can be
#' converted into a [ggplot][ggplot2::ggplot] using [wrap()].
#' @param ... Additional arguments passed on to [wrap()], only used if `plot` is
#' not a [ggplot][ggplot2::ggplot] object.
#' @inheritParams align
#' @return A `AlignWrap` object.
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     ggwrap(ggplot(mtcars, aes(wt, mpg))) +
#'     geom_point()
#' @export
align_wrap <- function(plot, ...,
                       size = NULL, action = NULL,
                       set_context = TRUE, order = NULL, name = NULL) {
    UseMethod("align_wrap")
}

#' @rdname align_wrap
#' @export
ggwrap <- align_wrap

#' @inheritParams rlang::args_dots_empty
#' @export
align_wrap.ggplot <- function(plot, ...,
                              size = NULL, action = NULL,
                              set_context = TRUE, order = NULL, name = NULL) {
    rlang::check_dots_empty()
    align(AlignWrap,
        params = list(plot = plot),
        size = size, data = NULL, action = action %||% waiver(),
        set_context = set_context, order = order, name = name,
        limits = FALSE, facet = FALSE
    )
}

#' @export
align_wrap.default <- function(plot, ...,
                               size = NULL, action = NULL,
                               set_context = TRUE, order = NULL, name = NULL) {
    plot <- wrap(plot = plot, ...)
    align(AlignWrap,
        params = list(plot = plot),
        size = size, data = NULL, action = action %||% waiver(),
        set_context = set_context, order = order, name = name,
        limits = FALSE, facet = FALSE
    )
}

#' @importFrom ggplot2 ggproto
AlignWrap <- ggproto("AlignWrap", Align,
    ggplot = function(self, plot) plot,
    draw = function(self, panel, index, extra_panel, extra_index) {
        .subset2(self, "plot")
    }
)
