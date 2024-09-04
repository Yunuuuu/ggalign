#' Wrap Arbitrary Graphics for Alignment
#'
#' The `alignwrap()` function allows non-ggplot2 elements to be converted into a
#' compliant representation for use with [plot_grid()]. This is useful for
#' adding any graphics that can be converted into a [grob][grid::grob] with the
#' [wrap()] method.
#'
#' @param plot Any graphic that can be converted into a [grob][grid::grob] using
#' [wrap()].
#' @param ... Additional arguments passed to the [wrap()] method.
#' @param align A string specifying the area to place the plot: `"full"` for the
#' full area, `"plot"` for the full plotting area (including the axis label), or
#' `"panel"` for only the actual area where data is drawn.
#' @param clip A single boolean value indicating whether the grob should be
#' clipped if they expand outside their designated area.
#' @return A `wrapped_plot` object that can be directly placed into
#' [plot_grid()].
#' @importFrom ggplot2 ggplot
#' @importFrom grid is.grob
#' @export
alignwrap <- function(plot, ..., align = NULL, clip = TRUE) {
    align <- match.arg(align, c("panel", "plot", "full"))
    assert_bool(clip)
    if (!is.grob(grob <- wrap(x = plot, ...))) {
        cli::cli_abort("{.fn wrap} must return a {.cls grob}")
    }
    clip <- if (clip) "on" else "off"
    attr(grob, "align") <- align
    attr(grob, "clip") <- clip
    patch <- ggplot()
    patch$wrapped_grob <- grob
    add_class(patch, "wrapped_plot")
}

#' Convert Object into a Grob for Wrapping
#'
#' The `wrap()` function is used by [alignwrap()] to convert objects into a
#' [grob][grid::grob], enabling their alignment within [plot_grid()].
#'
#' @param x An object to be converted into a [grob][grid::grob].
#' @return A [grob][grid::grob] object.
#' @export
#' @keywords internal
wrap <- function(x, ...) UseMethod("wrap")

#' @export
wrap.grob <- function(x, ...) x

#' @export
wrap.formula <- function(x, ...) {
    rlang::check_installed("gridGraphics", "to add base plots")
    gp <- graphics::par(no.readonly = TRUE)
    plot_call <- function() {
        old_gp <- graphics::par(no.readonly = TRUE)
        graphics::par(gp)
        on.exit(try(graphics::par(old_gp)))
        suppressMessages(eval(x[[2]], attr(x, ".Environment")))
        invisible(NULL)
    }
    gridGraphics::echoGrob(plot_call,
        name = "base_plot",
        device = offscreen_dev()
    )
}

offscreen_dev <- function() {
    if (requireNamespace("ragg", quietly = TRUE)) {
        function(width, height) {
            ragg::agg_capture(width = width, height = height, units = "in")
            grDevices::dev.control("enable")
        }
    } else {
        function(width, height) {
            grDevices::pdf(NULL, width = width, height = height)
            grDevices::dev.control("enable")
        }
    }
}

# For wrapped plot -------------------
#' @export
alignpatch.grob <- function(x) alignwrap(x)

#' @export
alignpatch.formula <- function(x) alignwrap(x)

#' @export
alignpatch.wrapped_plot <- function(x) x

#' @importFrom gtable gtable_add_grob
#' @export
#' @rdname alignpatch
#' @order 5
patch_gtable.wrapped_plot <- function(patch, guides) {
    ans <- NextMethod() # will call ggplot2 method
    grob <- .subset2(patch, "wrapped_grob")
    align <- attr(grob, "align")
    clip <- attr(grob, "clip")
    ans <- switch(align,
        full = gtable_add_grob(ans,
            list(grob), 1L, 1L, nrow(ans), ncol(ans),
            clip = clip, name = "wrap_full"
        ),
        plot = gtable_add_grob(ans,
            list(grob), PLOT_TOP, PLOT_LEFT, PLOT_BOTTOM, PLOT_RIGHT,
            clip = clip, name = "alignwrap"
        ),
        panel = gtable_add_grob(ans,
            list(grob), PANEL_ROW, PANEL_COL,
            clip = clip, name = "wrap_panel"
        )
    )
    add_class(ans, "gtable_wrapped_plot")
}
