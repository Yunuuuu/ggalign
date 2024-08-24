wrap <- function(plot, align = NULL, clip = TRUE) UseMethod("wrap")

#' @export
wrap.grob <- function(plot, align = NULL, clip = TRUE) {
    assert_bool(clip)
    align <- match.arg(align, c("panel", "plot", "full"))
    clip <- if (clip) "on" else "off"
    attr(plot, "align") <- align
    attr(plot, "clip") <- clip
    add_class(plot, "wrapped_plot")
}

#' @export
wrap.ggplot <- wrap.grob

#' @export
wrap.wrapped_plot <- function(plot, align = NULL, clip = TRUE) {
    align <- match.arg(align, c("panel", "plot", "full"))
    attr(plot, "align") <- align
    plot
}

#' @export
wrap.formula <- function(x, align = NULL, clip = TRUE) {
    rlang::check_installed("gridGraphics", "to add base plots to patchworks")
    gp <- graphics::par(no.readonly = TRUE)
    force(x)
    plot_call <- function() {
        old_gp <- graphics::par(no.readonly = TRUE)
        graphics::par(gp)
        on.exit(try(graphics::par(old_gp)))
        suppressMessages(eval(x[[2]], attr(x, ".Environment")))
        invisible(NULL)
    }
    wrap.grob(gridGraphics::echoGrob(plot_call,
        name = "base_plot",
        device = offscreen_dev()
    ), align, clip)
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
patch_gtable.wrapped_plot <- function(patch) {
    gt <- NextMethod()
    attr(gt, "align") <- attr(patch, "align")
    attr(gt, "clip") <- attr(patch, "clip")
    add_class(gt, "gtable_wrapped")
}

# wrapped patch has been used by patchwork
#' @importFrom gtable gtable_add_grob
#' @export
patch_align.gtable_wrapped <- function(gt, guides) {
    ans <- make_patch()
    align <- attr(gt, "align")
    clip <- attr(gt, "clip")
    ans <- switch(align,
        full = gtable_add_grob(ans,
            list(gt), 1L, 1L, nrow(ans), ncol(ans),
            clip = clip, name = "wrap_full"
        ),
        plot = gtable_add_grob(ans,
            list(gt), PLOT_TOP, PLOT_LEFT, PLOT_BOTTOM, PLOT_RIGHT,
            clip = clip, name = "wrap_plot"
        ),
        panel = gtable_add_grob(ans,
            list(gt), PANEL_ROW, PANEL_COL,
            clip = clip, name = "wrap_panel"
        )
    )
    add_class(ans, "align_wrapped", "alignpatch")
}

#########################################
# `patch` from `patchwork`: patchwork::wrap_elements
#' @export
patch_gtable.wrapped_patch <- function(patch) {
    add_class(patch, "gtable_wrapped_patch")
}

#' @export
patch_align.gtable_wrapped_patch <- function(gt, guides) {
    class(gt) <- setdiff(class(gt), "gtable_wrapped_patch")
    guides <- if (length(guides)) "collect" else "keep"
    add_class(
        patchwork::patchGrob(gt, guides = guides),
        "align_wrapped_patch", "alignpatch"
    )
}
