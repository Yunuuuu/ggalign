#' Create a ggplot inset
#'
#' @inheritParams ggwrap
#' @return A `patch_inset` object, which can be added in ggplot.
#' @inherit patch seealso
#' @examples
#' library(grid)
#' p1 <- ggplot(mtcars) +
#'     geom_point(aes(mpg, disp))
#' p2 <- ggplot(mtcars) +
#'     geom_boxplot(aes(gear, disp, group = gear))
#' p1 + inset(p2, vp = viewport(0.6, 0.6,
#'     just = c(0, 0), width = 0.4, height = 0.4
#' ))
#' @export
inset <- function(
    plot,
    ...,
    align = "panel",
    on_top = TRUE,
    clip = TRUE,
    vp = NULL) {
    make_inset(
        plot = plot,
        ...,
        align = align,
        on_top = on_top,
        clip = clip,
        vp = vp
    )
}

#' @importFrom grid editGrob
#' @importFrom rlang arg_match0
make_inset <- function(
    plot,
    ...,
    align,
    on_top,
    clip,
    vp,
    call = caller_call()) {
    assert_bool(on_top, call = call)
    align <- arg_match0(align, c("panel", "plot", "full"), error_call = call)
    assert_bool(clip, call = call)
    assert_s3_class(vp, "viewport", allow_null = TRUE, call = call)
    if (!is.grob(grob <- patch(x = plot, ...))) {
        cli_abort("{.fn patch} must return a {.cls grob}", call = call)
    }
    if (!is.null(vp)) grob <- editGrob(grob, vp = vp)
    structure(
        list(
            grob = grob,
            align = align,
            clip = if (clip) "on" else "off",
            on_top = on_top
        ),
        class = "patch_inset"
    )
}

#' @importFrom grid grid.draw
#' @export
grid.draw.patch_inset <- function(x, recording = TRUE) {
    grid.draw(.subset2(x, "grob"))
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.patch_inset <- function(object, plot, object_name, ...) {
    make_wrap(plot, object)
}
