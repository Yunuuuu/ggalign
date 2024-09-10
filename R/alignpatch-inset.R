#' Create a ggplot inset
#' @inheritParams wrap
#' @return A `patch_inset` object, which can be added in ggplot.
#' @inherit patch seealso
#' @export
inset <- function(plot, ..., align = "panel", on_top = TRUE,
                  clip = TRUE, vp = NULL) {
    grob <- make_inset(
        plot = plot, ..., align = align, on_top = on_top,
        clip = clip, vp = vp
    )
    add_class(grob, "patch_inset")
}

#' @importFrom rlang arg_match0
make_inset <- function(plot, ..., align, on_top, clip, vp,
                       call = caller_call()) {
    assert_bool(on_top, call = call)
    align <- arg_match0(align, c("panel", "plot", "full"), error_call = call)
    assert_bool(clip, call = call)
    assert_s3_class(vp, "viewport", null_ok = TRUE, call = call)
    if (!is.grob(grob <- patch(x = plot, ...))) {
        cli::cli_abort("{.fn patch} must return a {.cls grob}", call = call)
    }
    attr(grob, "align") <- align
    attr(grob, "clip") <- if (clip) "on" else "off"
    attr(grob, "vp") <- vp
    attr(grob, "on_top") <- on_top
    grob
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.patch_inset <- function(object, plot, object_name) {
    make_wrap(plot, object)
}
