#' Create a ggplot inset
#'
#' @param plot Any graphic that can be converted into a [`grob`][grid::grob]
#' using [`patch()`].
#' @param ... Additional arguments passed to the [`patch()`] method.
#' @param align A string specifying the area to place the plot: `"full"` for the
#' full area, `"plot"` for the full plotting area (including the axis label), or
#' `"panel"` for only the actual area where data is drawn.
#' @param clip A single boolean value indicating whether the grob should be
#' clipped if they expand outside their designated area.
#' @param on_top A single boolean value indicates whether the graphic plot
#' should be put frontmost. Note: the graphic plot will always put above the
#' background.
#' @param vp A [`viewport`][grid::viewport] object, you can use this to define
#' the plot area.
#' @return An `inset` object, which can be added to ggplot.
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
#' @importFrom rlang arg_match0
#' @importFrom grid is.grob
#' @importFrom S7 prop
#' @export
inset <- S7::new_class(
    "inset",
    properties = list(
        grob = S7::new_S3_class("grob"),
        vp = S7::new_union(NULL, S7::new_S3_class("viewport")),
        align = S7::new_property(
            S7::class_character,
            setter = function(self, value) {
                value <- arg_match0(
                    value, c("panel", "plot", "full"),
                    arg_nm = "@align"
                )
                prop(self, "align", check = FALSE) <- value
                self
            }
        ),
        clip = S7::new_property(
            S7::class_character,
            setter = function(self, value) {
                assert_bool(value, arg = "@clip")
                prop(self, "clip", check = FALSE) <- if (value) "on" else "off"
                self
            }
        ),
        on_top = S7::new_property(
            S7::class_logical,
            setter = function(self, value) {
                assert_bool(value, arg = "@on_top")
                prop(self, "on_top", check = FALSE) <- value
                self
            }
        )
    ),
    constructor = function(plot, ..., align = "panel", on_top = TRUE,
                           clip = TRUE, vp = NULL) {
        if (!is.grob(grob <- patch(x = plot, ...))) {
            cli_abort("{.fn patch} must return a {.cls grob} for {.obj_type_friendly {plot}}")
        }
        new_object(
            S7_object(),
            grob = grob,
            vp = vp,
            align = align,
            clip = clip,
            on_top = on_top
        )
    }
)

#' @importFrom grid grid.draw
local(S7::method(grid.draw, inset) <- function(x, recording = TRUE) {
    grid.draw(prop(x, "grob"))
})

#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(inset, ggplot2::class_ggplot)) <-
    function(object, plot, objectname, ...) {
        make_wrap(plot, object)
    }
