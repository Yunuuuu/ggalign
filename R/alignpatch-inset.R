#' Create a ggplot inset
#'
#' @param plot Any graphic that can be converted into a [`grob`][grid::grob]
#' using [`as_grob()`].
#' @param ... Additional arguments passed to the [`as_grob()`] method.
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
#' @return An `inset` object that can be added to any object implementing the
#' [`patch()`] method.
#' @inherit as_grob seealso
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
            validator = function(value) {
                if (length(value) != 1L || is.na(value) ||
                    !any(value == c("panel", "plot", "full"))) {
                    return(sprintf(
                        "can only be a string containing the %s characters",
                        oxford_and(c("panel", "plot", "full"))
                    ))
                }
            }
        ),
        clip = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
            }
        ),
        on_top = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be a single boolean value")
                }
                if (is.na(value)) {
                    return("cannot be missing (`NA`)")
                }
            }
        )
    ),
    constructor = function(plot, ..., align = "panel", on_top = TRUE,
                           clip = TRUE, vp = NULL) {
        if (!is.grob(grob <- as_grob(x = plot, ...))) {
            cli_abort("{.fn as_grob} must return a {.cls grob} for {.obj_type_friendly {plot}}")
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

add_inset <- function(plot, inset) {
    attr(plot, "ggalign_insets") <- c(
        attr(plot, "ggalign_insets", exact = TRUE),
        list(inset)
    )
    if (!inherits(plot, "ggalign_inset")) {
        plot <- add_class(plot, "ggalign_inset")
    }
    plot
}

#' @importFrom grid grid.draw
local(S7::method(grid.draw, inset) <- function(x, recording = TRUE) {
    grid.draw(prop(x, "grob"))
})

#' @export
print.ggalign_inset <- print.patch_ggplot

#' @importFrom grid grid.draw
#' @exportS3Method
grid.draw.ggalign_inset <- grid.draw.patch_ggplot

#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(inset, ggplot2::class_ggplot)) <-
    function(object, plot, objectname, ...) add_inset(plot, object)

#' @importFrom ggplot2 update_ggplot
S7::method(update_ggplot, list(inset, alignpatches)) <-
    function(object, plot, objectname, ...) add_inset(plot, object)

#################################################
#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid editGrob
#' @importFrom gtable is.gtable gtable_add_grob
#' @export
patch.ggalign_inset <- function(x) {
    insets <- attr(x, "ggalign_insets", exact = TRUE)
    attr(x, "ggalign_insets") <- NULL
    Parent <- NextMethod()
    if (is.null(insets)) return(Parent) # styler: off
    ggproto(
        "PatchWrapped",
        Parent,
        gtable = function(self, theme = NULL, guides = NULL, tagger = NULL) {
            gt <- ggproto_parent(Parent, self)$gtable(theme, guides, tagger)
            # Note: When the gtable represents a facetted plot, the number
            # of rows/columns (heights or widths) will exceed
            #   TABLE_ROWS/COLS.
            if (is.gtable(gt) && nrow(gt) >= TABLE_ROWS &&
                ncol(gt) >= TABLE_COLS) {
                z <- .subset2(.subset2(gt, "layout"), "z")
                top_z <- max(z) + 1L
                bottom_z <- min(z) - 1L
                for (i in seq_along(insets)) {
                    inset <- .subset2(insets, i)
                    align <- prop(inset, "align")
                    clip <- if (prop(inset, "clip")) "on" else "off"
                    grob <- prop(inset, "grob")
                    if (!is.null(vp <- prop(inset, "vp"))) {
                        grob <- editGrob(grob, vp = vp)
                    }
                    if (prop(inset, "on_top")) {
                        inset_z <- top_z
                    } else {
                        inset_z <- bottom_z
                    }

                    # add the grob to the gtable
                    if (align == "full") {
                        gt <- gtable_add_grob(gt,
                            list(grob), 1L, 1L, nrow(gt), ncol(gt),
                            clip = clip,
                            name = sprintf("inset-full-%d", i),
                            z = inset_z
                        )
                    } else {
                        panel_loc <- find_panel(gt)
                        gt <- switch(align,
                            plot = gtable_add_grob(gt,
                                list(grob),
                                .subset2(panel_loc, "t") - 3L,
                                .subset2(panel_loc, "l") - 3L,
                                .subset2(panel_loc, "b") + 3L,
                                .subset2(panel_loc, "r") + 3L,
                                clip = clip,
                                name = sprintf("inset-plot-%d", i),
                                z = inset_z
                            ),
                            panel = gtable_add_grob(gt,
                                list(grob),
                                .subset2(panel_loc, "t"),
                                .subset2(panel_loc, "l"),
                                .subset2(panel_loc, "b"),
                                .subset2(panel_loc, "r"),
                                clip = clip,
                                name = sprintf("inset-panel-%d", i),
                                z = inset_z
                            )
                        )
                    }
                }
            }
            gt
        }
    )
}
