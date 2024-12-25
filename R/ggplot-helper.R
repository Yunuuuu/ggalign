# Exported function for ggplot2
# Usually a quick shortcuts to define something
#
#' Remove axis elements
#'
#' @param axes Which axes elements should be removed? A string containing
#' one or more of `r oxford_and(c(.tlbr, "x", "y"))`.
#' @param text If `TRUE`, will remove the axis labels.
#' @param ticks If `TRUE`, will remove the axis ticks.
#' @param title If `TRUE`, will remove the axis title.
#' @param line If `TRUE`, will remove the axis line.
#' @return A [`theme()`][ggplot2::theme] object.
#' @examples
#' p <- ggplot() +
#'     geom_point(aes(x = wt, y = qsec), data = mtcars)
#' p + theme_no_axes()
#' p + theme_no_axes("b")
#' p + theme_no_axes("l")
#' @importFrom rlang inject
#' @importFrom ggplot2 theme element_blank
#' @export
theme_no_axes <- function(axes = "xy", text = TRUE, ticks = TRUE,
                          title = TRUE, line = FALSE) {
    assert_string(axes, empty_ok = FALSE)
    if (grepl("[^tlbrxy]", axes)) {
        cli_abort(sprintf(
            "{.arg axes} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    }
    axes <- split_position(axes)
    el <- list(text = text, ticks = ticks, title = title, line = line)
    el <- names(el)[vapply(el, isTRUE, logical(1L), USE.NAMES = FALSE)]
    el_axis <- el_pos <- NULL
    if (length(positions <- vec_set_intersect(axes, .tlbr))) {
        positions <- .subset(
            c(t = "top", l = "left", b = "bottom", r = "right"),
            positions
        )
        el_pos <- vec_expand_grid(pos = positions, el = el)
        el_pos <- paste("axis",
            .subset2(el_pos, "el"),
            if_else(.subset2(el_pos, "pos") %in% c("top", "bottom"), "x", "y"),
            .subset2(el_pos, "pos"),
            sep = "."
        )
    }
    if (length(axes <- vec_set_intersect(axes, c("x", "y")))) {
        el_axis <- vec_expand_grid(axes = axes, el = el)
        el_axis <- paste("axis",
            .subset2(el_axis, "el"), .subset2(el_axis, "axes"),
            sep = "."
        )
    }
    el <- c(el_axis, el_pos)
    el <- vec_set_names(vec_rep(list(element_blank()), length(el)), el)
    inject(theme(!!!el, validate = FALSE))
}

#' Theme Polygon elements
#'
#' Draw polygon.
#'
#' @inheritParams ggplot2::element_rect
#' @inheritParams geom_rect3d
#' @inheritParams ggplot2::fill_alpha
#' @seealso [`element_rect`][ggplot2::element_rect]
#' @return A `element_polygon` object
#' @export
element_polygon <- function(fill = NULL, colour = NULL, linewidth = NULL,
                            linetype = NULL, alpha = NULL, lineend = NULL,
                            linejoin = NULL, linemitre = NULL, color = NULL,
                            inherit.blank = FALSE) {
    if (!is.null(color)) colour <- color
    structure(
        list(
            fill = fill, colour = colour, alpha = alpha,
            linewidth = linewidth, linetype = linetype,
            lineend = lineend, linejoin = linejoin, linemitre = linemitre,
            inherit.blank = inherit.blank
        ),
        class = c("element_polygon", "element")
    )
}

#' @importFrom grid gpar
#' @importFrom ggplot2 element_grob
#' @export
element_grob.element_polygon <- function(element, x, y,
                                         width = 1, height = 1, fill = NULL,
                                         colour = NULL, linewidth = NULL, linetype = NULL, ...) {
    gp <- gpar(
        lwd = ggfun("len0_null")(linewidth * .pt),
        col = colour,
        fill = fill,
        lty = linetype
    )
    element_gp <- gpar(
        lwd = ggfun("len0_null")(element$linewidth * .pt),
        col = element$colour,
        fill = fill_alpha(element$fill, element$alpha %||% NA),
        lty = element$linetype,
        lineend = element$lineend,
        linejoin = element$linejoin,
        linemitre = element$linemitre
    )
    grid::polygonGrob(
        x = x, y = y,
        gp = ggfun("modify_list")(element_gp, gp), ...
    )
}

######################################################
#' Remove scale expansion
#'
#' @param borders Which border should be removed? A string containing
#' one or more of `r oxford_and(c(.tlbr, "x", "y"))`.
#' @return An object which can be added to ggplot.
#' @export
no_expansion <- function(borders) {
    assert_string(borders, empty_ok = FALSE)
    if (grepl("[^tlbrxy]", borders)) {
        cli_abort(sprintf(
            "{.arg borders} can only contain the %s characters",
            oxford_and(c(.tlbr, "x", "y"))
        ))
    }
    borders <- .subset(list(
        t = "top", l = "left", b = "bottom", r = "right",
        x = c("left", "right"), y = c("bottom", "top")
    ), split_position(borders))
    borders <- vec_unique(unlist(borders, recursive = FALSE, use.names = FALSE))
    # expansion in x-axis
    structure(list(borders = borders), class = c("ggalign_no_expansion"))
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_no_expansion <- function(object, plot, object_name) {
    ParentFacet <- .subset2(plot, "facet")
    borders <- .subset2(object, "borders")
    plot$facet <- ggproto(
        NULL, ParentFacet,
        init_scales = function(self, layout, x_scale = NULL,
                               y_scale = NULL, params) {
            if (!is.null(x_scale) && any(borders == c("top", "bottom"))) {
                expansion <- x_scale$expand %|w|%
                    ggfun("default_expansion")(x_scale, expand = TRUE)
                if (any(borders == "bottom")) {
                    expansion[1:2] <- 0
                }
                if (any(borders == "top")) {
                    expansion[3:4] <- 0
                }
                x_scale$expand <- expansion
            }
            if (!is.null(y_scale) && any(borders == c("left", "right"))) {
                expansion <- y_scale$expand %|w|%
                    ggfun("default_expansion")(y_scale, expand = TRUE)
                if (any(borders == "left")) {
                    expansion[1:2] <- 0
                }
                if (any(borders == "right")) {
                    expansion[3:4] <- 0
                }
                y_scale$expand <- expansion
            }
            ggproto_parent(ParentFacet, self)$init_scales(
                layout = layout,
                x_scale = x_scale,
                y_scale = y_scale,
                params = params
            )
        }
    )
    plot
}
