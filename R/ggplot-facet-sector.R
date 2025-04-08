#' Polar coordinates with Facet support
#'
#' Draw each panel in a sector of the polar coordinate.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param radial A Polar [Coord][ggplot2::Coord] created by [`coord_circle()`].
#' @param spacing_theta The size of spacing between different panel. A numeric
#' of the radians or a [`rel()`][ggplot2::rel] object.
#' @examples
#' ggplot(mtcars, aes(disp, mpg)) +
#'     geom_point() +
#'     facet_sector(
#'         vars(cyl),
#'         coord_circle(
#'             start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.3,
#'             outer.radius = 0.8, expand = TRUE
#'         )
#'     )
#' @importFrom ggplot2 ggproto
#' @export
facet_sector <- function(facets, radial = NULL,
                         spacing_theta = pi / 180, drop = TRUE) {
    # For FacetCircle
    facets <- ggfun(
        version <= "3.5.1" ~ "wrap_as_facets_list",
        "compact_facets"
    )(facets)

    # @param strip.position By default, the labels are displayed on the
    # `"outer"` of the plot. Allowed values are `r oxford_or(c("outer",
    # "inner"))`
    # strip.position <- arg_match0(strip.position, c("outer", "inner"))
    # strip.position <- switch(strip.position,
    #     outer = "top",
    #     inner = "bottom"
    # )
    # labeller <- ggfun("fix_labeller")(labeller)
    assert_bool(drop)

    # TO-DO: remove this line and update to
    # the next version of ggplot2 (> 3.5.1)
    if (packageVersion("ggplot2") <= "3.5.1") {
        dir <- "h"
    } else {
        dir <- "lt"
    }
    radial <- radial %||% coord_circle()
    if (!inherits(radial, "CoordRadial")) {
        cli_abort("{.arg radial} must be created with {.fn coord_circle}")
    }
    structure(
        list(
            facet = ggproto(
                NULL,
                FacetSector,
                params = list(
                    facets = facets,
                    spacing_theta = spacing_theta,
                    free = list(x = TRUE, y = FALSE),
                    strip.position = "top",
                    drop = drop, ncol = NULL, nrow = 1L,
                    space_free = list(x = TRUE, y = FALSE),
                    labeller = ggplot2::label_value, dir = dir,
                    draw_axes = list(x = TRUE, y = FALSE),
                    axis_labels = list(x = TRUE, y = FALSE),
                    as.table = TRUE
                )
            ),
            coord = radial
        ),
        class = "ggalign_facet_sector"
    )
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_facet_sector <- function(object, plot, object_name) {
    ParentLayout <- .subset2(plot, "layout")
    plot$layout <- ggproto(
        NULL, ParentLayout,
        setup_panel_params = function(self) {
            ggproto_parent(ParentLayout, self)$setup_panel_params()
            if (!is.null(self$facet$setup_panel_params)) {
                self$panel_params <- self$facet$setup_panel_params(
                    self$coord, self$panel_params
                )
            }
            invisible()
        }
    )
    ggplot_add(unclass(object), plot, object_name)
}

#' @importFrom grid gTree editGrob viewport
#' @importFrom ggplot2 ggproto ggproto_parent
FacetSector <- ggproto(
    "FacetSector", ggplot2::FacetWrap,
    setup_panel_params = function(self, coord, panel_params) {
        # Ensure the CoordCircle is not changed by the users
        if (!inherits(coord, "CoordRadial")) {
            cli_abort(c(
                "Cannot use {.fn {snake_class(self)}}",
                "coordinate must be created with {.fn coord_circle}"
            ))
        }

        # total theta for panel area and panel spacing
        arc_theta <- abs(diff(coord$arc))
        spacing_theta <- self$params$spacing_theta
        if (inherits(spacing_theta, "rel")) {
            spacing_theta <- spacing_theta * arc_theta
        }
        panel_weights <- vapply(panel_params, function(panel_param) {
            abs(diff(.subset2(panel_param, "theta.range")))
        }, numeric(1L), USE.NAMES = FALSE)

        # total theta for panel area
        panel_theta <- arc_theta -
            # substract the number of spacing between panels
            spacing_theta *
                # for the whole circle, arc_theta == 2 * pi
                # there should be as many panels as the number of panel spacing
                if (abs(arc_theta - 2 * pi) < .Machine$double.eps^0.5) {
                    length(panel_weights)
                } else {
                    length(panel_weights) - 1L
                }
        if (panel_theta <= 0L) {
            cli_abort("No panel area, try to reduce {.arg spacing_theta}")
        }

        # re-distribute the arc for each panel
        panel_point <- vec_interleave(
            panel_theta * panel_weights / sum(panel_weights),
            rep_len(spacing_theta, length(panel_weights))
        )
        panel_point <- cumsum(
            c(coord$arc[1L], utils::head(panel_point, -1L))
        )
        for (i in seq_along(panel_params)) {
            panel_param <- .subset2(panel_params, i)
            panel_param$arc <- panel_point[i * 2L - 1:0]
            panel_param$bbox <- ggfun("polar_bbox")(
                panel_param$arc, margin = c(0, 0, 0, 0),
                inner_radius = coord$inner_radius
            )
            panel_params[[i]] <- panel_param
        }
        panel_params
    },
    draw_panels = function(self, panels, layout, x_scales, y_scales, ranges,
                           coord, data, theme, params) {
        if (!inherits(coord, "CoordRadial")) {
            cli_abort(c(
                "Cannot use {.fn {snake_class(self)}}",
                "coordinate must be created with {.fn coord_circle}"
            ))
        }
        # merge different sector into one panel
        bbox <- ggfun("polar_bbox")(
            coord$arc, margin = c(0, 0, 0, 0),
            inner_radius = coord$inner_radius
        )
        for (i in seq_along(panels)) {
            panel_param <- .subset2(ranges, i)
            vp <- list(
                x = scales::rescale(panel_param$bbox$x, from = bbox$x),
                y = scales::rescale(panel_param$bbox$y, from = bbox$y)
            )
            panels[[i]] <- editGrob(
                .subset2(panels, i),
                vp = viewport(
                    x = vp$x[1L], y = vp$y[1L],
                    width = abs(diff(vp$x)),
                    height = abs(diff(vp$y)),
                    just = c(0, 0),
                    clip = "off",
                    default.units = "native"
                )
            )
        }
        panels <- gTree(children = rlang::inject(gList(!!!panels)))
        ranges <- lapply(ranges, function(panel_param) {
            panel_param$arc <- coord$arc
            panel_param$bbox <- bbox
            panel_param
        })
        ggplot2::FacetNull$draw_panels(
            panels = list(panels),
            layout = layout, x_scales = x_scales, y_scales = y_scales,
            ranges = ranges, coord = coord, data = data,
            theme = theme, params = params
        )
    }
)
