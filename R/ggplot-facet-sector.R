#' Polar coordinates with Facet support
#'
#' Draw each panel in a sector of the polar coordinate system. If
#' `facet_sector()` is used in a ggplot, the coordinate system must be created
#' with [`coord_circle()`] or [`coord_radial()`][ggplot2::coord_radial].
#'
#' @inheritParams ggplot2::facet_wrap
#' @param sector_spacing The size of spacing between different panel. A numeric
#' of the radians or a [`rel()`][ggplot2::rel] object.
#' @param radial `r lifecycle::badge("deprecated")` Please add the coordinate
#' system directly to the ggplot instead.
#' @param spacing_theta `r lifecycle::badge("deprecated")` Please use
#' `sector_spacing` instead.
#' @examples
#' ggplot(mtcars, aes(disp, mpg)) +
#'     geom_point() +
#'     facet_sector(vars(cyl)) +
#'     coord_circle(
#'         start = -0.4 * pi, end = 0.4 * pi, inner.radius = 0.3,
#'         outer.radius = 0.8, expand = TRUE
#'     )
#' @importFrom ggplot2 ggproto
#' @export
facet_sector <- function(facets, sector_spacing = pi / 180, drop = TRUE,
                         radial = deprecated(), spacing_theta = deprecated()) {
    if (packageVersion("ggplot2") > "3.5.2") {
        facets <- ggfun("compact_facets")(facets)
    } else {
        facets <- ggfun("wrap_as_facets_list")(facets)
    }
    if (inherits(sector_spacing, "CoordRadial") ||
        lifecycle::is_present(radial)) {
        lifecycle::deprecate_stop(
            "1.0.2",
            "facet_sector(radial = )",
            details = "Please add the coordinate to the ggplot instead"
        )
    }
    if (lifecycle::is_present(spacing_theta)) {
        lifecycle::deprecate_warn(
            "1.0.2",
            "facet_sector(spacing_theta = )",
            "facet_sector(sector_spacing = )"
        )
        sector_spacing <- spacing_theta
    }

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
    # the next version of ggplot2 (> 3.5.2)
    if (packageVersion("ggplot2") > "3.5.2") {
        dir <- "lt"
    } else {
        dir <- "h"
    }
    ggproto(
        NULL,
        FacetSector,
        sector_spacing = sector_spacing,
        params = list(
            facets = facets,
            free = list(x = TRUE, y = FALSE),
            strip.position = "top",
            drop = drop, ncol = NULL, nrow = 1L,
            space_free = list(x = TRUE, y = FALSE),
            labeller = ggplot2::label_value, dir = dir,
            draw_axes = list(x = TRUE, y = FALSE),
            axis_labels = list(x = TRUE, y = FALSE),
            as.table = TRUE
        )
    )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.FacetSector <- function(object, plot, object_name) {
    plot <- NextMethod()
    if (!inherits(plot, "ggalign_facet_sector_plot")) {
        plot <- add_class(plot, "ggalign_facet_sector_plot")
    }
    plot
}

#' @importFrom ggplot2 ggplot_build ggproto ggproto_parent
#' @export
ggplot_build.ggalign_facet_sector_plot <- function(plot) {
    if (inherits(plot$facet, "FacetSector")) {
        if (!inherits(plot$coordinates, "CoordRadial")) {
            if (!isTRUE(plot$coordinates$default)) {
                cli_abort(c(
                    paste(
                        "Cannot use {.fn {snake_class(plot$coordinates)}}",
                        "coordinate with {.fn facet_sector}"
                    ),
                    i = "Please use {.fn coord_circle} instead"
                ))
            }
            plot$coordinates <- coord_circle()
        }
        ParentLayout <- plot$layout
        plot$layout <- ggproto(
            "FacetSectorLayout", ParentLayout,
            setup_panel_params = function(self) {
                ggproto_parent(ParentLayout, self)$setup_panel_params()
                if (is.null(ggplot2::Facet$setup_panel_params) &&
                    !is.null(self$facet$setup_panel_params)) {
                    self$panel_params <- self$facet$setup_panel_params(
                        self$panel_params, self$coord
                    )
                }
                invisible()
            }
        )
    }
    NextMethod()
}

#' @importFrom rlang inject
#' @importFrom grid gTree editGrob viewport
#' @importFrom ggplot2 ggproto ggproto_parent
FacetSector <- ggproto(
    "FacetSector", ggplot2::FacetWrap,
    setup_panel_params = function(self, panel_params, coord, ...) {
        # total theta for panel area and panel spacing
        arc_theta <- abs(diff(coord$arc))
        sector_spacing <- self$sector_spacing
        if (inherits(sector_spacing, "rel")) {
            sector_spacing <- sector_spacing * arc_theta
        }
        panel_weights <- vapply(panel_params, function(panel_param) {
            abs(diff(.subset2(panel_param, "theta.range")))
        }, numeric(1L), USE.NAMES = FALSE)

        # total theta for panel area
        panel_theta <- arc_theta -
            # substract the number of spacing between panels
            sector_spacing *
                # for the whole circle, arc_theta == 2 * pi
                # there should be as many panels as the number of panel spacing
                if (abs(arc_theta - 2 * pi) < .Machine$double.eps^0.5) {
                    length(panel_weights)
                } else {
                    length(panel_weights) - 1L
                }
        if (panel_theta <= 0L) {
            cli_abort("No panel area, try to reduce {.arg sector_spacing}")
        }

        # re-distribute the arc for each panel
        panel_point <- vec_interleave(
            panel_theta * panel_weights / sum(panel_weights),
            rep_len(sector_spacing, length(panel_weights))
        )
        panel_point <- cumsum(c(coord$arc[1L], utils::head(panel_point, -1L)))
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
        panels <- gTree(children = inject(gList(!!!panels)))
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
