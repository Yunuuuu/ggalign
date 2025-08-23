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
    facets <- compact_facets(facets)
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
    if (!is.numeric(sector_spacing)) {
        cli_abort("{.arg sector_spacing} must be a {.cls numeric}")
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
    ggproto(
        NULL,
        FacetSector,
        sector_spacing = sector_spacing,
        params = list(
            facets = facets,
            strip.position = "top",
            drop = drop, ncol = NULL, nrow = 1L,
            free = list(x = TRUE, y = FALSE),
            space_free = list(x = TRUE, y = FALSE),
            labeller = ggplot2::label_value, dir = "lt",
            draw_axes = list(x = TRUE, y = FALSE),
            axis_labels = list(x = TRUE, y = FALSE),
            as.table = TRUE
        )
    )
}

#' @importFrom ggplot2 ggplot_add
#' @export
ggplot_add.FacetSector <- function(object, plot, object_name, ...) {
    plot <- NextMethod()
    if (!inherits(plot, "ggalign_facet_sector_plot")) {
        plot <- add_class(plot, "ggalign_facet_sector_plot")
    }
    plot
}

#' @importFrom ggplot2 ggplot_build ggproto ggproto_parent
#' @export
ggplot_build.ggalign_facet_sector_plot <- function(plot, ...) {
    if (inherits(plot$facet, "FacetSector")) {
        if (!inherits(plot$coordinates, "CoordRadial")) {
            if (!isTRUE(plot$coordinates$default)) {
                cli_abort(c(
                    paste(
                        "Cannot use {.fn {snake_class(plot$coordinates)}}",
                        "coordinate with {.fn facet_sector}"
                    ),
                    i = "Please use {.fn coord_circle}/{.fn coord_radial} instead"
                ))
            }
            plot$coordinates <- coord_circle()
        }
    }
    NextMethod()
}

#' @importFrom rlang inject
#' @importFrom grid gTree editGrob viewport
#' @importFrom ggplot2 ggproto ggproto_parent
FacetSector <- ggproto(
    "FacetSector", ggplot2::FacetWrap,
    setup_panel_params = function(self, panel_params, coord, ...) {
        # Compute the total angular span (theta)
        arc_theta <- abs(diff(coord$arc))

        # Weight of each panel is determined by its angular range
        panel_weights <- vapply(panel_params, function(panel_param) {
            abs(diff(.subset2(panel_param, "theta.range")))
        }, numeric(1L), USE.NAMES = FALSE)

        # For a full circle, arc_theta == 2π
        # If not a full circle, the last spacing is not included
        n_spacing <- if (abs(arc_theta - 2 * pi) < sqrt(.Machine$double.eps)) {
            length(panel_weights)
        } else {
            length(panel_weights) - 1L
        }

        # Convert relative spacing into angular spacing (theta units)
        spacing <- if (inherits(self$sector_spacing, "rel")) {
            self$sector_spacing * arc_theta
        } else {
            self$sector_spacing
        }
        spacing <- rep_len(spacing, length(panel_weights))

        # Effective angular span available for panels (subtract spacings)
        panel_theta <- arc_theta - sum(spacing[seq_len(n_spacing)])

        if (panel_theta <= 0L) {
            cli_abort("No panel area, try to reduce {.arg sector_spacing}")
        }

        # Distribute panel arcs proportionally by weight and insert spacing
        panel_breaks <- vec_interleave(
            panel_theta * panel_weights / sum(panel_weights),
            spacing
        )
        panel_breaks <- cumsum(c(coord$arc[1L], utils::head(panel_breaks, -1L)))

        # Assign arc and bounding box to each panel
        for (i in seq_along(panel_params)) {
            panel_param <- .subset2(panel_params, i)
            panel_param$arc <- panel_breaks[i * 2L - 1:0]
            panel_param$bbox <- ggfun("polar_bbox")(
                panel_param$arc, margin = c(0, 0, 0, 0),
                inner_radius = coord$inner_radius
            )
            panel_params[[i]] <- panel_param
        }
        panel_params
    },
    draw_panel_content = function(self, panels, layout, x_scales, y_scales,
                                  ranges, coord, data, theme, params, ...) {
        # Remove redundant vertical axes in middle panels
        for (i in seq_along(ranges)) {
            if (!is.null(ranges[[i]])) {
                # remove extra axis in the median paenl
                guide_positions <- lapply(
                    ranges[[i]]$guides$params,
                    `[[`, "position"
                )
                if (i == 1L || i == length(ranges)) {
                    # For the first panel, drop left guides
                    if (i == 1L) {
                        idx <- which(vapply(guide_positions, identical,
                            logical(1),
                            y = "left", USE.NAMES = FALSE
                        ))
                        ranges[[i]]$guides$guides[idx] <- rep_len(
                            list(ggplot2::guide_none()), length(idx)
                        )
                    }
                    # For the last panel, drop right guides
                    if (i == length(ranges)) {
                        idx <- which(vapply(guide_positions, identical,
                            logical(1),
                            y = "right", USE.NAMES = FALSE
                        ))
                        ranges[[i]]$guides$guides[idx] <- rep_len(
                            list(ggplot2::guide_none()), length(idx)
                        )
                    }
                } else {
                    # For intermediate panels, drop both left and right guides
                    idx <- which(vapply(guide_positions, function(position) {
                        identical(position, "left") ||
                            identical(position, "right")
                    }, logical(1), USE.NAMES = FALSE))
                    ranges[[i]]$guides$guides[idx] <- rep_len(
                        list(ggplot2::guide_none()), length(idx)
                    )
                }
            }
        }
        # Delegate back to FacetWrap’s panel drawing
        ggproto_parent(ggplot2::FacetWrap, self)$draw_panel_content(
            panels, layout, x_scales, y_scales,
            ranges, coord, data, theme, params, ...
        )
    },
    draw_panels = function(self, panels, layout, x_scales, y_scales, ranges,
                           coord, data, theme, params) {
        # Merge all sectors into a single circular panel
        bbox <- ggfun("polar_bbox")(
            coord$arc, margin = c(0, 0, 0, 0),
            inner_radius = coord$inner_radius
        )
        # Adjust viewport of each sector panel so they align within the circle
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

        # Combine all panels into a single gTree
        panels <- gTree(children = inject(gList(!!!panels)))

        # Update ranges so arc and bbox match the coord system
        ranges <- lapply(ranges, function(panel_param) {
            panel_param$arc <- coord$arc
            panel_param$bbox <- bbox
            panel_param
        })

        # Draw as a single null facet
        ggplot2::FacetNull$draw_panels(
            panels = list(panels),
            layout = layout, x_scales = x_scales, y_scales = y_scales,
            ranges = ranges, coord = coord, data = data,
            theme = theme, params = params
        )
    }
)
