ggplot_add.FacetChannel <- function(object, plot, object_name) {
    plot <- NextMethod()
    if (!inherits(plot, "ggalign_facet_channel_plot")) {
        plot <- add_class(plot, "ggalign_facet_channel_plot")
    }
    plot
}

facet_channel_draw_facet_panels <- function(self, panels, ...) {
    panels <- lapply(panels, function(layer) lapply(layer, ensure_grob))
    ggproto_parent(ggplot2::Facet, self)$draw_facet_panels(panels, ...)
}

facet_channel_draw_panels <- function(self, panels, layout, x_scales, y_scales,
                                      ranges, coord, data, theme, params) {
    coord_fg <- lapply(seq_along(panels[[1]]), function(i) {
        coord_fg <- coord$render_fg(ranges[[i]], theme)
        ggproto_parent(Coord, coord)$draw_panel(
            coord_fg, ranges[[i]], theme
        )
    })
    coord_bg <- lapply(seq_along(panels[[1]]), function(i) {
        coord_bg <- coord$render_bg(ranges[[i]], theme)
        ggproto_parent(Coord, coord)$draw_panel(
            coord_bg, ranges[[i]], theme
        )
    })
    names <- paste("layer", seq_along(panels), sep = "-")
    panels <- c(list(facet_bg), panels, list(facet_fg))
    names <- c("facet-bg", names, "facet-fg")
    panels <- lapply(panels, function(panel) {
        # let Coord modify the panel
        lapply(seq_along(panel), function(i) {
            coord$draw_panel(panel[[i]], ranges[[i]], theme)
        })
    })

    if (isTRUE(theme$panel.ontop)) {
        panels <- c(panels, list(coord_bg), list(coord_fg))
        names <- c(names, "coord-bg", "coord-fg")
    } else {
        panels <- c(list(coord_bg), panels, list(coord_fg))
        names <- c("coord-bg", names, "coord-fg")
    }
    for (i in seq_along(panels)) {
        table <- gtable_add_grob(
            table, panels[[i]],
            t = layout$ROW,
            l = layout$COL,
            # when drawing, the grob with the same `z` will be drawn in the
            # ordering they added
            z = 1,
            name = paste("panel", layout$COL, layout$ROW, names[[i]], sep = "-")
        )
    }
}
