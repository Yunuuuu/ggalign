#' @importFrom rlang is_empty
annotations_build <- function(annotations, panels, index, scales,
                              facet, position) {
    scale_name <- to_coord_axis(position)
    plots <- lapply(annotations, function(x) {
        # we merge anno `labels` and `labels_nudge`
        # we also extract the expand from the heatmap
        default_scales <- ggheat_default_scale(
            scale_name = scale_name, panels = panels, index = index,
            labels = .subset2(x, "labels"),
            nudge = .subset2(x, "labels_nudge"),
            expand = lapply(scales, function(scale) .subset2(scale, "expand"))
        )
        htanno_build(
            htanno = x,
            panels = panels,
            index = index,
            scales = default_scales,
            facet = facet
        )
    })

    # we reorder annotation based on the `order` slot
    index <- order(vapply(annotations, .subset2, integer(1L), name = "order"))
    plots <- .subset(plots, index)

    # combine all annotations
    keep <- lengths(plots) > 0L
    plots <- .subset(plots, keep)
    if (is_empty(plots)) return(list(NULL, NULL)) # styler: off

    sizes <- lapply(.subset(annotations, index), .subset2, name = "size")
    sizes <- do.call(unit.c, sizes)
    sizes <- sizes[keep]
    list(plots, sizes)
}

##################################################################
#' @importFrom ggplot2 theme element_blank
htanno_build <- function(htanno, panels, index, scales, facet) {
    if (is.null(.subset2(htanno, "plot"))) return(NULL) # styler: off
    htanno$lock()
    on.exit(htanno$unlock())
    # let `htanno` to determine how to draw
    # 1. add default layer
    # 2. add plot data
    params <- .subset2(htanno, "params")
    draw_params <- params[
        intersect(names(params), htanno_method_params(htanno$draw))
    ]
    ans <- rlang::inject(htanno$draw(panels, index, !!!draw_params))
    # remove the title of axis parallelly with heatmap
    ans <- ans + switch_position(
        .subset2(htanno, "position"),
        theme(axis.title.y = element_blank()),
        theme(axis.title.x = element_blank())
    )
    # in the finally, we ensure the scale limits is the same with heatmap
    htanno_set_scales_and_facet(
        plot = ans,
        .subset2(htanno, "facetted_pos_scales"),
        .subset2(htanno, "position"),
        scales, facet
    )
}

add_default_mapping <- function(plot, default_mapping) {
    mapping <- .subset2(plot, "mapping")
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    plot$mapping <- default_mapping
    plot
}

htanno_set_scales_and_facet <- function(plot, facetted_pos_scales,
                                        position, default_scales,
                                        default_facet) {
    axis <- to_coord_axis(position)
    user_scales <- ggheat_extract_scales(
        axis,
        plot = plot, n = length(default_scales),
        facet_scales = facetted_pos_scales
    )
    for (i in seq_along(default_scales)) {
        user_scales[[i]] <- ggheat_melt_scale(
            .subset2(user_scales, i),
            .subset2(default_scales, i),
            set_expand = TRUE
        )
    }
    user_facet <- ggheat_melt_facet(.subset2(plot, "facet"), default_facet)
    if (is.null(default_facet)) { # no panels
        plot <- plot + user_scales + user_facet
    } else {
        plot <- plot + user_facet +
            switch_position(
                position,
                ggh4x::facetted_pos_scales(y = user_scales),
                ggh4x::facetted_pos_scales(x = user_scales)
            )
    }
    plot
}
