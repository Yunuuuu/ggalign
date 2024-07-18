anno_build <- function(x, panels, index, scales, facet, position) {
    UseMethod("anno_build")
}

#' @export
anno_build.gganno <- function(x, panels, index, scales, facet, position) {
    data <- anno_build_data(slot(x, "data"), panels, index, position)
    plot <- slot(x, "plot")
    plot$data <- data
    plot <- anno_add_default_mapping(plot, switch_position(
        position,
        aes(y = .data$.y),
        aes(x = .data$.x)
    ))
    anno_set_scales_and_facet(
        plot,
        slot(x, "facetted_pos_scales"),
        position, scales, facet,
        type = "gganno"
    )
}

#' @importFrom ggplot2 is.ggplot
#' @export
anno_build.htanno <- function(x, panels, index, scales, facet, position) {
    htanno <- slot(x, "htanno")
    # let `htanno` to determine how to draw
    plot <- rlang::inject(htanno$draw(
        slot(x, "data"),
        panels, index, position,
        !!!htanno$draw_params
    ))
    if (is.null(plot)) return(plot) # styler: off
    anno_set_scales_and_facet(
        plot,
        slot(x, "facetted_pos_scales"),
        position, scales, facet,
        type = "htanno"
    )
}

anno_build_data <- function(data, panels, index, position) {
    # annotation accepts two class of data
    # matrix: will be reshaped to the long-format data.frame
    # data.frame: won't do any thing special
    if (is.matrix(data)) {
        data <- melt_matrix(data)
    } else {
        data <- as_tibble0(data, rownames = ".row_names")
        data$.row_index <- seq_len(nrow(data))
    }
    coords <- data_frame0(.panel = panels[index], .index = index)
    coords[[paste0(".", to_coord_axis(position))]] <- seq_along(index)
    merge(data, coords,
        by.x = ".row_index", by.y = ".index",
        sort = FALSE, all = TRUE
    )
}

#' @importFrom ggplot2 aes
anno_add_default_mapping <- function(plot, default_mapping) {
    mapping <- .subset2(plot, "mapping")
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    plot$mapping <- default_mapping
    plot
}

anno_set_scales_and_facet <- function(plot, facetted_pos_scales,
                                      position, default_scales, default_facet,
                                      type) {
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
