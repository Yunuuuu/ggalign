anno_build <- function(x, index, panels, scales, facet, position) {
    UseMethod("anno_build")
}

#' @export
anno_build.gganno <- function(x, index, panels, scales, facet, position) {
    data <- anno_build_data(slot(x, "data"), index, panels, position)
    plot <- slot(x, "plot")
    plot$data <- data
    plot <- anno_add_default_mapping(plot, position)
    anno_set_scales_and_facet(
        plot,
        slot(x, "facetted_pos_scales"),
        position, scales, facet
    )
}

anno_build_data <- function(data, index, panels, position) {
    # annotation accepts two class of data
    # matrix: will be reshaped to the long-format data.frame
    # data.frame: won't do any thing special
    if (is.matrix(data)) {
        data <- melt_matrix(data)
    } else {
        data <- as_tibble0(data, rownames = ".row_names")
        data$.row_index <- seq_len(nrow(data))
    }
    coords <- tibble0(.panel = panels[index], .index = index)
    coords[[switch_position(position, ".y", ".x")]] <- seq_along(index)
    merge(data, coords,
        by.x = ".row_index", by.y = ".index",
        sort = FALSE, all = TRUE
    )
}

#' @importFrom ggplot2 is.ggplot
#' @export
anno_build.htanno <- function(x, index, panels, scales, facet, position) {
    htanno <- slot(x, "htanno")
    # let `htanno` to determine how to draw
    plot <- rlang::inject(htanno$draw(
        slot(x, "data"),
        slot(x, "statistics"),
        panels, index, position,
        !!!htanno$draw_params
    ))
    if (is.ggplot(plot)) {
        plot <- anno_add_default_mapping(plot, position)
        plot <- anno_set_scales_and_facet(
            plot,
            slot(x, "facetted_pos_scales"),
            position, scales, facet
        )
    }
    plot
}

#' @importFrom ggplot2 aes
anno_add_default_mapping <- function(plot, position) {
    default_mapping <- switch_position(
        position,
        aes(y = .data$.y),
        aes(x = .data$.x)
    )
    mapping <- .subset2(plot, "mapping")
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    plot$mapping <- default_mapping
    plot
}

anno_set_scales_and_facet <- function(plot, facetted_pos_scales,
                                      position, scales, facet) {
    user_scales <- ggheat_extract_scales(
        switch_position(position, "y", "x"),
        plot = plot, n = length(scales),
        facet_scales = facetted_pos_scales
    )
    for (i in seq_along(scales)) {
        scale <- ggheat_melt_scale(
            .subset2(user_scales, i),
            .subset2(scales, i)
        )
        # we always remove labels and breaks of annotation.
        scale$labels <- NULL
        scale$breaks <- NULL
        user_scales[[i]] <- scale
    }
    user_facet <- ggheat_melt_facet(.subset2(plot, "facet"), facet)
    if (is.null(facet)) { # no panels
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

anno_combine_scales <- function(scales) {
    ans <- .subset2(scales, 1L)$clone()
    components <- lapply(scales, function(scale) {
        list(
            limits = scale$limits,
            breaks = scale$breaks,
            labels = scale$labels
        )
    })
    components <- transpose(components)
    ans$limits <- range(unlist(.subset2(components, "limits"), FALSE, FALSE))
    # we always remove labels and breaks of annotation.
    ans$breaks <- NULL
    ans$labels <- NULL
    # ans$breaks <- unlist(.subset2(components, "breaks"), FALSE, FALSE)
    # ans$labels <- unlist(.subset2(components, "labels"), FALSE, FALSE)
    ans
}
