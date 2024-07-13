anno_build <- function(x, coords, scale, facet, position) {
    UseMethod("anno_build")
}

#' @export
anno_build.ggannotation <- function(x, coords, scale, facet, position) {
    data <- anno_build_data(slot(x, "data"), coords, position)
    plot <- slot(x, "plot")
    plot$data <- data
    anno_add_default_mapping(plot, position) + scale + facet
}

#' @importFrom ggplot2 is.ggplot
#' @export
anno_build.htanno <- function(x, coords, scale, facet, position) {
    data <- anno_build_data(slot(x, "data"), coords, position)
    htanno <- slot(x, "htanno")
    plot <- rlang::inject(
        htanno$draw(
            data, position, slot(x, "statistics"),
            !!!htanno$reorder_params
        )
    )
    if (is.ggplot(plot)) {
        plot <- anno_add_default_mapping(plot, position) + scale + facet
    }
    plot
}

anno_build_data <- function(data, coords, position) {
    # annotation accepts two class of data
    # matrix: will be reshaped to the long-format data.frame
    # data.frame: won't do any thing special
    if (is.matrix(data)) {
        data <- melt_matrix(data)
    } else {
        data <- as_tibble0(data, rownames = ".row_names")
        data$.row_index <- seq_len(nrow(data))
    }
    coords <- rename(
        coords,
        switch_position(position, c(.coord = ".y"), c(.coord = ".x"))
    )
    merge(data, coords,
        by.x = ".row_index", by.y = ".index",
        sort = FALSE, all = TRUE
    )
}

anno_add_default_mapping <- function(p, position) {
    default_mapping <- switch_position(
        position,
        ggplot2::aes(y = .data$.y),
        ggplot2::aes(x = .data$.x)
    )
    mapping <- .subset2(p, "mapping")
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    p$mapping <- default_mapping
    p
}
