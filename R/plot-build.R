#' @export
print.ggheatmap <- function(x, ...) {
    p <- ggheat_build(x)
    print(p)
}

ggscale_map <- function(plot, scale, value) {
    build <- ggplot2::ggplot_build(plot)
    if (!is.null(scale <- build$plot$scales$get_scales(scale))) {
        scale$map(value)
    } else {
        cli::cli_abort("Cannot find {.field {scale}}")
    }
}

ggheat_build <- function(x) UseMethod("ggheat_build")

#' @export
ggheat_build.ggheatmap <- function(x) {
    mat <- slot(x, "matrix")
    params <- slot(x, "params")

    # prepare data for plot -------------------------------
    data <- melt_matrix(mat)

    # contruct the final plot ----------------------------
    p <- slot(x, "heatmap")
    p$data <- data
    p <- p +
        ggplot2::geom_tile(
            ggplot2::aes(
                .data$.column_index, .data$.row_index,
                fill = .data$value
            ),
            width = 1L, height = 1L,
            data = data
        )
    xlabels <- .subset2(params, "xlabels") %||% colnames(mat)
    ylabels <- .subset2(params, "ylabels") %||% rownames(mat)

    # https://stackoverflow.com/questions/72402570/why-doesnt-gplot2labs-overwrite-update-the-name-argument-of-scales-function
    # There are multiple ways to set labels in a plot, which take different
    # priorities. Here are the priorities from highest to lowest.
    # 1. The guide title.
    # 2. The scale name.
    # 3. The `labs()` function.
    # 4. The captured expression in aes().
    # So we should update position scale label here since scale name has higher
    # priority than `labs()` function
    p +
        ggplot2::scale_x_continuous(
            name = .subset2(params, "xlab"),
            limits = c(0.5, ncol(mat) + 0.5),
            breaks = seq_len(ncol(mat)),
            labels = xlabels,
            expand = ggplot2::expansion()
        ) +
        ggplot2::scale_y_continuous(
            name = .subset2(params, "ylab"),
            limits = c(0.5, nrow(mat) + 0.5),
            breaks = seq_len(nrow(mat)),
            labels = ylabels,
            expand = ggplot2::expansion()
        )
}

#' @export
gganno_build <- function(x) UseMethod("gganno_build")
