#' @export
print.ggheatmap <- function(x, ...) {
    p <- ggheat_build(x)
    print(p)
}

ggheat_build <- function(x) {
    mat <- .subset2(x, "matrix")
    params <- .subset2(x, "params")

    # prepare data for plot -------------------------------
    row_nms <- rownames(mat)
    col_nms <- colnames(mat)
    data <- as_tibble0(mat, rownames = NULL)
    colnames(data) <- seq_len(ncol(data))
    data$.row_index <- seq_len(nrow(data))
    data <- tidyr::pivot_longer(data,
        cols = !".row_index", names_to = ".column_index",
        values_to = "value"
    )
    data$.column_index <- as.integer(data$.column_index)
    if (!is.null(row_nms)) data$.row_names <- row_nms[data$.row_index]
    if (!is.null(col_nms)) data$.column_names <- col_nms[data$.column_index]

    # contruct the final plot ----------------------------
    p <- .subset2(x, "heatmap")
    p$data <- data
    p <- p +
        ggplot2::geom_tile(
            ggplot2::aes(.data$.column_index, .data$.row_index,
                fill = .data$value
            ),
            width = 1L, height = 1L,
            data = data
        )
    xlabels <- .subset2(x, "xlabels") %||% col_nms
    ylabels <- .subset2(x, "ylabels") %||% row_nms

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
