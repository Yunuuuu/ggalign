build_heatmap <- function(x) {
    # prepare data for ggplot2
    matrix <- x$matrix
    row_nms <- rownames(matrix)
    col_nms <- colnames(matrix)
    data <- as_tibble0(matrix)
    colnames(data) <- seq_len(ncol(data))
    data$.row_index <- seq_len(nrow(data))
    data <- tidyr::pivot_longer(data,
        cols = !".row_index",
        names_to = ".column_index",
        values_to = "values"
    )
    data$.column_index <- as.integer(data$.column_index)
    data$.x <- data$.column_index
    data$.y <- data$.row_index
    p <- ggplot2::ggplot(data, ggplot2::aes(.data$.x, .data$.y))
    if (!isTRUE(x$blank)) {
        p <- p + ggplot2::geom_tile(
            ggplot2::aes(fill = .data$values),
            width = 1L, height = 1L
        ) + ggplot2::labs(fill = x$name)
    }
    if (!is.null(x$draw_fn)) {
        p <- rlang::inject(x$ggfn(p, !!!x$draw_params))
        if (!ggplot2::is.ggplot(p)) {
            cli::cli_abort(
                "{.arg .fn} must return a {.cls ggplot2} object."
            )
        }
    }

    xlabels <- unique(data[c(".column_index", ".x")])
    xlabels <- structure(col_nms[xlabels$.column_index], names = xlabels$.x)

    ylabels <- unique(data[c(".row_index", ".y")])
    ylabels <- structure(row_nms[ylabels$.row_index], names = ylabels$.y)
    p <- p +
        ggplot2::scale_x_continuous(
            name = x$column_title,
            limits = c(0.5, ncol(matrix) + 0.5),
            breaks = seq_len(ncol(matrix)),
            labels = xlabels,
            expand = ggplot2::expansion()
        ) +
        ggplot2::scale_y_continuous(
            name = x$row_title,
            limits = c(0.5, nrow(matrix) + 0.5),
            breaks = seq_len(nrow(matrix)),
            labels = ylabels,
            expand = ggplot2::expansion()
        )
    p
}
