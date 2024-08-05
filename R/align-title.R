#' Draw Heatmap rows/columns titles
#' @param titles A character of titles, must be of length with the same number
#' of heatmap rows/columns panels.
#' @param ... Additional arguments passed to [geom_text][ggplot2::geom_text].
#' @inheritParams ggplot2::ggplot
#' @inheritParams align
#' @inherit align return
#' @examples
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheatmap(small_mat) +
#'     hmanno("t") +
#'     align_group(sample(letters[1:4], ncol(small_mat), replace = TRUE)) +
#'     align_title()
#' @export
align_title <- function(titles = NULL, ..., mapping = aes(),
                        size = unit(1, "cm"),
                        set_context = TRUE, order = NULL, name = NULL) {
    assert_mapping(mapping)
    align(AlignTitle,
        params = list(
            mapping = mapping, titles = titles,
            text_params = rlang::list2(...)
        ),
        size = size, data = NULL,
        set_context = set_context,
        order = order, name = name
    )
}

AlignTitle <- ggplot2::ggproto("AlignTitle", Align,
    ggplot = function(self, mapping, text_params) {
        ans <- ggplot2::ggplot(mapping = mapping) +
            rlang::inject(ggplot2::geom_text(!!!text_params)) +
            ggplot2::theme_void()
        add_default_mapping(ans, switch_direction(
            .subset2(self, "direction"),
            aes(x = 0L, y = .data$.y, label = .data$.label),
            aes(y = 0L, x = .data$.x, label = .data$.label)
        ))
    },
    draw = function(self, panels, index, extra_panels, extra_index, titles) {
        direction <- .subset2(self, "direction")
        axis <- to_coord_axis(direction)
        coords <- data_frame0(.panel = panels[index], .index = index)
        coords[[paste0(".", axis)]] <- seq_along(index)
        formula <- rlang::new_formula(
            rlang::sym(paste0(".", axis)),
            quote(.panel)
        )
        plot <- .subset2(self, "plot")
        data <- stats::aggregate(formula, data = coords, median)
        if (is.null(titles)) {
            titles <- as.character(data$.panel)
        } else if (length(titles) != nlevels(panels)) {
            cli::cli_abort(
                paste(
                    "{.arg titles} must be of length of",
                    "the same number of the layout",
                    to_matrix_axis(direction), "-axis panels (",
                    nlevels(panels), ")"
                ),
                call = .subset2(self, "call")
            )
        } else if (!is.atomic(titles)) {
            cli::cli_abort(
                "{.arg titles} must be an atomic vector",
                call = .subset2(self, "call")
            )
        } else if (rlang::is_named(titles)) {
            titles <- titles[as.character(data$.panel)]
        } else {
            titles <- as.character(titles)
        }
        data$.label <- titles
        plot$data <- data
        plot
    }
)
