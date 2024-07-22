#' Draw Heatmap rows/columns titles
#' @param titles A character of titles, must be of length with the same number
#' of heatmap rows/columns panels.
#' @param ... Additional arguments passed to [geom_text][ggplot2::geom_text].
#' @inheritParams ggplot2::ggplot
#' @inheritParams htanno
#' @inherit htanno return
#' @examples
#' small_mat <- matrix(rnorm(81), nrow = 9)
#' ggheat(small_mat) +
#'     htanno_group(
#'         sample(letters[1:4], ncol(small_mat), replace = TRUE),
#'         position = "top"
#'     ) +
#'     htanno_title()
#' @export
htanno_title <- function(titles = NULL, ..., mapping = aes(),
                         size = unit(1, "cm"),
                         set_context = TRUE, order = NULL, name = NULL,
                         position = NULL) {
    assert_mapping(mapping)
    htanno(HtannoTitle,
        params = list(
            mapping = mapping, titles = titles,
            text_params = rlang::list2(...)
        ),
        labels = NULL, labels_nudge = NULL,
        position = position, size = size, data = NULL,
        set_context = set_context,
        order = order, name = name
    )
}

HtannoTitle <- ggplot2::ggproto("HtannoTitle", HtannoProto,
    ggplot = function(self, mapping, text_params) {
        ans <- ggplot2::ggplot(mapping = mapping) +
            rlang::inject(ggplot2::geom_text(!!!text_params)) +
            ggplot2::theme_void()
        add_default_mapping(ans, switch_position(
            .subset2(self, "position"),
            aes(x = 0L, y = .data$.y, label = .data$.label),
            aes(y = 0L, x = .data$.x, label = .data$.label)
        ))
    },
    draw = function(self, panels, index, titles) {
        position <- .subset2(self, "position")
        axis <- to_coord_axis(position)
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
                sprintf(
                    "{.arg titles} must be of length of %s %s %s (%d)",
                    "the same number with heatmap",
                    to_matrix_axis(position), "panels", nlevels(panels)
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
