#' Heatmap annotation with `HtannoGG`
#'
#' @inheritParams htanno
#' @importFrom ggplot2 aes
#' @inheritParams ggplot2::ggplot
#' @return A `HtannoGG` object.
#' @examples
#' ggheat(matrix(rnorm(81), nrow = 9)) +
#'     gganno(position = "top") +
#'     geom_point(aes(y = value))
#' @importFrom rlang caller_call current_call
#' @export
htanno_gg <- function(mapping = aes(), data = NULL, size = NULL,
                      labels = NULL, labels_nudge = NULL,
                      set_context = TRUE, order = NULL, name = NULL,
                      position = NULL) {
    assert_mapping(mapping)
    htanno(HtannoGG,
        params = list(mapping = mapping),
        labels = labels, labels_nudge = labels_nudge,
        position = position, size = size, data = data,
        set_context = set_context, order = order, name = name
    )
}

#' @export
#' @rdname htanno_gg
gganno <- htanno_gg

#' @export
#' @rdname htanno_gg
gganno_top <- function(...) htanno_gg(position = "top", ...)

#' @export
#' @rdname htanno_gg
gganno_bottom <- function(...) htanno_gg(position = "bottom", ...)

#' @export
#' @rdname htanno_gg
gganno_left <- function(...) htanno_gg(position = "left", ...)

#' @export
#' @rdname htanno_gg
gganno_right <- function(...) htanno_gg(position = "right", ...)

HtannoGG <- ggplot2::ggproto("HtannoGG", HtannoProto,
    ggplot = function(self, panels, index, mapping) {
        ans <- ggplot2::ggplot(mapping = mapping) +
            ggplot2::theme_bw()

        add_default_mapping(ans, switch_position(
            .subset2(self, "position"),
            aes(y = .data$.y),
            aes(x = .data$.x)
        ))
    },
    draw = function(self, panels, index) {
        data <- .subset2(self, "data")
        axis <- to_coord_axis(.subset2(self, "position"))
        # matrix: will be reshaped to the long-format data.frame
        # data.frame: won't do any thing special
        if (is.matrix(data)) {
            data <- melt_matrix(data)
        } else {
            data <- as_tibble0(data, rownames = ".row_names")
            data$.row_index <- seq_len(nrow(data))
        }
        coords <- data_frame0(.panel = panels[index], .index = index)
        coords[[paste0(".", axis)]] <- seq_along(index)
        data <- merge(data, coords,
            by.x = ".row_index", by.y = ".index",
            sort = FALSE, all = TRUE
        )
        plot <- .subset2(self, "plot")
        plot$data <- data
        plot
    }
)
