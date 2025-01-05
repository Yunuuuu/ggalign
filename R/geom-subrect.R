#' Subdivide Rectangles
#'
#' @description
#' These geoms subdivide rectangles with shared borders into a grid. Both geoms
#' achieve the same result but differ in how the rectangles are parameterized:
#' - **`geom_subrect()`**: Defines rectangles using their four corners (`xmin`,
#'   `xmax`, `ymin`, `ymax`).
#' - **`geom_subtile()`**: Defines rectangles using the center (`x`, `y`) and
#'   dimensions (`width`, `height`).
#'
#' @param direction A string specifying the arrangement direction:
#' - `"h"`(`horizontal`): Creates a single row (one-row layout).
#' - `"v"`(`vertical`): Creates a single column (one-column layout).
#' - `NULL`: Automatically determines the layout dimensions using logic similar
#'   to [`facet_wrap()`][ggplot2::facet_wrap].
#' @param byrow A single boolean value indicates whether we should arrange the
#' divided rectangles in the row-major order.
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_segment
#' @eval rd_gg_aesthetics("geom", "subrect")
#' @examples
#' # arranges by row
#' ggplot(data.frame(value = letters[seq_len(5)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value))
#'
#' # arranges by column
#' ggplot(data.frame(value = letters[seq_len(9)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value), byrow = FALSE)
#'
#' # one-row
#' ggplot(data.frame(value = letters[seq_len(4)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value), direction = "h")
#'
#' # one-column
#' ggplot(data.frame(value = letters[seq_len(4)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value), direction = "v")
#'
#' @importFrom rlang list2
#' @export
geom_subrect <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         byrow = TRUE, direction = NULL,
                         lineend = "butt", linejoin = "mitre",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    assert_bool(byrow)
    if (!is.null(direction)) {
        direction <- check_direction(direction)
    }
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSubrect,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            byrow = byrow,
            direction = direction,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom ggplot2 ggproto ggproto_parent wrap_dims
GeomSubrect <- ggproto(
    "GeomSubrect",
    ggplot2::GeomRect,
    extra_params = c(ggplot2::GeomRect$extra_params, "byrow", "direction"),
    setup_data = function(self, data, params) {
        data <- ggproto_parent(ggplot2::GeomRect, self)$setup_data(data, params)
        indices <- vec_group_loc(data[c("xmin", "xmax", "ymin", "ymax")])
        data_list <- vec_chop(data, indices = .subset2(indices, "loc"))
        max_n_tiles <- max(list_sizes(data_list))
        if (max_n_tiles == 1L) return(data) # styler: off
        cli_inform(paste(
            "{.fn {snake_class(self)}} subdivide tile into a maximal",
            "of {max_n_tiles} rectangles"
        ))
        vec_rbind(!!!lapply(data_list, function(data) {
            n <- vec_size(data)
            if (n == 1L) return(data) # styler: off
            if (is.null(direction <- .subset2(params, "direction"))) {
                n_rows <- n_cols <- NULL
            } else if (is_horizontal(direction)) {
                n_rows <- 1L
                n_cols <- NULL
            } else {
                n_rows <- NULL
                n_cols <- 1L
            }
            dims <- wrap_dims(n, nrow = n_rows, ncol = n_cols)
            n_rows <- dims[1L]
            n_cols <- dims[2L]
            one_row <- vec_slice(data, 1L)
            width <- (one_row$xmax - one_row$xmin) / n_cols
            height <- (one_row$ymax - one_row$ymin) / n_rows

            if (.subset2(params, "byrow")) {
                # we arrange the rectangles from from left to
                # right, then from top to bottom
                data$xmin <- data$xmin +
                    vec_rep(width * (seq_len(n_cols) - 1L), n_rows)[
                        seq_len(n)
                    ]
                data$xmax <- data$xmax -
                    vec_rep(width * rev(seq_len(n_cols) - 1L), n_rows)[
                        seq_len(n)
                    ]

                data$ymin <- data$ymin +
                    vec_rep_each(height * rev(seq_len(n_rows) - 1L), n_cols)[
                        seq_len(n)
                    ]
                data$ymax <- data$ymax -
                    vec_rep_each(height * (seq_len(n_rows) - 1L), n_cols)[
                        seq_len(n)
                    ]
            } else {
                # we arrange the rectangles from top to bottom,
                # then from left to right
                data$xmin <- data$xmin +
                    vec_rep_each(width * (seq_len(n_cols) - 1L), n_rows)[
                        seq_len(n)
                    ]
                data$xmax <- data$xmax -
                    vec_rep_each(width * rev(seq_len(n_cols) - 1L), n_rows)[
                        seq_len(n)
                    ]

                data$ymin <- data$ymin +
                    vec_rep(height * rev(seq_len(n_rows) - 1L), n_cols)[
                        seq_len(n)
                    ]
                data$ymax <- data$ymax -
                    vec_rep(height * (seq_len(n_rows) - 1L), n_cols)[
                        seq_len(n)
                    ]
            }

            data
        }))
    }
)

#' @eval rd_gg_aesthetics("geom", "subtile")
#' @importFrom rlang list2
#' @export
#' @rdname geom_subrect
geom_subtile <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         byrow = TRUE, direction = NULL,
                         lineend = "butt", linejoin = "mitre",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
    assert_bool(byrow)
    if (!is.null(direction)) {
        direction <- check_direction(direction)
    }
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomSubtile,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list2(
            byrow = byrow,
            direction = direction,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
GeomSubtile <- ggproto(
    "GeomSubtile",
    ggplot2::GeomTile,
    extra_params = c(ggplot2::GeomRect$extra_params, "byrow", "direction"),
    setup_data = function(self, data, params) {
        data <- ggproto_parent(ggplot2::GeomTile, self)$setup_data(data, params)
        ggproto_parent(GeomSubrect, self)$setup_data(data, params)
    }
)
