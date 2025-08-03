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
#' @param byrow A single boolean value indicates whether we should arrange the
#' divided rectangles in the row-major order.
#' @param nrow,ncol A single positive integer specifying the number of rows or
#' columns in the layout of the subdivided cell. By default, the layout
#' dimensions are determined automatically using logic similar to
#' [`facet_wrap()`][ggplot2::facet_wrap].
#' @param direction `r lifecycle::badge("deprecated")` A string specifying the
#' arrangement direction:
#' - `"h"`(`horizontal`): Creates a single row (one-row layout).
#' - `"v"`(`vertical`): Creates a single column (one-column layout).
#' @inheritParams ggplot2::geom_rect
#' @inheritParams ggplot2::geom_segment
#' @eval rd_gg_aesthetics("geom", "subrect")
#' @examples
#' # arranges by row
#' ggplot(data.frame(value = letters[seq_len(5)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value), byrow = TRUE)
#'
#' # arranges by column
#' ggplot(data.frame(value = letters[seq_len(9)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value))
#'
#' # one-row
#' ggplot(data.frame(value = letters[seq_len(4)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value), nrow = 1)
#'
#' # one-column
#' ggplot(data.frame(value = letters[seq_len(4)])) +
#'     geom_subtile(aes(x = 1, y = 1, fill = value), ncol = 1)
#'
#' @importFrom rlang list2
#' @export
geom_subrect <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,
                         byrow = FALSE, nrow = NULL, ncol = NULL,
                         lineend = "butt", linejoin = "mitre",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         direction = deprecated()) {
    assert_bool(byrow)
    assert_number_whole(nrow, min = 1, allow_null = TRUE)
    assert_number_whole(ncol, min = 1, allow_null = TRUE)
    if (lifecycle::is_present(direction)) {
        lifecycle::deprecate_warn(
            "1.0.2",
            "geom_subtile(direction = )",
            details = "Please use the `nrow`/`ncol` argument instead."
        )
        if (is_horizontal(direction)) {
            nrow <- 1L
            ncol <- NULL
        } else {
            nrow <- NULL
            ncol <- 1L
        }
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
            nrow = nrow,
            ncol = ncol,
            lineend = lineend,
            linejoin = linejoin,
            na.rm = na.rm,
            ...
        )
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent wrap_dims
GeomSubrect <- ggproto(
    "GeomSubrect",
    ggplot2::GeomRect,
    extra_params = c(ggplot2::GeomRect$extra_params, "byrow", "nrow", "ncol"),
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
        nrow <- .subset2(params, "nrow")
        ncol <- .subset2(params, "ncol")
        vec_rbind(!!!lapply(data_list, function(data) {
            n <- vec_size(data)
            if (n == 1L) return(data) # styler: off
            dims <- wrap_dims(n, nrow = nrow, ncol = ncol)
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
                         byrow = TRUE, nrow = NULL, ncol = NULL,
                         lineend = "butt", linejoin = "mitre",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE,
                         direction = deprecated()) {
    assert_bool(byrow)
    assert_number_whole(nrow, min = 1, allow_null = TRUE)
    assert_number_whole(ncol, min = 1, allow_null = TRUE)
    if (lifecycle::is_present(direction)) {
        lifecycle::deprecate_warn(
            "1.0.2",
            "geom_subtile(direction = )",
            details = "Please use the `nrow`/`ncol` argument instead."
        )
        if (is_horizontal(direction)) {
            nrow <- 1L
            ncol <- NULL
        } else {
            nrow <- NULL
            ncol <- 1L
        }
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
            nrow = nrow,
            ncol = ncol,
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
    extra_params = c(ggplot2::GeomRect$extra_params, "byrow", "nrow", "ncol"),
    setup_data = function(self, data, params) {
        data <- ggproto_parent(ggplot2::GeomTile, self)$setup_data(data, params)
        ggproto_parent(GeomSubrect, self)$setup_data(data, params)
    }
)
