#' Connect two layout crosswise
#'
#' @description
#' `cross_gg` resets the layout ordering index of a [`cross_align()`]. This
#' allows you to add other `align_*` objects to define a new layout ordering
#' index. Any objects added after `cross_gg` will use this updated layout
#' ordering index. This feature is particularly useful for creating `tanglegram`
#' visualizations. `ggcross()` is an alias of `cross_gg()`.
#'
#' @inheritParams ggalign
#' @section ggplot2 specification:
#' `ggcross()` initializes a ggplot `data` and `mapping`.
#'
#' `ggcross()` always applies a default mapping for the axis of the data index
#' in the layout. This mapping is `aes(y = .data$y)` for horizontal stack layout
#' (including left and right annotation) and `aes(x = .data$x)` for vertical
#' stack layout (including top and bottom annotation).
#'
#' The data in the underlying `ggplot` object will contain following columns:
#'
#'  - `.panel`: the panel for the aligned axis. It means `x-axis` for vertical
#'    stack layout (including top and bottom annotation), `y-axis` for
#'    horizontal stack layout (including left and right annotation).
#'
#'  - `.index`: an integer index of the data.
#'
#'  - `.names`: a character of data labels (only applicable when names exists).
#'
#'  - `.hand`: a factor indicates the index groups.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates.
#'
#' It is recommended to use `.x`/`.y` as the `x`/`y` mapping.
#'
#' @importFrom ggplot2 ggproto aes
#' @export
cross_gg <- function(mapping = aes(), size = NULL,
                     no_axes = NULL, active = NULL) {
    active <- update_active(active, new_active(use = TRUE))
    cross(
        cross = CrossGg,
        plot = ggplot(mapping = mapping),
        schemes = default_schemes(th = theme_no_panel()),
        size = size, no_axes = no_axes, active = active
    )
}

#' @usage NULL
#' @export
#' @rdname cross_gg
ggcross <- cross_gg

#' @importFrom ggplot2 ggproto ggplot
CrossGg <- ggproto("CrossGg", Cross,
    setup_plot = function(self, plot) {
        ggadd_default(plot, mapping = switch_direction(
            self$direction, aes(y = .data$.y), aes(x = .data$.x)
        ))
    },
    build_plot = function(self, plot, coords, extra_coords, previous_coords) {
        direction <- self$direction
        index <- vec_c(
            .subset2(previous_coords, "index"),
            .subset2(coords, "index")
        )
        data <- data_frame0(
            .panel = vec_c(
                .subset2(previous_coords, "panel"),
                .subset2(coords, "panel")
            ),
            .index = index,
            .names = .subset(.subset2(self, "labels"), index),
            .hand = if (is_horizontal(direction)) {
                factor(
                    vec_rep_each(
                        c("left", "right"),
                        c(
                            .subset2(previous_coords, "nobs"),
                            .subset2(coords, "nobs")
                        )
                    ),
                    c("left", "right")
                )
            } else {
                factor(
                    vec_rep_each(
                        c("top", "bottom"),
                        c(
                            .subset2(previous_coords, "nobs"),
                            .subset2(coords, "nobs")
                        )
                    ),
                    c("bottom", "top")
                )
            }
        )
        coords$labels <- .subset(
            .subset2(self, "labels"),
            .subset2(coords, "index")
        )
        data[[switch_direction(direction, ".y", ".x")]] <- vec_c(
            seq_len(.subset2(previous_coords, "nobs")),
            seq_len(.subset2(coords, "nobs"))
        )
        if (nlevels(.subset2(coords, "panel")) > 1L) {
            default_facet <- switch_direction(
                direction,
                ggplot2::facet_grid(
                    rows = ggplot2::vars(.data$.panel),
                    scales = "free_y", space = "free",
                    drop = FALSE, as.table = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free_x", space = "free",
                    drop = FALSE, as.table = FALSE
                )
            )
        } else {
            default_facet <- ggplot2::facet_null()
        }
        plot$data <- data
        if (is_horizontal(direction)) {
            default_coord <- cartesian_coord("y")
            default_align <- discrete_ggalign(y = coords)
            default_expand <- default_expansion(x = expansion())
        } else {
            default_coord <- cartesian_coord("x")
            default_expand <- default_expansion(y = expansion())
            default_align <- discrete_ggalign(x = coords)
        }
        plot +
            align_melt_facet(default_facet, plot$facet) +
            default_coord + default_expand + default_align
    }
)
