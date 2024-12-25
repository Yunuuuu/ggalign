#' Connect two layout crosswise
#'
#' @description
#' `ggcross` resets the layout ordering index of a [`stack_cross()`]. This
#' allows you to add other `align_*` objects to define a new layout ordering
#' index. Any objects added after `ggcross` will use this updated layout
#' ordering index. This feature is particularly useful for creating `tanglegram`
#' visualizations. `ggcross()` is an alias of `ggcross()`.
#'
#' @inheritParams ggalign
#' @section ggplot2 specification:
#' `ggcross()` initializes a ggplot `data` and `mapping`.
#'
#' `ggcross()` always applies a default mapping for the axis of the data index
#' in the layout. This mapping is `aes(y = .data$.y)` for horizontal stack
#' layout (including left and right annotation) and `aes(x = .data$.x)` for
#' vertical stack layout (including top and bottom annotation).
#'
#' The data in the underlying `ggplot` object will contain following columns:
#'
#'  - `.panel`: The panel for the aligned axis. Refers to the `x-axis` for
#'    vertical `stack_layout()` (including top and bottom annotations), and the
#'    `y-axis` for horizontal `stack_layout()` (including left and right
#'    annotations).
#'
#'  - `.names` ([`vec_names()`][vctrs::vec_names]) and `.index`
#'    ([`vec_size()`][vctrs::vec_size()]/[`NROW()`]): Character names (if
#'    available) and the integer index of the original data.
#'
#'  - `.hand`: a factor indicates the index groups.
#'
#'  - `.x`/`.y` and `.discrete_x`/`.discrete_y`: Integer indices for `x`/`y`
#'    coordinates, and a factor of the data labels (only applicable when names
#'    exist).
#'
#' It is recommended to use `.x`/`.y` as the `x`/`y` mapping.
#'
#' @importFrom ggplot2 ggproto aes
#' @export
ggcross <- function(mapping = aes(), size = NULL,
                    no_axes = NULL, active = NULL) {
    active <- update_active(active, new_active(use = TRUE))
    cross(
        cross = CrossGg,
        plot = ggplot(mapping = mapping),
        schemes = default_schemes(th = theme_no_panel()),
        size = size, no_axes = no_axes, active = active
    )
}

#' @importFrom ggplot2 ggproto ggplot
CrossGg <- ggproto("CrossGg", Cross,
    setup_plot = function(self, plot) {
        ggadd_default(plot, mapping = switch_direction(
            self$direction, aes(y = .data$.y), aes(x = .data$.x)
        ))
    },
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        direction <- self$direction
        index <- vec_c(
            .subset2(previous_design, "index"),
            .subset2(design, "index")
        )
        data <- data_frame0(
            .panel = vec_c(
                .subset2(previous_design, "panel"),
                .subset2(design, "panel")
            ),
            .index = index,
            # ggcross() only reset ordering index, labels should be the same
            .names = .subset(.subset2(self, "labels"), index),
            .hand = if (is_horizontal(direction)) {
                factor(
                    vec_rep_each(
                        c("left", "right"),
                        c(
                            .subset2(previous_design, "nobs"),
                            .subset2(design, "nobs")
                        )
                    ),
                    c("left", "right")
                )
            } else {
                factor(
                    vec_rep_each(
                        c("top", "bottom"),
                        c(
                            .subset2(previous_design, "nobs"),
                            .subset2(design, "nobs")
                        )
                    ),
                    c("bottom", "top")
                )
            }
        )
        axis <- to_coord_axis(direction)
        coord_name <- paste0(".", axis)
        data[[coord_name]] <- vec_c(
            seq_len(.subset2(previous_design, "nobs")),
            seq_len(.subset2(design, "nobs"))
        )
        if (!is.null(.subset2(data, ".names"))) {
            data[[paste0(".discrete_", axis)]] <- reorder(
                .subset2(data, ".names"),
                .subset2(data, coord_name),
                order = FALSE
            )
        }
        plot <- gguse_data(plot, data)
        plot + switch_direction(
            direction,
            default_expansion(x = expansion()),
            default_expansion(y = expansion())
        )
    },
    summary = function(self, plot) {
        header <- ggproto_parent(AlignProto, self)$summary(plot)
        c(header, "  Reset the ordering index and Add plot")
    }
)
