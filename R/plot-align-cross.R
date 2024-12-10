#' Connect two layout crosswise
#'
#' @description
#' `cross_link` resets the layout ordering index of a [`cross_align()`]. This
#' allows you to add other `align_*` objects to define a new layout ordering
#' index. Any objects added after `cross_link` will use this updated layout
#' ordering index. This feature is particularly useful for creating `tanglegram`
#' visualizations.
#' @inheritParams ggalign
#' @section ggplot2 specification:
#' `cross_link` initializes a ggplot `data` and `mapping`.
#'
#' `cross_link()` always applies a default mapping for the axis of the data
#' index in the layout. This mapping is `aes(y = .data$y)` for horizontal stack
#' layout (including left and right annotation) and `aes(x = .data$x)` for
#' vertical stack layout (including top and bottom annotation).
#'
#' The data in the underlying `ggplot` object will contain following columns:
#'
#'  - `.panel`: the panel for the aligned axis. It means `x-axis` for vertical
#'    stack layout (including top and bottom annotation), `y-axis` for
#'    horizontal stack layout (including left and right annotation).
#'
#'  - `.index`: an integer of the data index.
#'
#'  - `.hand`: a factor indicates the index groups.
#'
#'  - `.x` or `.y`: the `x` or `y` coordinates.
#'
#' It is recommended to use `.x`/`.y` as the `x`/`y` mapping.
#'
#' @importFrom ggplot2 ggproto aes
#' @export
cross_link <- function(mapping = aes(), size = NULL,
                       no_axes = NULL, active = NULL) {
    active <- update_active(active, new_active(
        use = TRUE, order = NA_integer_, name = NA_character_
    ))
    new_align_plot(
        align = ggproto(NULL, CrossLink),
        plot = ggplot(mapping = mapping),
        size = size, no_axes = no_axes, active = active,
        class = "ggalign_cross_link"
    )
}

#' @include plot-align-.R
methods::setClass("ggalign_cross_link", contains = "ggalign_align_plot")

#' @importFrom methods is
is_cross_link <- function(x) is(x, "ggalign_cross_link")

#' @importFrom ggplot2 ggproto ggplot
CrossLink <- ggproto("CrossLink", AlignProto,
    align = function(self, direction, position, object_name,
                     layout_data, layout_coords, layout_name) {
        if (is.null(.subset2(layout_coords, "nobs"))) {
            cli_abort(sprintf(
                "layout observations for %s must be initialized before adding {.var {object_name}}",
                layout_name
            ))
        }
        # we keep the names from the layout data for usage
        self$labels <- vec_names(layout_data)
        layout_coords["index"] <- list(NULL) # reset the index
        layout_coords
    },
    setup_plot = function(self, plot, mapping, direction) {
        ggadd_default(plot, mapping = switch_direction(
            direction, aes(y = .data$.y), aes(x = .data$.x)
        ))
    },
    finish = function(layout, layout_coords) {
        # udpate cross_points
        layout@cross_points <- c(layout@cross_points, length(layout@plot_list))
        # update index
        layout@index_list <- c(
            layout@index_list,
            list(.subset2(layout_coords, "index"))
        )
        layout
    },
    build = function(self, plot, schemes, coords, extra_coords, previous_coords,
                     direction, position) {
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
                    rows = ggplot2::vars(fct_rev(.data$.panel)),
                    scales = "free_y", space = "free",
                    drop = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$.panel),
                    scales = "free_x", space = "free",
                    drop = FALSE
                )
            )
        } else {
            default_facet <- ggplot2::facet_null()
        }
        plot$data <- data
        if (is_horizontal(direction)) {
            default_coord <- coord_ggalign(y = coords)
            default_expand <- default_expansion(x = expansion())
        } else {
            default_coord <- coord_ggalign(x = coords)
            default_expand <- default_expansion(y = expansion())
        }
        plot +
            align_melt_facet(plot$facet, default_facet, direction) +
            default_coord + default_expand
    }
)
