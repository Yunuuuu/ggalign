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
#' It is recommended to use `.x`/`.y`, or `.discrete_x`/`.discrete_y` as the
#' `x`/`y` mapping.
#'
#' @importFrom ggplot2 ggplot aes
#' @export
ggcross <- function(mapping = aes(), size = NULL, active = NULL,
                    no_axes = deprecated()) {
    assert_active(active)
    active <- active_update(active(use = TRUE), active)
    if (lifecycle::is_present(no_axes)) {
        lifecycle::deprecate_stop(
            "1.0.3",
            "ggcross(no_axes = )",
            details = "Please add `theme()` to the ggplot instead"
        )
    }
    cross(
        CrossGg,
        plot = ggplot(mapping = mapping),
        schemes = default_schemes(th = theme_no_strip()),
        size = size, active = active
    )
}

#' @importFrom ggplot2 ggproto
CrossGg <- ggproto("CrossGg",
    CraftCross,
    interact_layout = function(self, layout) {
        if (!is_cross_layout(layout)) {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(self), self$layout_name
                ),
                i = sprintf(
                    "%s can only be used in {.fn stack_cross}",
                    object_name(self)
                )
            ))
        }

        # udpate cross_points
        layout@cross_points <- c(layout@cross_points, length(layout@box_list))

        # update old domain list
        layout@odomain <- c(layout@odomain, list(layout@domain))

        # we keep the names from the layout data for usage
        self$labels <- vec_names(layout@data)
        layout
    },
    setup_domain = function(self, domain) {
        prop(domain, "index") <- NULL # always reset the index
        domain
    },
    setup_plot = function(self, plot) {
        ggadd_default(plot, mapping = switch_direction(
            self$direction, aes(y = .data$.y), aes(x = .data$.x)
        ))
    },
    #' @importFrom stats reorder
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        if (is.na(prop(domain, "nobs"))) {
            cli_abort(sprintf(
                "you must initialize %s before drawing %s",
                self$layout_name, object_name(self)
            ), call = self$call)
        }
        direction <- self$direction
        index <- vec_c(
            prop(previous_domain, "index"),
            prop(domain, "index")
        )
        data <- data_frame0(
            .panel = vec_c(
                prop(previous_domain, "panel"),
                prop(domain, "panel")
            ),
            .index = index,
            # ggcross() only reset ordering index, labels should be the same
            .names = .subset(self$labels, index),
            .hand = if (is_horizontal(direction)) {
                factor(
                    vec_rep_each(
                        c("left", "right"),
                        c(
                            prop(previous_domain, "nobs"),
                            prop(domain, "nobs")
                        )
                    ),
                    c("left", "right")
                )
            } else {
                factor(
                    vec_rep_each(
                        c("top", "bottom"),
                        c(
                            prop(previous_domain, "nobs"),
                            prop(domain, "nobs")
                        )
                    ),
                    c("bottom", "top")
                )
            }
        )
        axis <- to_coord_axis(direction)
        coord_name <- paste0(".", axis)
        data[[coord_name]] <- vec_c(
            seq_len(prop(previous_domain, "nobs")),
            seq_len(prop(domain, "nobs"))
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
    finish_plot = function(self, plot, schemes, theme) {
        direction <- self$direction
        # remove axis titles, text, ticks used for alignment
        if (isTRUE(self$no_axes)) {
            schemes <- scheme_update(
                schemes,
                theme_no_axes(switch_direction(direction, "y", "x"))
            )
        }
        plot <- plot_add_scheme(plot, schemes)
        if (is_horizontal(direction)) {
            theme <- theme(
                panel.spacing.y = calc_element("panel.spacing.y", theme)
            )
        } else {
            theme <- theme(
                panel.spacing.x = calc_element("panel.spacing.x", theme)
            )
        }
        plot <- plot + theme
        ggremove_margin(plot, direction) + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(Craftsman, self)$summary(plot)
        c(header, "  Reset the ordering index and Add plot")
    }
)
