#######################################################################
# ggplot2 add default scales in `compute_aesthetics` process
# then ggplot2 transform all scales
#  layout:
#   in ggplot_build
#    - `setup`:
#       - call `facet$setup_params`
#       - attach `plot_env`
#       - call `facet$setup_data`
#       - call `facet$compute_layout`
#       - call `coord$setup_layout`
#       - call `facet$map_data`
#    - `train_position`: (run twice)
#       - call `facet$init_scales`
#       - call `facet$train_scales`
#    - `setup_panel_params`
#       - call `coord$modify_scales`: we align scales here, since this step
#                                     scales have been trained
#       - call `coord$setup_panel_params`: `view_scales_from_scale()`
#    - `map_position`
#    - `setup_panel_guides`
#       - call `coord$setup_panel_guides`
#       - call `coord$train_panel_guides`
#  in ggplot_gtable
#    - `layout$render`:
#         - call `facet$draw_back`
#         - call `facet$draw_front`
#         - call `coord$draw_panel` for each panel
#         - call `facet$draw_panels`: only once
#           - call `facet$init_gtable`:
#           - call `facet$attach_axes`:
#             - call `coord$render_axis_h`:
#               - call `guide$draw`:
#             - call `coord$render_axis_v`:
#               - call `guide$draw`:
#           - call `facet$attach_strips`:

#' Set `limits`, `breaks`, `labels` for each panel
#'
#' @param x,y A list of layout parameters.
#'  - panel: panel group ordered by index.
#'  - index: the original index.
#'  - labels: the original labels ordered by index.
#'  - limits: A boolean value indicates whether should set limits.
#' @keywords internal
#' @noRd
discrete_ggalign <- function(x = NULL, y = NULL) {
    structure(list(x = x, y = y), class = "discrete_ggalign")
}

setup_limits <- function(axis, params) {
    panel <- .subset2(params, "panel")
    index <- .subset2(params, "index")

    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (axis == "y") panel <- fct_rev(panel)
    lapply(split(seq_along(index), panel), function(plot_index) {
        range(plot_index) + c(-0.5, 0.5)
    })
}

is_coord_okay <- function(x, axes) UseMethod("is_coord_okay")

#' @export
is_coord_okay.CoordCartesian <- function(x, axes) TRUE

#' @export
is_coord_okay.CoordTrans <- function(x, axes) {
    # we only allow identity trans in the axis used to align observations
    all(vapply(
        x$trans[axes],
        function(trans) identical(trans$name, "identity"), logical(1L),
        USE.NAMES = FALSE
    ))
}

#' @export
is_coord_okay.default <- function(x, axes) FALSE

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.discrete_ggalign <- function(object, plot, object_name) {
    x_params <- .subset2(object, "x")
    y_params <- .subset2(object, "y")

    # we will set limits by default
    if (!is.null(x_params) && (.subset2(x_params, "limits") %||% TRUE)) {
        xlim_list <- setup_limits("x", x_params)
        n_cycle <- length(xlim_list)
    } else {
        n_cycle <- xlim_list <- NULL
    }

    # we will set limits by default
    if (!is.null(y_params) && (.subset2(y_params, "limits") %||% TRUE)) {
        ylim_list <- setup_limits("y", y_params)
    } else {
        ylim_list <- NULL
    }
    ParentCoord <- .subset2(plot, "coordinates")

    # styler: off
    if (!is_coord_okay(ParentCoord,
                      c("x", "y")[c(!is.null(x_params), !is.null(y_params))])) {
        # styler: on
        cli_warn(c(
            sprintf(
                "Currently, {.fn %s} is not supported",
                snake_class(ParentCoord)
            ),
            i = "Will use {.fn coord_cartesian} instead"
        ))
        ParentCoord <- ggplot2::coord_cartesian()
    }

    plot$coordinates <- ggproto(
        NULL, ParentCoord,
        num_of_panels = NULL,
        panel_counter = NULL,
        setup_layout = function(self, layout, params) {
            # we always initialize the number of panels and a panel counter
            self$num_of_panels <- vec_unique_count(.subset2(layout, "PANEL"))
            self$panel_counter <- 0L
            # call the parent method
            ggproto_parent(ParentCoord, self)$setup_layout(layout, params)
        },
        # take the tricks to modify scales in place
        modify_scales = function(self, scales_x, scales_y) {
            # for each scale, we set the `breaks` and `labels`
            if (!is.null(x_params)) {
                align_discrete_scales("x", x_params, scales_x)
            }
            if (!is.null(y_params)) {
                align_discrete_scales("y", y_params, scales_y)
            }
            ggproto_parent(ParentCoord, self)$modify_scales(scales_x, scales_y)
        },
        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            # `setup_panel_params()` will utilize the `limits`
            # set limits here, in this way, each plot will have the same limits
            cur_panel <- self$panel_counter + 1L
            if (!is.null(xlim_list)) {
                xlim <- .subset2(xlim_list, recycle_whole(cur_panel, n_cycle))
                if (scale_x$is_discrete()) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    xlim <- xlim - (min(xlim) - 0.5)
                }
                self$limits$x <- xlim
            }
            if (!is.null(ylim_list)) {
                ylim <- .subset2(ylim_list, recycle_each(cur_panel, n_cycle))
                if (scale_y$is_discrete()) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    ylim <- ylim - (min(ylim) - 0.5)
                }
                self$limits$y <- ylim
            }
            self$panel_counter <- cur_panel
            ggproto_parent(ParentCoord, self)$setup_panel_params(
                scale_x = scale_x, scale_y = scale_y, params = params
            )
        }
    )
    plot
}

align_discrete_scales <- function(axis, params, scales) {
    panel <- .subset2(params, "panel")
    index <- .subset2(params, "index")
    labels <- .subset2(params, "labels")

    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (axis == "y") panel <- fct_rev(panel)
    if (is.null(labels)) {
        data_labels <- NULL
    } else {
        data_labels <- split(labels, panel)
    }
    data_index <- split(index, panel)
    plot_index <- split(seq_along(index), panel)
    default_expand <- ggplot2::expansion()
    for (i in seq_along(scales)) {
        scale <- .subset2(scales, i)
        # we always use the discrete scale to determine labels and breaks
        # https://github.com/tidyverse/ggplot2/blob/7fb4c382f9ea332844d469663a8047355a88dd7a/R/scale-.R#L927
        # setup breaks and labels --------------------
        if (is.null(data_labels) &&
            is.waive(scale$labels) &&
            is.waive(scale$breaks)) {
            # special case for data have no labels
            # By default we also remove the breaks
            scale$breaks <- NULL
            scale$labels <- NULL
        } else {
            dindex <- .subset2(data_index, i)
            pindex <- .subset2(plot_index, i)
            labels <- .subset2(data_labels, i)
            scale$breaks <- get_discrete_breaks(scale, pindex, dindex, labels)
            scale$labels <- get_discrete_labels(
                scale, scale$breaks, pindex, dindex, labels
            )
        }
        # by default we elways remove any expansion
        scale$expand <- scale$expand %|w|% default_expand

        # for continuous scale, we don't allow the trans
        if (!scale$is_discrete() && !identical(scale$trans$name, "identity")) {
            cli::cli_warn(sprintf(
                "{.arg trans} must be {.field identity} in {.code %s}",
                deparse(scale$call)
            ))
            scale$trans <- scales::as.transform("identity")
        }
    }
}

#' @importFrom rlang is_empty
get_discrete_breaks <- function(scale, pindex, dindex, labels) {
    if (scale$is_empty()) return(numeric()) # styler: off
    breaks <- scale$breaks
    if (identical(breaks, NA)) {
        cli_abort(c(
            "Invalid {.arg breaks} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }
    if (is.null(breaks)) {
        return(NULL)
    }
    if (is.waive(breaks)) {
        if (scale$is_discrete() &&
            !is.null(labels) &&
            !is.null(scale$range$range) &&
            all(scale$range$range %in% labels)) {
            ans <- labels
        } else {
            ans <- pindex
        }
    } else {
        if (is.null(labels)) {
            limits <- dindex
        } else {
            limits <- labels
        }
        if (is.function(breaks)) {
            breaks <- breaks(limits)
        }

        if (is.factor(breaks) || is.character(breaks)) {
            # we interpreted the character breaks as the names of the original
            # matrix data.
            pos <- match(
                as.character(limits),
                vec_cast(breaks, character(),
                    x_arg = "breaks", call = scale$call
                )
            )
        } else {
            # By default, we interpreted the breaks as the data index
            # If wrapped with `I()`, we interpreted it as the plot index
            if (inherits(breaks, "AsIs")) { # plot index
                pos <- match(pindex, vec_cast(breaks, integer(),
                    x_arg = "breaks", call = scale$call
                ))
            } else { # data index
                pos <- match(dindex, vec_cast(breaks, integer(),
                    x_arg = "breaks", call = scale$call
                ))
            }
        }
        index <- which(!is.na(pos))
        if (is_empty(index)) {
            return(NULL)
        }
        pos <- pos[index]
        if (scale$is_discrete() &&
            !is.null(labels) &&
            !is.null(scale$range$range) &&
            all(scale$range$range %in% labels)) {
            ans <- structure(labels[index], index = index, pos = pos)
        } else {
            ans <- structure(pindex[index], index = index, pos = pos)
        }
    }
    ans
}

#' @importFrom rlang is_empty
get_discrete_labels <- function(scale, breaks, pindex, dindex, labels) {
    scale_labels <- scale$labels
    if (is_empty(breaks) || is.null(scale_labels)) { # if no breaks, no labels
        return(NULL)
    }

    if (identical(scale_labels, NA)) {
        cli_abort(c(
            "Invalid {.arg labels} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }

    # Need to ensure that if breaks were dropped
    if (!is.null(index <- attr(breaks, "index"))) {
        dindex <- dindex[index]
        labels <- labels[index]
    }

    # if layout have no names, use the data index directly
    # re-defined the breaks, the plot use the coordinates index
    # we interpreted user input as the data index
    if (is.null(labels)) {
        user_breaks <- dindex
    } else {
        user_breaks <- labels
    }
    if (is.waive(scale_labels)) { # By default, use the breaks
        user_breaks
    } else if (is.function(scale_labels)) {
        scale_labels(user_breaks)
    } else if (!is.null(names(scale_labels))) {
        # If labels have names, use them to match with breaks
        map <- match(as.character(user_breaks), names(scale_labels))
        user_breaks[map] <- scale_labels[!is.na(map)]
        user_breaks
    } else {
        # Need to ensure that if breaks were dropped, corresponding labels
        # are too
        if (is.null(pos <- attr(breaks, "pos"))) {
            if (inherits(scale_labels, "AsIs")) { # already in the plot index
                # we sort the `pos` so labels ordering won't be changed
                scale_labels <- scale_labels[pindex]
            } else { # in the data index
                scale_labels <- scale_labels[dindex]
            }
        } else {
            if (inherits(scale_labels, "AsIs")) { # already in the plot index
                # we sort the `pos` so labels ordering won't be changed
                scale_labels <- scale_labels[sort(pos)]
            } else { # in the data index
                scale_labels <- scale_labels[pos]
            }
        }
        scale_labels
    }
}
