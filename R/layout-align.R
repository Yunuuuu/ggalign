#' Set Expansion for the Layout
#'
#' @description
#' To align axes, it is important to keep the expansion consistent across all
#' plots in the layout. You can add a `layout_expand` object to the layout. For
#' the `quad_layout()` function, you must specify `x` and `y` arguments. For
#' other layouts, you can pass the expansion values using `...` directly.
#'
#' @param ... A list of range expansion constants, used to add padding around
#' the data to ensure they are placed some distance away from the axes. Use the
#' convenience function [`expansion()`][ggplot2::expansion()] to generate the
#' values.
#' @param x,y Same as `...`, but specifically for `quad_layout()`.
#'
#' @importFrom rlang list2
#' @export
layout_expand <- function(..., x = waiver(), y = waiver()) {
    if (...length() > 0L && (!is.waive(x) || !is.waive(y))) {
        cli_abort(
            "Cannot mix the usage of {.arg ...} with {.arg x}/{.arg y} argument"
        )
    }
    if (...length() > 0L) {
        ans <- list2(...)
        names(ans) <- NULL
    } else {
        ans <- list(x = x, y = y)
    }
    structure(ans, class = "ggalign_layout_expand")
}

#' Set continuous limits for the layout
#'
#' @description
#' To align continuous axes, it is important to keep the limits consistent
#' across all plots in the layout. You can set the limits by passing a function
#' directly to the `limits` or `xlim`/`ylim` argument, using `...` only.
#' Alternatively, you can add a `continuous_limits()` object to the layout. For
#' the `quad_layout()` function, you must specify `x`/`y` arguments. For other
#' layouts, you should pass the limits using `...` directly.
#'
#' @param ... A list of two numeric values, specifying the left/lower limit and
#' the right/upper limit of the scale.
#' @inheritParams layout_expand
#' @importFrom rlang list2
#' @export
continuous_limits <- function(..., x = waiver(), y = waiver()) {
    if (...length() > 0L && (!is.waive(x) || !is.waive(y))) {
        cli_abort(
            "Cannot mix the usage of {.arg ...} with {.arg x}/{.arg y} argument"
        )
    }
    if (...length() > 0L) {
        ans <- list2(...)
        names(ans) <- NULL
    } else {
        ans <- list(x = x, y = y)
    }
    structure(ans, class = c("continuous_limits", "layout_design"))
}

################################################################
is_continuous_design <- function(x) {
    is.null(x) || inherits(x, "continuous_limits")
}

is_discrete_design <- function(x) inherits(x, "discrete_design")

#' Layout can align ordinal variable or continuous variable
#'
#' @param x A `LayoutProto` object.
#' @noRd
is_layout_discrete <- function(x, ...) UseMethod("is_layout_discrete")

is_layout_continuous <- function(x, ...) UseMethod("is_layout_continuous")

################################################################
# layout params are used to align the observations
discrete_design <- function(panel = NULL, index = NULL, nobs = NULL) {
    structure(
        list(panel = panel, index = index, nobs = nobs),
        class = c("discrete_design", "layout_design")
    )
}

# Initialize the index and panel
# Reorder the panel based the ordering index and
setup_design <- function(design) {
    if (is_continuous_design(design)) return(design) # styler: off
    # if `nobs` is not initialized, it means no `Align` object exist
    # it's not necessary to initialize the `panel` and `index`
    # this is for `stack_layout` which may have no data
    if (is.null(nobs <- .subset2(design, "nobs"))) {
        return(design)
    }
    panel <- .subset2(design, "panel") %||% factor(rep_len(1L, nobs))
    index <- .subset2(design, "index") %||% reorder_index(panel)
    discrete_design(panel[index], index, nobs)
}

reorder_index <- function(panel, index = NULL) {
    index <- index %||% seq_along(panel)
    unlist(split(index, panel[index]), recursive = FALSE, use.names = FALSE)
}

############################################################
#' @keywords internal
update_design <- function(layout, ..., design, object_name) {
    UseMethod("update_design")
}

#' @importFrom methods slot slot<-
#' @export
update_design.QuadLayout <- function(layout, ..., direction, design,
                                     object_name) {
    slot(layout, direction) <- design
    if (is_horizontal(direction)) {
        if (!is.null(left <- layout@left)) {
            layout@left <- update_design(left,
                design = design, object_name = object_name
            )
        }
        if (!is.null(right <- layout@right)) {
            layout@right <- update_design(right,
                design = design, object_name = object_name,
                from_head = TRUE
            )
        }
    } else {
        if (!is.null(top <- layout@top)) {
            layout@top <- update_design(top,
                design = design, object_name = object_name
            )
        }
        if (!is.null(bottom <- layout@bottom)) {
            layout@bottom <- update_design(bottom,
                design = design, object_name = object_name,
                from_head = TRUE
            )
        }
    }
    layout
}

#' @importFrom methods slot slot<-
#' @export
update_design.StackLayout <- function(layout, ..., design, object_name) {
    layout@design <- design
    layout@plot_list <- lapply(layout@plot_list, function(plot) {
        if (is_ggalign_plot(plot)) return(plot) # styler: off
        update_design(plot,
            direction = layout@direction,
            design = design
        )
    })
    layout
}

#' @export
update_design.CircleLayout <- update_design.StackLayout

#' @importFrom methods slot slot<-
#' @export
update_design.CrossLayout <- function(layout, ..., design, object_name,
                                      from_head = FALSE) {
    if (from_head && !is_empty(layout@cross_points)) {
        layout@design["nobs"] <- list(.subset2(design, "nobs"))
        layout@design["panel"] <- list(.subset2(design, "panel"))
        layout@index_list[1L] <- list(.subset2(design, "index"))
    } else {
        layout@design <- design
    }
    n_plots <- length(plot_list <- layout@plot_list)
    if (n_plots == 0L) {
        return(layout)
    }
    if (n_breaks <- length(layout@cross_points)) {
        # we also check the panel doesn't break the original index
        if (!is.null(panel <- .subset2(design, "panel"))) {
            index_list <- layout@index_list
            for (i in seq_along(index_list)) {
                if (is.null(old <- .subset2(index_list, i))) {
                    next
                }
                new <- reorder_index(panel, old)
                # we always prevent from reordering twice.
                if (!is.null(old) && !all(old == new)) {
                    cli_abort(sprintf(
                        "%s disrupt the previously established ordering index of %s (%d)",
                        object_name, object_name(layout), i
                    ))
                }
                index_list[[i]] <- new
            }
            layout@index_list <- index_list
        }
        # extract the the first or the last `cross_points`
        if (from_head) { # update the head plots
            index <- layout@cross_points[1L]
            if (index == 0L) {
                return(layout)
            }
            index <- seq_len(index)
        } else { # update the tail plots
            # one for the `ggcross()` plot itself
            index <- layout@cross_points[n_breaks] + 1L + 1L
            if (index > n_plots) {
                return(layout)
            }
            index <- index:n_plots
        }
    } else { # if no breaks, update all plots
        index <- seq_len(n_plots)
    }

    layout@plot_list[index] <- lapply(plot_list[index], function(plot) {
        if (is_ggalign_plot(plot)) return(plot) # styler: off
        update_design(plot,
            direction = layout@direction,
            design = design
        )
    })
    layout
}

############################################################
check_discrete_design <- function(old, new, old_name, new_name,
                                  call = caller_call()) {
    old_nobs <- .subset2(old, "nobs")
    new_nobs <- .subset2(new, "nobs")
    if (is.null(new_nobs)) { # no `nobs` provided
        nobs <- old_nobs
    } else if (is.null(old_nobs)) {
        nobs <- new_nobs
    } else if (!identical(new_nobs, old_nobs)) {
        cli_abort(sprintf(
            "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
            new_name, new_nobs, old_name, old_nobs
        ), call = call)
    } else {
        nobs <- new_nobs
    }

    # check panel
    old_panel <- .subset2(old, "panel")
    new_panel <- .subset2(new, "panel")

    if (is.null(new_panel)) { # no panel provided
        panel <- old_panel
    } else if (anyNA(new_panel)) {
        cli_abort(sprintf(
            "layout panels defined by %s contain `NA`",
            new_name
        ), call = call)
    } else if (!is.atomic(new_panel)) {
        cli_abort(c(
            sprintf("invalid layout panels defined by %s", new_name),
            i = "layout panels must be an atomic vector"
        ), call = call)
    } else if (is.null(nobs) || length(new_panel) != nobs) {
        # we have defined panel, but don't define the `nobs`
        cli_abort(sprintf(
            "layout panels defined by %s (nobs: %d) is not compatible with the nobs: %d",
            new_name, length(new_panel), nobs %||% 0L
        ), call = call)
    } else if (!is.null(old_panel) && !(new_panel %nest% old_panel)) {
        cli_abort(sprintf(
            "%s disrupt the previously established panel groups of %s",
            new_name, old_name
        ), call = call)
    } else {
        panel <- new_panel
        if (!is.factor(panel)) panel <- factor(panel)
    }

    # check index
    old_index <- .subset2(old, "index")
    new_index <- .subset2(new, "index")
    if (is.null(new_index)) {
        index <- old_index
    } else if (anyNA(new_index)) {
        cli_abort(sprintf(
            "layout ordering index defined by %s contain `NA`",
            new_name
        ), call = call)
    } else if (!is.integer(new_index)) {
        cli_abort(c(
            sprintf("invalid layout ordering index defined by %s", new_name),
            i = "layout ordering index must be integer"
        ), call = call)
    } else if (is.null(nobs) || length(new_index) != nobs) {
        # we have defined index, but don't define the `nobs`
        cli_abort(sprintf(
            "layout ordering index defined by %s (nobs: %d) is not compatible with the nobs: %d",
            new_name, length(new_index), nobs %||% 0L
        ), call = call)
    } else {
        index <- new_index
    }

    # we always make the index following the panel
    if (!is.null(panel) && !is.null(index)) {
        index <- reorder_index(panel, index)
    }

    # we always prevent from reordering twice.
    if (!is.null(old_index) && !all(old_index == index)) {
        cli_abort(sprintf(
            "%s disrupt the previously established ordering index of %s",
            new_name, old_name
        ), call = call)
    }
    discrete_design(panel, index, nobs)
}

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
#' @param x,y design for the layout.
#' @keywords internal
#' @noRd
ggalign_design <- function(x = NULL, y = NULL,
                           xlabels = NULL, ylabels = NULL,
                           xlim = TRUE, ylim = TRUE) {
    structure(
        list(
            x = x, y = y,
            xlabels = xlabels, ylabels = ylabels,
            xlim = xlim, ylim = ylim
        ),
        class = "ggalign_design"
    )
}

setup_discrete_limits <- function(axis, design) {
    panel <- .subset2(design, "panel")
    index <- .subset2(design, "index")

    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (axis == "y") panel <- fct_rev(panel)
    lapply(split(seq_along(index), panel), function(plot_index) {
        range(plot_index) + c(-0.5, 0.5)
    })
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_design <- function(object, plot, object_name) {
    x_design <- .subset2(object, "x")
    y_design <- .subset2(object, "y")
    if (is.null(x_design) && is.null(y_design)) {
        return(plot)
    }
    if (.subset2(object, "xlim") && !is.null(x_design)) {
        if (is_discrete_design(x_design)) {
            xlim_list <- setup_discrete_limits("x", x_design)
        } else {
            xlim_list <- x_design
        }
    } else {
        xlim_list <- NULL
    }

    if (.subset2(object, "ylim") && !is.null(y_design)) {
        if (is_discrete_design(y_design)) {
            ylim_list <- setup_discrete_limits("y", y_design)
        } else {
            ylim_list <- y_design
        }
    } else {
        ylim_list <- NULL
    }

    ParentCoord <- .subset2(plot, "coordinates")
    plot$coordinates <- ggproto(
        NULL, ParentCoord,
        num_of_panels = NULL,
        panel_counter = NULL,
        n_cycle = NULL, # should be the number of panels in x
        setup_layout = function(self, layout, params) {
            # we always initialize the number of panels and a panel counter
            self$num_of_panels <- vec_unique_count(.subset2(layout, "PANEL"))
            self$panel_counter <- 0L
            self$n_cycle <- vec_unique_count(.subset2(layout, "COL"))
            # call the parent method
            ggproto_parent(ParentCoord, self)$setup_layout(layout, params)
        },
        # take the tricks to modify scales in place
        modify_scales = function(self, scales_x, scales_y) {
            # for each scale, we set the `breaks` and `labels`
            if (self$is_linear()) {
                if (is_discrete_design(x_design)) {
                    align_discrete_scales(
                        "x", scales_x, x_design,
                        .subset2(object, "xlabels")
                    )
                }
                if (is_discrete_design(y_design)) {
                    align_discrete_scales(
                        "y", scales_y, y_design,
                        .subset2(object, "ylabels")
                    )
                }
            }
            ggproto_parent(ParentCoord, self)$modify_scales(scales_x, scales_y)
        },
        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            # `setup_panel_params()` will utilize the `limits`
            # set limits here, in this way, each plot will have the same limits
            cur_panel <- self$panel_counter + 1L
            if (!is.null(xlim_list)) {
                xlim <- .subset2(
                    xlim_list,
                    recycle_whole(cur_panel, self$n_cycle)
                )
                if (is_discrete_design(x_design) && scale_x$is_discrete()) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    xlim <- xlim - (min(xlim) - 0.5)
                }
                self$limits$x <- xlim
            }
            if (!is.null(ylim_list)) {
                ylim <- .subset2(
                    ylim_list,
                    recycle_each(cur_panel, self$n_cycle)
                )
                if (is_discrete_design(y_design) && scale_y$is_discrete()) {
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

align_discrete_scales <- function(axis, scales, design, labels = NULL) {
    panel <- .subset2(design, "panel")
    index <- .subset2(design, "index")

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
        # if (!scale$is_discrete() && !identical(scale$trans$name, "identity")) {
        #     cli::cli_warn(sprintf(
        #         "{.arg trans} must be {.field identity} in {.code %s}",
        #         deparse(scale$call)
        #     ))
        #     scale$trans <- scales::as.transform("identity")
        # }
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
