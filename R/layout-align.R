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
#' @keywords internal
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

############################################################
melt_discrete_design <- function(old, new, old_name, new_name,
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
    } else if (!is.null(old_panel) && !(new_panel %nest% old_panel)) {
        cli_abort(sprintf(
            "%s disrupt the previously established panel groups of %s",
            new_name, old_name
        ), call = call)
    } else {
        panel <- new_panel
    }

    # check index
    old_index <- .subset2(old, "index")
    new_index <- .subset2(new, "index")
    if (is.null(new_index)) {
        index <- old_index
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

setup_discrete_limits <- function(axis, design, n_panels) {
    panel <- .subset2(design, "panel")
    index <- .subset2(design, "index")
    if (n_panels == 1L) {
        list(range(index) + c(-0.5, 0.5))
    } else {
        # For y-axis, ggplot arrange panel from top to bottom,
        # we always choose to reverse the panel order
        if (axis == "y") panel <- fct_rev(panel)
        lapply(split(seq_along(index), panel), function(plot_index) {
            range(plot_index) + c(-0.5, 0.5)
        })
    }
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_design <- function(object, plot, object_name, ...) {
    x_design <- .subset2(object, "x")
    y_design <- .subset2(object, "y")
    if (is.null(x_design) && is.null(y_design)) {
        return(plot)
    }
    ParentCoord <- plot$coordinates
    plot$coordinates <- ggproto(
        NULL, ParentCoord,
        num_of_panels = NULL,
        panel_counter = NULL,
        n_row_panels = NULL, # should be the number of panels in y
        n_column_panels = NULL, # should be the number of panels in x
        setup_layout = function(self, layout, params) {
            # we always initialize the number of panels and a panel counter
            self$num_of_panels <- vec_unique_count(.subset2(layout, "PANEL"))
            self$panel_counter <- 0L
            self$n_column_panels <- vec_unique_count(.subset2(layout, "COL"))
            self$n_row_panels <- vec_unique_count(.subset2(layout, "ROW"))
            if (.subset2(object, "xlim") && !is.null(x_design)) {
                if (is_discrete_design(x_design)) {
                    self$xlim_list <- setup_discrete_limits(
                        "x", x_design, self$n_column_panels
                    )
                } else {
                    self$xlim_list <- x_design
                }
            }
            if (.subset2(object, "ylim") && !is.null(y_design)) {
                if (is_discrete_design(y_design)) {
                    self$ylim_list <- setup_discrete_limits(
                        "y", y_design, self$n_row_panels
                    )
                } else {
                    self$ylim_list <- y_design
                }
            }
            # call the parent method
            ggproto_parent(ParentCoord, self)$setup_layout(layout, params)
        },
        # take the tricks to modify scales in place
        modify_scales = function(self, scales_x, scales_y) {
            # for each scale, we set the `breaks` and `labels`
            if (is_discrete_design(x_design)) {
                align_discrete_scales(
                    "x", scales_x, x_design,
                    labels = .subset2(object, "xlabels"),
                    n_panels = self$n_column_panels,
                    circle_layout = inherits(ParentCoord, "CoordRadial")
                )
            }
            if (is_discrete_design(y_design)) {
                align_discrete_scales(
                    "y", scales_y, y_design,
                    labels = .subset2(object, "ylabels"),
                    n_panels = self$n_row_panels,
                    circle_layout = inherits(ParentCoord, "CoordRadial")
                )
            }
            ggproto_parent(ParentCoord, self)$modify_scales(scales_x, scales_y)
        },
        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            # `setup_panel_params()` will utilize the `limits`
            # set limits here to ensure each plot will have the same limits
            cur_panel <- self$panel_counter + 1L
            if (!is.null(self$xlim_list)) {
                xlim <- .subset2(
                    self$xlim_list,
                    recycle_whole(cur_panel, self$n_column_panels)
                )
                if (is_discrete_design(x_design) && scale_x$is_discrete() &&
                    !is.null(scale_x$range$range)) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    xlim <- xlim - (min(xlim) - 0.5)
                }
                if (inherits(self, "CoordRadial")) {
                    self$limits$theta <- xlim
                } else {
                    self$limits$x <- xlim
                }
            }
            if (!is.null(self$ylim_list)) {
                ylim <- .subset2(
                    self$ylim_list,
                    recycle_each(cur_panel, self$n_column_panels)
                )
                if (is_discrete_design(y_design) && scale_y$is_discrete() &&
                    !is.null(scale_y$range$range)) {
                    # for discrete scale, the limits starts from zero in each
                    # panel
                    ylim <- ylim - (min(ylim) - 0.5)
                }
                if (inherits(self, "CoordRadial")) {
                    self$limits$r <- ylim
                } else {
                    self$limits$y <- ylim
                }
            }
            self$panel_counter <- cur_panel
            ggproto_parent(ParentCoord, self)$setup_panel_params(
                scale_x = scale_x, scale_y = scale_y, params = params
            )
        }
    )
    plot
}

align_discrete_scales <- function(axis, scales, design, labels, n_panels,
                                  circle_layout) {
    panel <- .subset2(design, "panel")
    index <- .subset2(design, "index")

    if (n_panels == 1L) {
        panel <- factor(vec_rep(1L, length(index)))
    } else {
        # For y-axis, ggplot arrange panel from top to bottom,
        # we always choose to reverse the panel order
        if (axis == "y") panel <- fct_rev(panel)
    }
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
        # we don't allow the set of expansion for discrete variables
        # otherwise, ggmark and `cross_mark` won't work properly
        if (!circle_layout) scale$expand <- default_expand

        # for continuous scale, we don't allow the trans
        # if (!scale$is_discrete() && !identical(scale$trans$name, "identity")) {
        #     cli_warn(sprintf(
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

######################################################
# this will remove the old coordinate,
# so always run firstly
gguse_linear_coord <- function(plot, layout_name) {
    coord <- plot$coordinates
    if (!inherits(coord, "CoordTrans") && !coord$is_linear()) {
        cli_warn(c(
            sprintf(
                "{.fn %s} is not supported in %s",
                snake_class(coord), layout_name
            ),
            i = "Will use {.fn coord_cartesian} instead"
        ))
        plot$coordinates <- ggplot2::coord_cartesian()
    }
    plot
}

gguse_circle_coord <- function(plot, coord, ..., layout_name) {
    if (inherits(plot_coord <- plot$coordinates, "CoordRadial")) {
        out <- ggproto(
            NULL, plot_coord,
            theta = coord$theta,
            r = coord$r,
            arc = coord$arc,
            direction = coord$direction,
            r_axis_inside = coord$r_axis_inside,
            expand = coord$expand,
            ...,
            setup_panel_params = circle_panel_params
        )
    } else {
        if (!isTRUE(plot$coordinates$default)) {
            cli_warn(c(
                sprintf(
                    "{.fn %s} is not supported in %s",
                    snake_class(plot_coord), layout_name
                ),
                i = sprintf("Will use {.fn %s} instead", snake_class(coord))
            ))
        }
        if (!inherits(coord, "CoordCircle")) {
            out <- ggproto(NULL, coord, ...,
                setup_panel_params = circle_panel_params
            )
        } else {
            out <- ggproto(NULL, coord, ...)
        }
    }
    out
}

######################################################
#' @importFrom ggplot2 ggproto
ggfacet_modify <- function(plot, ...) {
    ParentFacet <- plot$facet
    plot$facet <- ggproto(NULL, ParentFacet, ...)
    plot
}

gguse_facet <- function(plot, facet) {
    plot$facet <- facet
    plot
}

ggmelt_facet <- function(plot, facet, ...) {
    gguse_facet(plot, melt_facet(facet, plot$facet, ...))
}

#' @param use A template facet object which will be used.
#' @param facet User provided facet object.
#' @noRd
melt_facet <- function(use, facet, ...) UseMethod("melt_facet")

#' @export
melt_facet.NULL <- function(use, facet, ...) {
    facet
}

#' @importFrom ggplot2 ggproto
#' @export
melt_facet.FacetGrid <- function(use, facet, ...,
                                 free_row = FALSE,
                                 free_column = FALSE) {
    if (inherits(facet, "FacetGrid")) {
        # re-dispatch parameters
        params <- facet$params
        if (length(use$params$rows) || !free_row) {
            params$rows <- use$params$rows
        }
        if (length(use$params$cols) || !free_column) {
            params$cols <- use$params$cols
        }
        if (!free_row) { # Don't allow user change the rows
            params$free$y <- use$params$free$y
            params$space_free$y <- use$params$space_free$y
        }
        if (!free_column) { # Don't allow user change the cols
            params$free$x <- use$params$free$x
            params$space_free$x <- use$params$space_free$x
        }

        params$drop <- use$params$drop
        params$as.table <- use$params$as.table

        # if the use is free, it must be free
        ggproto(NULL, facet, params = params)
    } else {
        use
    }
}

#' @importFrom ggplot2 ggproto
#' @export
melt_facet.FacetWrap <- function(use, facet, ...) {
    if (inherits(facet, "FacetWrap")) {
        # re-dispatch parameters
        params <- facet$params

        # we always fix the grid rows and cols
        params$facets <- use$params$facets
        params$nrow <- use$params$nrow
        params$ncol <- use$params$ncol
        params$drop <- use$params$drop
        params$as.table <- use$params$as.table
        ggproto(NULL, facet, params = params)
    } else {
        use
    }
}

#' @export
melt_facet.FacetNull <- function(use, facet, ...) {
    if (inherits(facet, "FacetNull")) {
        facet
    } else {
        use
    }
}

#' @export
melt_facet.FacetStack <- function(use, facet, ...) {
    if (inherits(facet, "FacetGrid")) {
        params <- facet$params
        if (is_horizontal(.subset2(use, "direction"))) {
            # for horizontal stack, we cannot facet by rows
            if (!is.null(params$rows)) {
                cli_warn(sprintf(
                    "Cannot facet by rows in %s",
                    .subset2(use, "object_name")
                ))
                params$rows <- NULL
            }
        } else if (!is.null(params$cols)) {
            # for vertical stack, we cannot facet by cols
            cli_warn(sprintf(
                "Cannot facet by cols in %s",
                .subset2(use, "object_name")
            ))
            params$cols <- NULL
        }
        ggproto(NULL, facet, params = params)
    } else if (inherits(facet, "FacetWrap")) {
        params <- facet$params
        if (is_horizontal(.subset2(use, "direction"))) {
            # for horizontal stack, we cannot facet by rows
            if (is.null(params$nrow)) {
                params$nrow <- 1L
            } else if (params$nrow > 1L) {
                cli_warn(sprintf(
                    "Cannot wrap facet by rows in %s",
                    .subset2(use, "object_name")
                ))
                params$nrow <- 1L
            }
        } else if (!is.null(params$cols)) {
            # for vertical stack, we cannot facet by cols
            if (is.null(params$ncol)) {
                params$ncol <- 1L
            } else if (params$ncol > 1L) {
                cli_warn(sprintf(
                    "Cannot wrap facet by cols in %s",
                    .subset2(use, "object_name")
                ))
                params$ncol <- 1L
            }
        }
    } else if (inherits(facet, "FacetNull")) {
        facet
    } else {
        ggplot2::facet_null()
    }
}

facet_stack <- function(direction, object_name) {
    structure(
        list(direction = direction, object_name = object_name),
        class = "FacetStack"
    )
}

#' @export
melt_facet.FacetQuad <- function(use, facet, ...,
                                 free_row = FALSE,
                                 free_column = FALSE) {
    if (inherits(facet, "FacetGrid")) {
        if (free_row || free_column) {
            params <- facet$params
            if (!free_row && !is.null(params$rows)) {
                cli_warn(sprintf(
                    "Cannot facet by rows in %s",
                    .subset2(use, "layout_name")
                ))
                params$rows <- NULL
                # for horizontal stack, we cannot facet by rows
            }
            if (!free_column && !is.null(params$cols)) {
                cli_warn(sprintf(
                    "Cannot facet by cols in %s",
                    .subset2(use, "layout_name")
                ))
                params$cols <- NULL
            }
            ggproto(NULL, facet, params = params)
        } else {
            ggplot2::facet_null()
        }
    } else if (inherits(facet, "FacetNull")) {
        facet
    } else {
        ggplot2::facet_null()
    }
}

facet_quad <- function(layout_name) {
    structure(list(layout_name = layout_name), class = "FacetQuad")
}
