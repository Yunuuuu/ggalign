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
layout_align <- function(x = NULL, y = NULL,
                         xlabels = NULL, ylabels = NULL,
                         xlim = TRUE, ylim = TRUE) {
    if (!is.null(xlabels) && is_discrete_domain(x)) {
        xlabels <- .subset(xlabels, prop(x, "index"))
    }
    if (!is.null(ylabels) && is_discrete_domain(y)) {
        ylabels <- .subset(ylabels, prop(y, "index"))
    }
    structure(
        list(
            x = x, y = y,
            xlabels = xlabels, ylabels = ylabels,
            xlim = xlim, ylim = ylim
        ),
        class = "layout_align"
    )
}

setup_discrete_limits <- function(axis, domain, n_panels) {
    panel <- prop(domain, "panel")
    index <- prop(domain, "index")
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
ggplot_add.layout_align <- function(object, plot, object_name, ...) {
    x_domain <- .subset2(object, "x")
    y_domain <- .subset2(object, "y")
    if (is.null(x_domain) && is.null(y_domain)) {
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
            if (.subset2(object, "xlim") && !is.null(x_domain)) {
                if (is_discrete_domain(x_domain)) {
                    self$xlim_list <- setup_discrete_limits(
                        "x", x_domain, self$n_column_panels
                    )
                } else if (is_continuous_domain(x_domain) &&
                    length(limits <- prop(x_domain, "limits"))) {
                    self$xlim_list <- limits
                }
            }
            if (.subset2(object, "ylim") && !is.null(y_domain)) {
                if (is_discrete_domain(y_domain)) {
                    self$ylim_list <- setup_discrete_limits(
                        "y", y_domain, self$n_row_panels
                    )
                } else if (is_continuous_domain(y_domain) &&
                    length(limits <- prop(y_domain, "limits"))) {
                    self$ylim_list <- limits
                }
            }
            # call the parent method
            ggproto_parent(ParentCoord, self)$setup_layout(layout, params)
        },
        # take the tricks to modify scales in place
        modify_scales = function(self, scales_x, scales_y) {
            if (inherits(ParentCoord, "CoordRadial")) {
                default_expand <- ggplot2::expansion(add = 0.6)
            } else {
                default_expand <- ggplot2::expansion()
            }
            # for each scale, we set the `breaks` and `labels`
            if (is_discrete_domain(x_domain)) {
                align_discrete_scales(
                    "x", scales_x, x_domain,
                    labels = .subset2(object, "xlabels"),
                    n_panels = self$n_column_panels,
                    expand = default_expand
                )
            }
            if (is_discrete_domain(y_domain)) {
                align_discrete_scales(
                    "y", scales_y, y_domain,
                    labels = .subset2(object, "ylabels"),
                    n_panels = self$n_row_panels,
                    expand = default_expand
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
                if (is_discrete_domain(x_domain) &&
                    scale_x$is_discrete() &&
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
                if (is_discrete_domain(y_domain) && scale_y$is_discrete() &&
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

align_discrete_scales <- function(axis, scales, domain, labels, n_panels,
                                  expand) {
    panel <- prop(domain, "panel")
    index <- prop(domain, "index")

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
        scale_expand <- ggplot2::expansion()
        if (i == 1L) {
            scale_expand[1:2] <- (scale$expand %|w|% expand)[1:2]
            scale_expand[3:4] <- 0
        }

        if (i == length(scales)) {
            scale_expand[1:2] <- 0
            scale_expand[3:4] <- (scale$expand %|w|% expand)[3:4]
        }
        scale$expand <- scale_expand

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
gguse_linear_coord <- function(plot, coord, layout_name) {
    if (!inherits(plot$coordinates, "CoordTrans") &&
        !plot$coordinates$is_linear()) {
        cli_warn(c(
            sprintf(
                "{.fn %s} is not supported in %s",
                snake_class(plot$coordinates), layout_name
            ),
            i = sprintf("Will use {.fn %s} instead", snake_class(coord))
        ))
        plot$coordinates <- coord
    }
    plot
}

######################################################
# nocov start
gguse_facet <- function(plot, facet) {
    plot$facet <- facet
    plot
}

align_stack_discrete_facet <- function(direction, plot, domain, layout_name) {
    if (nlevels(prop(domain, "panel")) > 1L) {
        facets <- ggplot2::vars(.data$.panel)
    } else {
        facets <- NULL
    }
    gguse_facet(plot, align_stack_facet(
        direction, plot$facet, facets, "grid",
        layout_name
    ))
}

align_stack_continuous_facet <- function(direction, plot, domain, layout_name) {
    if (is.null(domain) || is.null(prop(domain, "facet_lvls"))) {
        return(plot)
    }
    facets <- ggplot2::vars(.data[[!!.subset(names(plot$data), 1L)]])
    gguse_facet(plot, align_stack_facet(
        direction, plot$facet, facets, "grid", layout_name
    ))
}

align_stack_facet <- function(direction, user, facets, type, layout_name) {
    if (inherits(user, "FacetGrid")) {
        params <- user$params
        if (is_horizontal(direction)) {
            # In a horizontal stack, faceting by rows is not allowed.
            # Replace existing rows with compacted facets instead.
            # for horizontal stack, we cannot facet by rows
            if (length(params$rows)) {
                cli_warn(sprintf("Cannot facet by rows in %s", layout_name))
            }
            params["rows"] <- list(compact_facets(facets))
        } else {
            # In a vertical stack, faceting by columns is not allowed.
            # Replace existing columns with compacted facets instead.
            if (length(params$cols)) {
                cli_warn(sprintf("Cannot facet by cols in %s", layout_name))
            }
            params["cols"] <- list(compact_facets(facets))
        }
        params$drop <- FALSE
        params$as.table <- FALSE
        ggproto(NULL, user, params = params)
    } else if (inherits(user, "FacetWrap")) {
        params <- user$params
        if (is_horizontal(direction)) {
            # In a horizontal stack:
            # - Disable wrapping by rows
            # - Allow only a single column (ncol = 1)
            if (!is.null(params$nrow)) {
                cli_warn(sprintf(
                    "Cannot wrap facet by rows in %s", layout_name
                ))
                params["nrow"] <- list(NULL)
            }
            if (is.null(params$ncol)) {
                params$ncol <- 1L
            } else if (params$ncol != 1L) {
                cli_warn(sprintf(
                    "Cannot wrap facet by multiple cols in %s", layout_name
                ))
                params$ncol <- 1L
            }
        } else {
            # In a vertical stack:
            # - Disable wrapping by columns
            # - Allow only a single row (nrow = 1)
            if (!is.null(params$cols)) {
                # for vertical stack, we cannot facet by cols
                cli_warn(sprintf(
                    "Cannot wrap facet by cols in %s", layout_name
                ))
                params["ncol"] <- list(NULL)
            }
            if (is.null(params$nrow)) {
                params$nrow <- 1L
            } else if (params$nrow != 1L) {
                cli_warn(sprintf(
                    "Cannot wrap facet by multiple rows in %s", layout_name
                ))
                params$nrow <- 1L
            }
        }
        params["facets"] <- list(compact_facets(facets))
        params$drop <- FALSE
        params$as.table <- FALSE
        ggproto(NULL, user, params = params)
    } else if (is.null(facets)) { # the default facets
        # No facets, we by default use `facet_null()`.
        if (inherits(user, "FacetNull")) {
            user
        } else {
            ggplot2::facet_null()
        }
    } else if (type == "grid") { # the default facets
        switch_direction(
            direction,
            ggplot2::facet_grid(
                rows = facets,
                scales = "free_y", space = "free",
                drop = FALSE, as.table = FALSE
            ),
            ggplot2::facet_grid(
                cols = facets,
                scales = "free_x", space = "free",
                drop = FALSE, as.table = FALSE
            )
        )
    } else if (type == "wrap") {
        switch_direction(
            direction,
            ggplot2::facet_wrap(
                facets = facets,
                ncol = 1L, drop = FALSE, as.table = FALSE
            ),
            ggplot2::facet_wrap(
                facets = facets,
                nrow = 1L, drop = FALSE, as.table = FALSE
            )
        )
    } else {
        ggplot2::facet_null()
    }
}

align_circle_discrete_facet <- function(plot, domain, sector_spacing,
                                        layout_name) {
    if (nlevels(prop(domain, "panel")) > 1L) {
        facets <- ggplot2::vars(.data$.panel)
    } else {
        facets <- NULL
    }
    gguse_facet(plot, align_circle_facet(
        plot$facet, facets, sector_spacing, layout_name
    ))
}

align_circle_continuous_facet <- function(plot, domain, sector_spacing,
                                          layout_name) {
    if (is.null(domain) || is.null(prop(domain, "facet_lvls"))) {
        return(plot)
    }
    facets <- ggplot2::vars(.data[[!!.subset(names(plot$data), 1L)]])
    gguse_facet(plot, align_circle_facet(
        plot$facet, facets, sector_spacing, layout_name
    ))
}

align_circle_facet <- function(user, facets, sector_spacing, layout_name) {
    if (inherits(user, "FacetSector")) {
        params <- user$params
        params$drop <- FALSE
        params["facets"] <- list(compact_facets(facets))
        ggproto(NULL, user,
            sector_spacing = sector_spacing,
            params = params
        )
    } else if (is.null(facets)) { # No facet
        if (inherits(user, "FacetNull")) {
            user
        } else {
            ggplot2::facet_null()
        }
    } else {
        facet_sector(facets, sector_spacing, drop = FALSE)
    }
}

align_quad_facet <- function(plot, row_domain, column_domain, layout_name) {
    # ------------------------------------------------------------
    # Step 1: Determine row facet and whether row scaling is free
    # ------------------------------------------------------------
    if (is_discrete_domain(row_domain)) {
        # Discrete: use .panel_y if multiple levels exist
        if (nlevels(prop(row_domain, "panel")) > 1L) {
            row_facet <- ggplot2::vars(.data$.panel_y)
        } else {
            row_facet <- NULL
        }
        free_row <- FALSE
    } else {
        # Continuous: allow free rows if domain or facet is missing
        if (is.null(row_domain) || is.null(prop(row_domain, "facet_lvls"))) {
            row_facet <- NULL
            free_row <- TRUE
        } else {
            row_facet <- ggplot2::vars(.data[[!!.subset(names(plot$data), 1L)]])
            free_row <- FALSE
        }
    }

    # ------------------------------------------------------------
    # Step 2: Determine column facet and whether column scaling is free
    # ------------------------------------------------------------
    if (is_discrete_domain(column_domain)) {
        # Discrete: use .panel_x if multiple levels exist
        if (nlevels(prop(column_domain, "panel")) > 1L) {
            column_facet <- ggplot2::vars(.data$.panel_x)
        } else {
            column_facet <- NULL
        }
        free_column <- FALSE
    } else {
        # Continuous: allow free columns if domain or facet is missing
        if (is.null(column_domain) ||
            is.null(prop(column_domain, "facet_lvls"))) {
            column_facet <- NULL
            free_column <- TRUE
        } else {
            column_facet <- ggplot2::vars(
                .data[[!!.subset(names(plot$data), 1L)]]
            )
            free_column <- FALSE
        }
    }

    # ------------------------------------------------------------
    # Step 3: Merge with user-specified facet if using facet_grid
    # ------------------------------------------------------------
    user <- plot$facet
    if (inherits(user, "FacetGrid")) {
        if (!free_row || !free_column) {
            params <- user$params
            if (free_row) {
                row_facet <- params$rows
            } else {
                if (length(params$rows)) {
                    cli_warn(sprintf("Cannot facet by rows in %s", layout_name))
                }
                row_facet <- compact_facets(row_facet)
                # Don't allow user change the rows
                params$free$y <- TRUE
                params$space_free$y <- TRUE
            }

            if (free_column) {
                column_facet <- params$cols
            } else {
                if (length(params$cols)) {
                    cli_warn(sprintf("Cannot facet by cols in %s", layout_name))
                }
                column_facet <- compact_facets(column_facet)
                # Don't allow user change the cols
                params$free$x <- TRUE
                params$space_free$x <- TRUE
            }
            params$drop <- FALSE
            params$as.table <- FALSE
            params$rows <- row_facet
            params$cols <- column_facet
            facet <- ggproto(NULL, user, params = params)
        } else {
            facet <- user
        }
    } else {
        # ------------------------------------------------------------
        # Step 4: Build a new facet_grid if no user facet or non-grid facet
        # ------------------------------------------------------------
        # Build appropriate facet_grid depending on available facets
        if (free_row && free_column) {
            facet <- user
        } else if (!is.null(row_facet) && !is.null(column_facet)) {
            facet <- ggplot2::facet_grid(
                rows = row_facet,
                cols = column_facet,
                scales = "free", space = "free",
                drop = FALSE, as.table = FALSE
            )
        } else if (!is.null(row_facet)) {
            facet <- ggplot2::facet_grid(
                rows = row_facet,
                scales = "free_y", space = "free",
                drop = FALSE, as.table = FALSE
            )
        } else if (!is.null(column_facet)) {
            facet <- ggplot2::facet_grid(
                cols = column_facet,
                scales = "free_x", space = "free",
                drop = FALSE, as.table = FALSE
            )
        } else if (inherits(user, "FacetNull")) {
            facet <- user
        } else {
            facet <- ggplot2::facet_null()
        }
    }
    gguse_facet(plot, facet)
}
# nocov end
