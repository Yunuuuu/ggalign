is_craftsman <- function(x) inherits(x, "Craftsman")

#' Craftsman Object for Layout Management
#'
#' The `Craftsman` is a virtual object used internally to manage layout-specific
#' behavior during the `ggalign` plot composition process. It defines how a
#' layout interacts with the domain data, sets up facets and coordinates, and
#' aligns scales or axis labels accordingly.
#'
#' This object is used by layout constructors and should not be modified
#' directly.
#'
#' @section Methods:
#' The following key methods are implemented:
#' - `setup_stack_facet()` / `setup_stack_coord()` / `finish_stack_plot()`
#' - `setup_circle_facet()` / `setup_circle_coord()` / `finish_circle_plot()`
#' - `build_plot()` / `finish_plot()` – finalize plot decorations
#' - `summary()` – print class info
#'
#' @importFrom ggplot2 ggproto
#' @keywords internal
#' @name Craftsman
NULL

Craftsman <- ggproto("Craftsman",
    call = NULL,

    # State used internally after layout insertion
    in_linear = NULL,
    layout_name = NULL,
    direction = NULL,
    position = NULL,    # e.g., used by stack_layout() in quad_layout()
    labels = NULL,      # for discrete domain, there should be labels

    # we always prevent user from modifying the object in `$build_plot()` and
    # `$finish_plot()` methods
    # Prevent user modification
    locked = TRUE,
    lock = function(self) {
        assign("locked", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("locked", value = FALSE, envir = self)
    },

    ############################################################
    # when added to the `Layout` object, will call following methods

    # Typically used to define number of observations using layout data
    interact_layout = function(self, layout) layout,

    # Define panel/index info from domain
    setup_domain = function(self, domain) domain,

    # Initialize the plot when adding to the layout
    init_plot = function(self, plot) plot,

    # ========== Stack Layout ==========
    # Setup facet for stack layout
    # It's important we also set `drop = FALSE`, otherwise, plot is hard to
    # align, by convention, we always set `as.table = FALSE` to follow usual
    # ggplot2 coordinate direction
    setup_stack_facet = function(self, plot, domain, ...) {
        if (is_discrete_domain(domain)) {
            if (nlevels(prop(domain, "panel")) > 1L) {
                if (is_horizontal(self$direction)) {
                    facet <- ggplot2::facet_grid(
                        rows = ggplot2::vars(.data$.panel),
                        scales = "free_y", space = "free",
                        drop = FALSE, as.table = FALSE
                    )
                    free_row <- FALSE
                    free_column <- TRUE
                } else {
                    facet <- ggplot2::facet_grid(
                        cols = ggplot2::vars(.data$.panel),
                        scales = "free_x", space = "free",
                        drop = FALSE, as.table = FALSE
                    )
                    free_row <- TRUE
                    free_column <- FALSE
                }
            } else {
                facet <- facet_stack(self$direction, self$layout_name)
            }
            plot <- ggmelt_facet(plot, facet,
                free_row = free_row, free_column = free_column
            )
        }
        plot
    },

    # Setup coordinate system for stack layout
    setup_stack_coord = function(self, plot, coord, ...) {
        # we by default only allow linear coordinate
        # don't need to make a copy, since stack layout will always create a new
        # one
        gguse_linear_coord(plot, coord, self$layout_name)
    },

    # Final alignment for stack layout
    finish_stack_plot = function(self, plot, domain, ...) {
        if (is_horizontal(self$direction)) {
            plot + layout_align(y = domain, ylabels = self$labels)
        } else {
            plot + layout_align(x = domain, xlabels = self$labels)
        }
    },

    # ========== Circle Layout ==========
    # Setup facet for circle layout
    # It's important we also set `drop = FALSE`, otherwise, plot is hard to
    # align
    setup_circle_facet = function(self, plot, domain, sector_spacing, ...) {
        ParentFacet <- plot$facet
        if (is_discrete_domain(domain)) {
            if (nlevels(prop(domain, "panel")) > 1L) {
                plot <- plot + facet_sector(
                    ggplot2::vars(.data$.panel),
                    sector_spacing = sector_spacing,
                    drop = FALSE
                )
            } else if (!inherits(ParentFacet, "FacetNull")) {
                plot$facet <- ggplot2::facet_null()
            }
        } else {
            if (inherits(ParentFacet, "FacetSector")) {
                params <- ParentFacet$params
                params$drop <- FALSE
                plot$facet <- ggproto(NULL, ParentFacet,
                    sector_spacing = sector_spacing,
                    params = params
                )
            } else if (!inherits(ParentFacet, "FacetNull")) {
                plot$facet <- ggplot2::facet_null()
            }
        }
        plot
    },

    # Setup coordinate system for circle layout
    setup_circle_coord = function(self, plot, coord, ...) {
        # transform ggplot2 coordiantes into
        if (inherits(plot_coord <- plot$coordinates, "CoordRadial")) {
            plot$coordinates <- ggproto(
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
                        snake_class(plot_coord), self$layout_name
                    ),
                    i = sprintf("Will use {.fn %s} instead", snake_class(coord))
                ))
            }
            plot$coordinates <- ggproto(NULL, coord, ...)
        }
        plot
    },

    # Final alignment for circle layout
    finish_circle_plot = function(self, plot, domain, ...) {
        plot + layout_align(x = domain, xlabels = self$labels)
    },

    # ========== General Build/Finish Steps ==========
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_scheme(plot, schemes)
        ggremove_margin(plot, self$direction) + theme_recycle()
    },

    # ========== Utilities ==========
    summary = function(self, plot) {
        cls <- class(self)
        cls <- cls[seq_len(which(cls == "Craftsman"))]
        sprintf("<Class: %s>", paste(cls, collapse = " "))
    }
)

# Used to lock the `Craftsman` object
#' @export
`$<-.Craftsman` <- function(x, name, value) {
    if (x$locked) {
        cli_abort(
            c(
                sprintf("Cannot modify %s", object_name(x)),
                i = sprintf("%s is locked", object_name(x))
            ),
            call = x$call
        )
    }
    NextMethod()
}

#' @export
`[[<-.Craftsman` <- `$<-.Craftsman`
