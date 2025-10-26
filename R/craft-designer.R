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
#' - `setup_stack_facet()` / `setup_stack_coord()` / `setup_stack_plot()`
#' - `setup_circle_facet()` / `setup_circle_coord()` / `setup_circle_plot()`
#' - `build_plot()` / `finish_plot()` – finalize plot decorations
#' - `summary()` – print class info
#'
#' @importFrom ggplot2 ggproto
#' @keywords internal
#' @name Craftsman
NULL

CraftDesigner <- ggproto("ggalign::CraftDesigner",
    call = NULL,

    # State used internally after layout insertion
    in_linear = NULL,
    layout_name = NULL,
    direction = NULL,
    position = NULL, # e.g., used by stack_layout() in quad_layout()
    labels = NULL, # for discrete domain, there should be labels

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
            align_stack_discrete_facet(
                self$direction, plot, domain,
                self$layout_name
            )
        } else {
            align_stack_continuous_facet(
                self$direction, plot, domain,
                self$layout_name
            )
        }
    },

    # Setup coordinate system for stack layout
    setup_stack_coord = function(self, plot, coord, ...) {
        # we by default only allow linear coordinate
        # don't need to make a copy, since stack layout will always create a new
        # one
        gguse_linear_coord(plot, coord, self$layout_name)
    },

    # Final alignment for stack layout
    setup_stack_plot = function(self, plot, domain, ...) {
        if (is_horizontal(self$direction)) {
            plot + layout_align(y = domain, ylabels = self$labels)
        } else {
            plot + layout_align(x = domain, xlabels = self$labels)
        }
    },

    # ========== Circle Layout ==========
    # Setup facet for circle layout
    # Use `drop = FALSE` to preserve all facet levels and ensure alignment
    setup_circle_facet = function(self, plot, domain, sector_spacing, ...) {
        if (is_discrete_domain(domain)) {
            align_circle_discrete_facet(
                plot, domain, sector_spacing,
                self$layout_name
            )
        } else {
            align_circle_continuous_facet(
                plot, domain, sector_spacing,
                self$layout_name
            )
        }
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
    setup_circle_plot = function(self, plot, domain, ...) {
        plot + layout_align(x = domain, xlabels = self$labels)
    },

    # ========== General Build/Finish Steps ==========
    # For continuous domains, we should always treat the first column as the
    # facet column
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
        cls <- cls[seq_len(which(cls == "ggalign::CraftDesigner"))]
        sprintf("<Class: %s>", paste(cls, collapse = " "))
    }
)
