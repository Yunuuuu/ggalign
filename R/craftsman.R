is_craftsman <- function(x) inherits(x, "Craftsman")

#' @importFrom ggplot2 ggproto
Craftsman <- ggproto("Craftsman",
    call = NULL,

    # These fields are populated when added to a layout
    in_linear = NULL,
    layout_name = NULL,
    direction = NULL,
    position = NULL, # e.g., used by stack_layout() in quad_layout()
    labels = NULL,

    # A single boolean value indicates whether we should set facet and coord
    free_facet = FALSE,
    free_coord = FALSE,
    free_limits = FALSE,

    # we always prevent user from modifying the object in `$build_plot()` and
    # `$finish_plot()` methods
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

    # we define the `panel` and `index` method in `setup_domain` method
    setup_domain = function(self, domain) domain,
    init_plot = function(self, plot) plot,

    ##############################################################
    # Don't change the facet and coord in following methods
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_scheme(plot, schemes)
        ggremove_margin(plot, self$direction) + theme_recycle()
    },

    # utils method to print the object, should return a character vector
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
