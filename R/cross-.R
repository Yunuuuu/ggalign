# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
cross <- function(cross, data = waiver(), ...,
                  plot = NULL, active = NULL, size = NULL,
                  schemes = NULL, call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_ggalign_plot(
        align = cross,
        input_data = allow_lambda(data),
        ...,
        plot = plot, active = active, size = size, schemes = schemes,
        call = call
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include plot-.R
Cross <- ggproto("Cross", AlignProto,
    reset_panel = FALSE,
    reset_nobs = FALSE,
    free_facet = TRUE,
    free_limits = TRUE,
    interact_layout = function(self, layout) {
        #  1. check layout is `*_cross()`
        #  2. add `cross_points`
        #  3. add `odesign`
        layout <- ggproto_parent(CrossGg, self)$interact_layout(layout)
        if (length(layout@break_points) &&
            is.null(.subset2(layout@design, "nobs"))) {
            cli_abort(sprintf(
                "layout {.field nobs} for %s must be initialized before adding %s",
                self$layout_name, object_name(self)
            ))
        }

        # will define labels0
        self$labels0 <- self$labels
        self$labels <- NULL

        # udpate break_points
        layout@break_points <- c(layout@break_points, length(layout@plot_list))
        layout
    },
    setup_design = function(self, design) {
        if (self$reset_nobs) {
            design["nobs"] <- list(NULL)
        }
        if (self$reset_nobs || self$reset_panel) {
            design["panel"] <- list(NULL)
        }
        design["index"] <- list(NULL) # always reset the index
        design
    }
)
