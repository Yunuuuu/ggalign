# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
cross <- function(cross, ..., call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_ggalign_plot(align = cross, ..., call = call)
}

#' @importFrom ggplot2 ggproto ggplot
#' @include plot-.R
Cross <- ggproto("Cross", AlignProto,
    reset_panel = FALSE,
    reset_nobs = FALSE,
    setup_layout = function(self, layout) {
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
        layout@cross_points <- c(layout@cross_points, length(layout@plot_list))

        # update old design list
        layout@odesign <- c(layout@odesign, list(layout@design))
        layout
    },
    setup_design = function(self, data, design) {
        if (self$reset_nobs && is.null(.subset2(design, "nobs"))) {
            layout_name <- .subset2(self, "layout_name")
            cli_abort(sprintf(
                "layout observations for %s must be initialized before adding %s",
                layout_name, object_name(self)
            ))
        }

        # we keep the names from the layout data for usage
        self$labels <- vec_names(data)
        design["index"] <- list(NULL) # always reset the index
        if (self$reset_nobs || self$reset_panel) {
            design["panel"] <- list(NULL)
        }
        if (self$reset_nobs) {
            design["nobs"] <- list(NULL)
        }
        design
    }
)
