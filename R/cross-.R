# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
cross <- function(cross, ..., call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_ggalign_plot(align = cross, ..., call = call)
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include plot-.R
Cross <- ggproto("Cross", AlignProto,
    reset_panel = FALSE,
    reset_nobs = FALSE,
    free_facet = TRUE,
    free_limits = TRUE,
    interact_layout = function(self, layout) {
        if (length(layout@break_points) && is.null(.subset2(design, "nobs"))) {
            cli_abort(sprintf(
                "layout {.field nobs} for %s must be initialized before adding %s",
                self$layout_name, object_name(self)
            ))
        }
        layout <- ggproto_parent(CrossGg, self)$interact_layout(layout)

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
