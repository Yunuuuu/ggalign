# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
cross <- function(cross, ..., call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_ggalign_plot(align = cross, ..., call = call)
}

#' @export
summary.Cross <- function(object, ...) c(TRUE, FALSE)

#' @importFrom ggplot2 ggproto ggplot
#' @include plot-.R
Cross <- ggproto("Cross", AlignProto,
    setup_layout = function(self, layout) {
        if (!is_cross_layout(layout)) {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(self), self$layout_name
                ),
                i = sprintf(
                    "%s can only be used in {.fn cross_discrete}",
                    object_name(self)
                )
            ))
        }

        # udpate cross_points
        layout@cross_points <- c(layout@cross_points, length(layout@plot_list))
        # update index
        layout@index_list <- c(
            layout@index_list,
            list(.subset2(layout@design, "index"))
        )
        layout
    },
    setup_design = function(self, layout_data, design) {
        object_name <- .subset2(self, "object_name")
        layout_name <- .subset2(self, "layout_name")
        if (is.null(.subset2(design, "nobs"))) {
            cli_abort(sprintf(
                "layout observations for %s must be initialized before adding {.var {%s}}",
                layout_name, object_name
            ))
        }
        # we keep the names from the layout data for usage
        self$labels <- vec_names(layout_data)
        design["index"] <- list(NULL) # reset the index
        design
    }
)
