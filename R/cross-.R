# Since `ggalign_align_plot` object need act with the layout, we Use R6 object
# here
cross <- function(cross, ..., call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    new_ggalign_plot(align = cross, ..., call = call)
}

is_cross_plot <- function(x) is_ggalign_plot(x) && is_cross(x@align)

is_cross <- function(x) inherits(x, "Cross")

#' @export
summary.Cross <- function(object, ...) c(TRUE, FALSE)

#' @importFrom ggplot2 ggproto ggplot
#' @include plot-.R
Cross <- ggproto("Cross", AlignProto,
    layout = function(self, layout_data, layout_coords, layout_name) {
        if (is.null(.subset2(layout_coords, "nobs"))) {
            cli_abort(sprintf(
                "layout observations for %s must be initialized before adding {.var {%s}}",
                layout_name, .subset2(self, "object_name")
            ))
        }
        # we keep the names from the layout data for usage
        self$labels <- vec_names(layout_data)
        layout_coords["index"] <- list(NULL) # reset the index
        layout_coords
    },
    finish_layout = function(self, layout) {
        # udpate cross_points
        layout@cross_points <- c(layout@cross_points, length(layout@plot_list))
        # update index
        layout@index_list <- c(
            layout@index_list,
            list(.subset2(layout@layout, "index"))
        )
        layout
    }
)
