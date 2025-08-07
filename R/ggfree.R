#' Add ggplot to layout without alignment
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' The `ggfree()` function allows you to incorporate a ggplot object into your
#' layout. Unlike `ggalign()`, which aligns every axis value precisely,
#' `ggfree()` focuses on integrating plots into the layout without enforcing
#' strict axis alignment.
#'
#' @inheritParams ggalign
#'
#' @section ggplot2 specification:
#' `ggalign` initializes a ggplot object. The underlying data is created using
#' [`fortify_data_frame()`]. Please refer to this method for more details.
#'
#' When used in `quad_layout()`/`ggheatmap()`, if the data is inherited from the
#' `quad_layout()` and the other direction aligns discrete variables, following
#' columns will be added:
#'
#'  - `.extra_panel`: Provides the panel information for the column (left or
#'    right annotation) or row (top or bottom annotation).
#'  - `.extra_index`: The index information for the column (left or right
#'    annotation) or row (top or bottom annotation).
#'
#' @examples
#' ggheatmap(matrix(rnorm(56), nrow = 7)) +
#'     anno_top() +
#'     align_dendro() +
#'     ggfree(mtcars, aes(wt, mpg)) +
#'     geom_point()
#' @export
ggfree <- function(data = waiver(), ..., size = NULL, active = NULL) {
    UseMethod("ggfree", data)
}

#' @inheritParams ggplot2::ggplot
#' @importFrom ggplot2 ggplot
#' @export
#' @rdname ggfree
ggfree.default <- function(data = waiver(), mapping = aes(), ...,
                           size = NULL, active = NULL) {
    data <- fortify_data_frame(data = data, ...)
    new_free_gg(
        plot = ggplot(data = NULL, mapping = mapping),
        data = data,
        size = size,
        active = active
    )
}

#' @export
ggfree.uneval <- function(data = waiver(), ...) {
    cli_abort(c(
        "{.arg data} cannot be {.obj_type_friendly {data}}",
        "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
    ))
}

#' @export
ggfree.ggplot <- function(data = waiver(), ..., size = NULL, active = NULL) {
    rlang::check_dots_empty()
    plot <- data
    # In ggplot2, `waiver()` was regard to no data
    data <- plot$data %|w|% NULL
    plot <- gguse_data(plot, waiver())
    new_free_gg(plot, data, size = size, active = active)
}

new_free_gg <- function(plot, data, size, active,
                        call = caller_call()) {
    assert_active(active, allow_null = TRUE, call = call)
    active <- active_update(active(use = TRUE), active)
    new_craftbox(
        FreeGg,
        # new field for FreeGg
        input_data = data,
        # slots for the plot
        plot = plot,
        size = size,
        active = active,
        schemes = default_schemes(data),
        call = call
    )
}

#' @importFrom ggplot2 ggproto
FreeGg <- ggproto("FreeGg", Craftsman,
    interact_layout = function(self, layout) {
        layout_data <- layout@data
        if (is.waive(input_data <- self$input_data)) { # inherit from the layout
            data <- layout_data
            self$labels <- vec_names(layout_data)

            # for data inherit from the layout, and the domain is for discrete
            # variable, we'll integrate the domain into the plot data
            self$use_domain <- is_stack_layout(layout)

            # if the layout data is from the quad-layout, we use the discrete
            # `domain`
            self$use_extra_domain <- is_stack_layout(layout) &&
                isTRUE(layout@heatmap$quad_matrix)
        } else if (is.function(input_data)) {
            if (is.null(layout_data)) {
                cli_abort(c(
                    sprintf(
                        "{.arg data} in %s cannot be a function",
                        object_name(self)
                    ),
                    i = sprintf("no data was found in %s", self$layout_name)
                ))
            }
            data <- input_data(layout_data)
        } else {
            data <- input_data
        }
        self$data <- ggalign_data_restore(
            fortify_data_frame(data, call = self$call), layout_data
        )
        layout
    },
    setup_stack_facet = function(self, plot, ...) plot,
    align_stack_plot = function(self, plot, ...) plot,
    setup_circle_facet = function(self, plot, ...) plot,
    align_circle_plot = function(self, plot, ...) plot,
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        if (is.function(data <- self$data)) {
            data <- NULL
        }
        if (is.null(data)) {
            return(gguse_data(plot, data))
        }
        if (isTRUE(self$use_extra_domain) &&
            is_discrete_domain(extra_domain) &&
            !is.na(prop(extra_domain, "nobs"))) {
            extra_plot_data <- data_frame0(
                .extra_panel = prop(extra_domain, "panel"),
                .extra_index = prop(extra_domain, "index")
            )
        } else {
            extra_plot_data <- NULL
        }

        # if inherit from the parent layout
        if (isTRUE(self$use_domain) &&
            is_discrete_domain(domain) &&
            !is.na(prop(domain, "nobs"))) {
            plot_data <- data_frame0(
                .panel = prop(domain, "panel"),
                .index = prop(domain, "index"),
                .names = .subset(self$labels, prop(domain, "index"))
            )
            if (!is.null(extra_plot_data)) {
                plot_data <- cross_join(plot_data, extra_plot_data)
                data <- full_join(data, plot_data,
                    by.x = c(".column_index", ".row_index"),
                    by.y = c(".extra_index", ".index")
                )
            } else {
                data <- full_join(data, plot_data,
                    by.x = ".row_index", by.y = ".index"
                )
            }
        } else if (!is.null(extra_plot_data)) {
            data <- full_join(data, extra_plot_data,
                by.x = ".column_index", by.y = ".extra_index"
            )
        }
        gguse_data(plot, data)
    },
    summary = function(self, plot) {
        header <- ggproto_parent(Craftsman, self)$summary(plot)
        c(header, "  Add plot without alignment")
    }
)
