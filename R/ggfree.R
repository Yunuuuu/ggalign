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
    rlang::check_dots_used()
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
    plot <- data
    # In ggplot2, `waiver()` was regard to no data
    data <- .subset2(plot, "data") %|w|% NULL
    plot <- gguse_data(plot, waiver())
    new_free_gg(plot, data, size = size, active = active)
}

new_free_gg <- function(plot, data, size, active,
                        call = caller_call()) {
    assert_active(active, allow_null = TRUE, call = call)
    active <- update_active(active, new_active(use = TRUE))
    new_ggalign_plot(
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
FreeGg <- ggproto("FreeGg", AlignProto,
    free_facet = TRUE,
    free_limits = TRUE,
    interact_layout = function(self, layout) {
        layout_data <- layout@data
        if (is.waive(input_data <- self$input_data)) { # inherit from the layout
            data <- layout_data
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
        self$data <- ggalign_attr_restore(fortify_data_frame(data), layout_data)
        layout
    },
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        if (is.function(data <- self$data)) {
            data <- NULL
        }
        # if inherit from the parent layout
        if (!is.null(data) &&
            is.waive(self$input_data) &&
            is_discrete_design(extra_design) &&
            !is.null(extra_panel <- .subset2(extra_design, "panel"))) {
            extra_index <- .subset2(extra_design, "index")
            data <- cross_join(data, data_frame0(
                .extra_panel = extra_panel, .extra_index = extra_index
            ))
        }
        gguse_data(plot, data)
    },
    summary = function(self, plot) {
        header <- ggproto_parent(AlignProto, self)$summary(plot)
        c(header, "  Add plot without alignment")
    }
)
