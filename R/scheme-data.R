#' Plot data Specifications
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Transforms the plot data. Many functions in this package require a specific
#' data format to align observations, `scheme_data()` helps reformat data frames
#' as needed.
#'
#' @param data A function to transform the plot data before rendering.
#' Acceptable values include:
#'
#' - `NULL`: No action taken.
#' - [`waiver()`][ggplot2::waiver()]: Inherits from the parent layout.
#' - A `function` or purrr-style `formula`: Used to transform the plot data,
#'   which should accept a data frame and return a data frame. You can apply
#'   this after the parent layout `scheme_data` function, using the `inherit`
#'   argument.
#'
#' Use this hook to modify the data for all `geoms` after the layout is created
#' (for matrix data, it has been melted to a long format data frame) but before
#' rendering by `ggplot2`. The returned data must be a data frame for ggplot.
#'
#' @param inherit A single boolean value indicates whether to apply the parent
#' `scheme_data` first and then apply the specified `scheme_data` for the plot.
#' Defaults to `FALSE`.
#'
#' @details
#' Defaults will attempt to inherit from the parent layout if the actual data is
#' inherited from the parent layout, with one exception: `align_dendro()`, which
#' will not inherit the `scheme_data` by default.
#'
#' @export
scheme_data <- S7::new_class(
    "scheme_data",
    parent = Scheme,
    properties = list(
        data = S7::new_property(
            S7::class_any,
            validator = function(value) {
                if (is_waiver(value) || is.null(value) || is.function(value)) {
                    return(NULL)
                }
                sprintf("must be a function, `NULL`, or {.fn waiver}")
            },
            setter = function(self, value) {
                prop(self, "data") <- allow_lambda(value)
                self
            },
            default = NULL
        ),
        inherit = S7::new_property(
            S7::class_logical,
            validator = function(value) {
                if (length(value) != 1L) {
                    return("must be of length 1")
                }
            },
            default = FALSE
        )
    )
)

#' @importFrom S7 prop prop<-
S7::method(scheme_inherit, list(scheme_data, scheme_data)) <- function(e1, e2) {
    if (is.null(scheme_data <- prop(e2, "data"))) return(e2) # styler: off
    if (is_waiver(scheme_data)) return(e1) # inherit from parent; styler: off
    if (!is.function(p_function <- prop(e1, "data"))) {
        return(e2)
    }
    # if both are function, we check if we should call parent first then call
    # itself
    if (prop(e2, "inherit")) {
        prop(e2, "data", check = FALSE) <- function(data) {
            # we always restore the attached attribute
            ans <- ggalign_data_restore(p_function(data), data)
            scheme_data(ans)
        }
    }
    e2
}

#' @importFrom S7 prop
S7::method(plot_add_scheme, list(ggplot2::class_ggplot, scheme_data)) <-
    function(plot, scheme, ...) {
        if (!is.null(scheme_data <- prop(scheme, "data") %|w|% NULL) &&
            !is.null(raw_data <- plot$data)) {
            # To be compatible with ggplot2, it must be a data frame
            if (!is.null(data <- scheme_data(raw_data)) &&
                !is_waiver(data) &&
                !is.data.frame(data)) {
                cli_abort("{.fn scheme_data} must return a {.cls data.frame}")
            }
            plot <- gguse_data(plot, data)
        }
        plot
    }
