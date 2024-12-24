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
scheme_data <- function(data, inherit = FALSE) {
    data <- check_scheme_data(data)
    assert_bool(inherit)
    new_scheme_data(data, inherit)
}

new_scheme_data <- function(data = NULL, inherit = FALSE) {
    new_scheme(
        name = "scheme_data",
        list(data = data, inherit = inherit),
        class = "scheme_data"
    )
}

#' @export
inherit_scheme.scheme_data <- function(scheme, pscheme) {
    if (is.null(o <- .subset2(scheme, "data"))) return(scheme) # styler: off
    if (is.waive(o)) return(pscheme) # inherit from parent; styler: off
    if (!is.function(p_function <- .subset2(pscheme, "data"))) {
        return(scheme)
    }
    # if both are function, we check if we should call parent first then call
    # itself
    if (.subset2(scheme, "inherit")) {
        user_scheme_data <- o # current action data function
        scheme$data <- function(data) {
            # we always restore the attached attribute
            ans <- ggalign_attr_restore(p_function(data), data)
            user_scheme_data(ans)
        }
    }
    scheme
}

#' @export
plot_add_scheme.scheme_data <- function(plot, scheme) {
    # by default, we won't change the data
    if (!is.null(scheme_data <- .subset2(scheme, "data") %|w|% NULL) &&
        !is.null(raw_data <- .subset2(plot, "data"))) {
        # To be compatible with ggplot2, it must be a data frame
        if (!is.null(data <- scheme_data(raw_data)) &&
            !is.waive(data) &&
            !is.data.frame(data)) {
            cli_abort("{.fn scheme_data} must return a {.cls data.frame}")
        }
        plot <- gguse_data(plot, data)
    }
    plot
}
