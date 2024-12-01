#' Plot data Specifications
#'
#' @description
#' `r lifecycle::badge('experimental')`
#'
#' Transforms the plot data. Many functions in this package require a specific
#' data format to align observations, `plot_data()` helps reformat data frames
#' as needed.
#'
#' @param data A function to transform the plot data before rendering, referred
#' to as `plot_data`. Acceptable values include:
#'
#' - `NULL`: No action taken.
#' - [`waiver()`][ggplot2::waiver()]: Inherits from the parent layout.
#' - A `function` or purrr-style `formula`: Used to transform the plot data,
#'   which should accept a data frame and return a data frame. You can apply
#'   this after the parent layout `plot_data` function, using the `inherit`
#'   argument.
#'
#' Use this hook to modify the data for all `geoms` after the layout is created
#' (for matrix data, it has been melted to a long format data frame) but before
#' rendering by `ggplot2`. The returned data must be a data frame for ggplot.
#'
#' @param inherit A single boolean value indicates whether to apply the parent
#' `plot_data` first and then apply the specified `plot_data` for the plot.
#' Defaults to `FALSE`.
#'
#' @details
#' Defaults will attempt to inherit from the parent layout if the actual data is
#' inherited from the parent layout, with one exception: `align_dendro()`, which
#' will not inherit the `plot_data` by default.
#'
#' @export
plot_data <- function(data, inherit = FALSE) {
    data <- check_plot_data(data)
    assert_bool(inherit)
    new_plot_data(data, inherit)
}

new_plot_data <- function(data = NULL, inherit = FALSE) {
    new_option(
        name = "plot_data",
        option = list(data = data, inherit = inherit),
        class = "plot_data"
    )
}

#' @export
inherit_option.plot_data <- function(option, poption) {
    if (is.null(o <- .subset2(option, "data"))) return(option) # styler: off
    if (is.waive(o)) return(poption) # inherit from parent; styler: off
    if (!is.function(p_function <- .subset2(poption, "data"))) {
        return(option)
    }
    # if both are function, we check if we should call parent first then call
    # itself
    if (.subset2(option, "inherit")) {
        user_plot_data <- o # current action data function
        option$data <- function(data) {
            # we always restore the attached attribute
            ans <- ggalign_attr_restore(p_function(data), data)
            user_plot_data(ans)
        }
    }
    option
}

#' @export
plot_add_option.plot_data <- function(option, plot) {
    # by default, we won't change the data
    if (!is.null(plot_data <- .subset2(option, "data") %|w|% NULL) &&
        !is.null(raw_data <- .subset2(plot, "data"))) {
        # To be compatible with ggplot2, it must be a data frame
        if (!is.data.frame(data <- plot_data(raw_data))) {
            cli_abort("{.fn plot_data} must return a {.cls data.frame}")
        }
        plot$data <- data
    }
    plot
}
