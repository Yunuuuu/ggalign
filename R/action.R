#' Plot Action Specifications in the Layout
#'
#' The `plot_action()` function defines the behavior of plots within a layout.
#' It can be used in the `action` argument of layout functions like `hmanno()`
#' or `stack_active()` to set global actions for all plots in the layout.
#' Additionally, `plot_action()` can be applied directly to specific plots
#' through the `action` argument in the `align_*()` functions, or it can be
#' added directly to a plot.
#'
#' @param data A function to transform the plot data before rendering. Defaults
#' to [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If
#' no parent layout is specified, the default is `NULL`, meaning the data won't
#' be modified. Use this hook to modify the data for all `geoms` after the
#' layout is created but before rendering by `ggplot2`. The data returned must
#' be a data frame.
#'
#' @param theme Default plot theme: `r rd_theme()`
#'
#' **Note:** Axis titles and labels that are parallel to the layout axis will
#' always be removed by default. For vertical stack layouts, this refers to the
#' x-axis; for horizontal stack layouts, this refers to the y-axis. To display
#' these axis titles or labels, you must manually add the appropriate
#' [theme()][ggplot2::theme] elements for the parallel axis.
#'
#' @param guides A string with one or more of `r rd_values(.tlbr)` indicating
#' which side of guide legends should be collected. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If no
#' parent layout, all guides will be collected. If `NULL`, no guides will be
#' collected.
#'
#' @param free_spaces A string with one or more of `r rd_values(.tlbr)`
#' indicating which border spaces should be removed. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If no
#' parent, the default is `NULL`, meaning no spaces are removed.
#'
#' Usually you want to apply this with the whole layout, instead of individual
#' plots.
#'
#' @param free_labs A string with one or more of `r rd_values(.tlbr)` indicating
#' which axis titles should be free from alignment. Defaults to
#' [`waiver()`][ggplot2::waiver()], which inherits from the parent layout. If no
#' parent layout, no axis titles will be aligned. If `NULL`, all axis titles
#' will be aligned.
#'
#' @return A `plot_action` object.
#' @examples
#' # used in the layout, define the default action for all plots in the layout
#' ggheatmap(matrix(rnorm(100L), nrow = 10),
#'     action = plot_action(
#'         theme = theme(plot.background = element_rect(fill = "red"))
#'     )
#' )
#'
#' # You can also add it for a single plot
#' ggheatmap(matrix(rnorm(100L), nrow = 10),
#'     action = plot_action(
#'         theme = theme(plot.background = element_rect(fill = "red"))
#'     )
#' ) + plot_action( # here, we modify the plot action for the heatmap body
#'     theme = theme(plot.background = element_rect(fill = "blue"))
#' )
#'
#' @export
plot_action <- function(data = NA, theme = NA, guides = NA,
                        free_spaces = NA, free_labs = NA) {
    if (!identical(data, NA)) data <- check_action_data(data)
    if (!identical(theme, NA)) assert_s3_class(theme, "theme", null_ok = TRUE)
    if (!identical(free_spaces, NA)) assert_layout_position(free_spaces)
    if (!identical(free_labs, NA)) assert_layout_position(free_labs)
    if (!identical(guides, NA)) assert_layout_position(guides)
    structure(
        list(
            data = data,
            theme = theme,
            free_spaces = free_spaces,
            free_labs = free_labs,
            guides = guides
        ),
        class = "plot_action"
    )
}

default_action <- function() {
    structure(
        list(
            data = waiver(), theme = NULL,
            free_spaces = waiver(), free_labs = waiver(),
            guides = waiver()
        ),
        class = "plot_action"
    )
}

#' @importFrom utils modifyList
update_action <- function(old, new) {
    modifyList(old,
        new[!vapply(new, identical, logical(1L), y = NA, USE.NAMES = FALSE)],
        keep.null = TRUE
    )
}

deprecate_action <- function(action, fun, plot_data, theme,
                             free_spaces, free_labs,
                             guides = deprecated(),
                             free_guides = deprecated(),
                             call = caller_call()) {
    if (lifecycle::is_present(free_guides)) {
        lifecycle::deprecate_stop(
            "0.0.5",
            sprintf("%s(free_guides)", fun),
            sprintf("%s(action)", fun)
        )
    }
    if (lifecycle::is_present(guides)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(guides)", fun),
            sprintf("%s(action)", fun)
        )
        assert_layout_position(guides, call = call)
        action["guides"] <- list(guides)
    }
    if (lifecycle::is_present(free_spaces)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(free_spaces)", fun),
            sprintf("%s(action)", fun)
        )
        assert_layout_position(free_spaces, call = call)
        action["free_spaces"] <- list(free_spaces)
    }
    if (lifecycle::is_present(plot_data)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(plot_data)", fun),
            sprintf("%s(action)", fun)
        )
        data <- check_action_data(plot_data, call = call)
        action["data"] <- list(data)
    }
    if (lifecycle::is_present(theme)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(theme)", fun),
            sprintf("%s(action)", fun)
        )
        assert_s3_class(theme, "theme", null_ok = TRUE, call = call)
        action["theme"] <- list(theme)
    }
    if (lifecycle::is_present(free_labs)) {
        lifecycle::deprecate_warn(
            "0.0.5",
            sprintf("%s(free_labs)", fun),
            sprintf("%s(action)", fun)
        )
        assert_layout_position(free_labs, call = call)
        action["free_labs"] <- list(free_labs)
    }
    action
}

#######################################################
inherit_theme <- function(theme, parent) {
    # By default, we'll always complete the theme when building the layout
    # so parent always exist.
    if (is.null(theme)) return(parent) # styler: off
    parent + theme
}

inherit_action <- function(action, parent) {
    action["data"] <- list(.subset2(action, "data") %|w|%
        .subset2(parent, "data"))
    action["theme"] <- list(inherit_theme(
        .subset2(action, "theme"),
        .subset2(parent, "theme")
    ))
    action["free_spaces"] <- list(.subset2(action, "free_spaces") %|w|%
        .subset2(parent, "free_spaces"))
    action["free_labs"] <- list(.subset2(action, "free_labs") %|w|%
        .subset2(parent, "free_labs"))
    action
}

#' @param theme Additional default theme elements to be added for the plot
#' @noRd
plot_add_action <- function(plot, action, theme = NULL, call = caller_call()) {
    # by default, we won't change the data
    if (!is.null(plot_data <- .subset2(action, "data") %|w|% NULL)) {
        # To be compatible with ggplot2, it must be a data frame
        if (!is.data.frame(data <- plot_data(.subset2(plot, "data")))) {
            cli::cli_abort(
                "plot action {.arg data} must return a {.cls data.frame}",
                call = call
            )
        }
        plot$data <- data
    }

    # setup plot theme
    plot$theme <- (.subset2(action, "theme") %||% default_theme()) +
        theme + .subset2(plot, "theme")

    # `align_plots` control how to inherit `guides` from the layout
    if (!is.waive(free_guides <- .subset2(action, "guides"))) {
        plot <- free_guide(plot, free_guides)
    }
    # by default, we'll attach all labs to the axis
    if (!is.null(free_labs <- .subset2(action, "free_labs") %|w|% "tlbr")) {
        plot <- free_lab(plot, free_labs)
    }
    # by default, we won't remove any spaces
    if (!is.null(free_spaces <- .subset2(action, "free_spaces") %|w|% NULL)) {
        plot <- free_space(free_border(plot, free_spaces), free_spaces)
    }
    plot
}
