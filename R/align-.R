#' Create a New `Align` Object
#'
#' `r lifecycle::badge('experimental')` An `Align` object interacts with the
#' `layout` object to reorder or split observations and, in some cases, add plot
#' components to the `layout`.
#'
#' @param align_class A `Align` object.
#' @param params A list of parameters for `align_class`.
#' @param data Options for `data`:
#'  - A matrix, data frame, or atomic vector.
#'  - [`waiver()`]: Uses the `layout matrix`.
#'  - `NULL`: No data is set.
#'  - A `function` (including purrr-like lambda syntax) applied to the layout
#'    `matrix`.
#'
#' @param size The relative size of the plot, can be specified as a
#' [`unit`][grid::unit].
#' @param controls Options for `controls`:
#'  - `NULL`: Used when `align_*()` functions do not add a plot.
#'  - [`waiver()`]: Try to infer `controls` based on `data`.
#'
#' @param limits Logical; if `TRUE`, sets layout limits for the plot.
#' @param facet Logical; if `TRUE`, applies facets to the layout. If `FALSE`,
#'   `limits` will also be set to `FALSE`.
#' @param no_axes `r lifecycle::badge('experimental')` Logical; if `TRUE`,
#'   removes axes elements for the alignment axis using [`theme_no_axes()`]. By
#'   default, will controled by the option-
#'   `r code_quote(sprintf("%s.align_no_axes", pkg_nm()))`.
#' @param active A [`active()`] object that defines the context settings when
#'   added to a layout.
#' @param free_guides `r lifecycle::badge("superseded")` Please use
#'   [`plot_align()`] function instead.
#' @param free_spaces `r lifecycle::badge("deprecated")` Please use
#' [`plot_align()`] function instead.
#' @param plot_data `r lifecycle::badge("deprecated")` Please use
#' [`plot_data()`] function instead.
#' @param theme `r lifecycle::badge("deprecated")` Please use
#' [`-`][layout-operator] method instead.
#' @param free_labs `r lifecycle::badge("deprecated")` Please use
#' [`plot_align()`] function instead.
#' @param check.param Logical; if `TRUE`, checks parameters and provides
#'   warnings as necessary.
#' @param call The `call` used to construct the `Align` object, for reporting
#'   messages.
#'
#' @section Axis Alignment for Observations:
#' It is important to note that we consider rows as observations, meaning
#' `vec_size(data)`/`NROW(data)` must match the number of observations along the
#' axis used for alignment (x-axis for a vertical stack layout, y-axis for a
#' horizontal stack layout).
#'
#'  - [`quad_layout()`]/[`ggheatmap()`]: For column annotation, the layout
#'    `matrix` will be transposed before use (if `data` is a function, it is
#'    applied to the transposed matrix), as column annotation uses columns as
#'    observations but alignment requires rows.
#'  - [`stack_layout()`]: The layout matrix is used as is, aligning all plots
#'    along a single axis.
#'
#' @return A new `Align` object.
#'
#' @examples
#' align_gg()
#' align_dendro()
#'
#' @importFrom rlang caller_call current_call
#' @importFrom ggplot2 ggproto
#' @export
#' @keywords internal
align <- function(align_class, params, data, size = NULL, controls = NULL,
                  limits = TRUE, facet = TRUE, no_axes = NULL, active = NULL,
                  free_guides = deprecated(), free_spaces = deprecated(),
                  plot_data = deprecated(), theme = deprecated(),
                  free_labs = deprecated(),
                  check.param = TRUE, call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    deprecate_action(
        snake_class(align_class),
        plot_data = plot_data,
        theme = theme,
        free_guides = free_guides,
        free_spaces = free_spaces,
        free_labs = free_labs,
        call = call
    )

    # check arguments ---------------------------------------------
    data <- allow_lambda(data)
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    assert_bool(facet, call = call)
    assert_bool(limits, call = call)
    assert_bool(no_axes, null_ok = TRUE, call = call)
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)

    if (is.waive(controls)) {
        controls <- new_controls(
            new_plot_data(if (is.waive(data)) waiver() else NULL)
        )
    }

    # Warn about extra params or missing parameters ---------------
    all <- align_class$parameters()
    if (isTRUE(check.param)) {
        if (length(extra_param <- vec_set_difference(names(params), all))) { # nolint
            cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
        }
        if (length(missing <- vec_set_difference(all, names(params)))) { # nolint
            cli::cli_abort("missing parameters: {missing}", call = call)
        }
    }

    new_align(
        # wrap all elements into this annotation ---------------------
        Object = ggproto(
            NULL,
            align_class,
            isLock = FALSE,
            # Following fields will be initialzed when added into the layout
            # and will be saved and accessed across the plot rendering process
            direction = NULL,
            position = NULL,
            params = NULL, # `$setup_params` method
            data = NULL, # $setup_data method
            statistics = NULL, # `$compute` method
            labels = NULL, # the original `vec_names()` of the `input_data`

            # use `NULL` if this align don't require any data
            # use `waiver()` to inherit from the layout data
            input_data = data,

            # collect parameters
            input_params = params[vec_set_intersect(names(params), all)],

            # used to provide error message
            call = call
        ),
        no_axes = no_axes,
        controls = controls,
        facet = facet,
        limits = limits,

        # user input -------------------------------
        size = size,

        # should we allow user switch between different plot with a string name?
        # Should I remove "name" argument from the user input?
        active = active,
        plot = NULL
    )
}

#' We create the align entity when initialize the Align object.
#' @noRd
new_align <- function(Object, ..., plot = NULL) {
    structure(list(Object = Object, ..., plot = plot), class = "align")
}

is_align <- function(x) inherits(x, "align")

#' @export
#' @keywords internal
plot.align <- function(x, ...) {
    cli::cli_abort("You cannot plot {.obj_type_friendly {x}} object directly")
}

#' @details
#' Each of the `Align*` objects is just a [`ggproto()`][ggplot2::ggproto]
#' object, descended from the top-level `Align`, and each implements various
#' methods and fields.
#'
#' To create a new type of `Align*` object, you typically will want to
#' override one or more of the following:
#'  - `setup_params`: Prepare parameter or check parameters used by this plot.
#'  - `setup_data`: Prepare data used by this plot.
#'  - `compute`: A method used to compute statistics.
#'  - `layout`: A method used to group observations into panel or reorder
#'    observations.
#'  - `draw`: A method used to draw the plot. Must return a `ggplot` object.
#' @importFrom ggplot2 ggproto
#' @export
#' @format NULL
#' @usage NULL
#' @rdname align
Align <- ggproto("Align",
    parameters = function(self) {
        c(
            align_method_params(self$compute),
            align_method_params(self$layout),
            align_method_params(self$ggplot, character()),
            align_method_params(
                self$draw,
                c("plot", "panel", "index", "extra_panel", "extra_index")
            ),
            self$extra_params
        )
    },
    lock = function(self) {
        assign("isLock", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("isLock", value = FALSE, envir = self)
    },

    # Most parameters for the `Align` are taken automatically from `compute()`,
    # `layout()` and `draw()`. However, some additional parameters may be
    # removed in `setup_params`. You should put these paramters here, otherwise,
    # they won't be collected.
    extra_params = character(),
    setup_params = function(nobs, params) params,
    setup_data = function(params, data) data,

    # You must provide `nobs()` function or data shouldn't be `NULL`
    # If this `Align` doesn't initialize the layout observations, we should
    # return `NULL`, in this way, you cannot use this `Align` object to
    # initialize the layout panel or index. We always ensure the panel and index
    # number is equal to `nobs`. If you want to indicates no obervations, you
    # must return `0L`.
    nobs = function(params) NULL,

    # Following fields should be defined for the new `Align` object.
    # argument name in these function doesn't matter.
    compute = function(self, panel, index) NULL,

    # Group heamap row/column and reorder, Must return a list of 2:
    #  - the first one should be the groups for heatmap row/column, the factor
    #    levels will determine the panel order, so it should always follow the
    #    index if you don't want the panel levels break the index. See
    #    `AlignDendro` for example.
    #  - the second one should be the heatmap row/column order index, and will
    #    determine the order in each grouped panel.
    #
    # See `align_initialize_layout` function for details
    # There will have following situations (the input is old index and old
    # panel):
    #
    # 1. old index is NULL and old panel is NULL, there is nothing wrong to
    #    define any new index or panel
    # 2. old index is `NULL` and old panel is not `NULL`, in this way, new
    #    index must follow the old panel.
    #
    #    For new `Align` object, which can do clustering, we must
    #    abort, if it can not do sub-clustering, if it can do sub-clustering, we
    #    should know if we want to change the order between the groups (panel
    #    levels).
    #
    #    Please check `AlignGroup` object and `Align` object
    #    For dendrogram, it can do sub-clustering within each group, it also
    #    allows reordering between groups (it provide `reorder_group` argument),
    #    so the new panel levels may be not the same with old panel
    #
    #    For `Align` object reordering the heatmap rows/columns.
    #    usually we provide a `strict` argument, to allow reorder heatmap within
    #    group only. See `AlignReorder`.
    #
    # 3. old index is not `NULL`, no matter whether old panel is `NULL` or not,
    #    in this way, we should always ensure the new index won't change the old
    #    index, this will be checked in `align_initialize_layout` function.
    layout = function(self, panel, index) list(panel, index),

    # initialize the ggplot object, if `NULL`, no plot area will be added.  we
    # must separate this method with draw method, since other ggplot elements
    # will be added for this plot.
    ggplot = function(self) NULL,

    # Following methods will be executed when building plot with the final
    # heatmap layout you shouldn't modify the `Align` object when drawing,
    # since all of above process will only run once.
    draw = function(self, plot, panel, index, extra_panel, extra_index) plot
)

# Used to lock the `Align` object
#' @export
`$<-.Align` <- function(x, name, value) {
    if (.subset2(x, "isLock")) {
        cli::cli_abort("{.fn {snake_class(x)}} is locked",
            call = .subset2(x, "call")
        )
    }
    assign(x = name, value = value, envir = x)
    invisible(x)
}

ggproto_formals <- function(x) formals(environment(x)$f)

align_method_params <- function(f, remove = c("panel", "index")) {
    vec_set_difference(names(ggproto_formals(f)), c("self", remove))
}
