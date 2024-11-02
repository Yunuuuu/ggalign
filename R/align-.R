#' Create a New `Align` Object
#'
#' An `Align` object interacts with the `layout` object to reorder or split
#' observations and, in some cases, add plot components to the `layout`.
#'
#' @param data Options for `data`:
#'  - A matrix, data frame, or atomic vector.
#'  - [`waiver()`]: Uses the `layout matrix`.
#'  - `NULL`: No data is set.
#'  - A `function` (including purrr-like `lambda` syntax), which will be applied
#'    to the `layout matrix`.
#'
#' @param action Options for `action`:
#'  - `NULL`: For `align_*()` functions that do not add a plot.
#'  - [`waiver()`]: Uses the default [`plot_action()`].
#'  - A [`plot_action()`] object to define specific plot actions.
#' @param action_data Default action data, inferred from the `data` argument by
#' default.
#' @param limits Logical; determines if layout limits are set for the plot.
#' @param facet Logical; determines if layout facets are set for the plot. If
#'   `FALSE`, `limits` will also be set to `FALSE`.
#' @param free_guides `r lifecycle::badge("deprecated")` Please use `action`
#' argument instead.
#' @inheritParams free_gg
#' @inheritParams hmanno
#' @param check.param Logical; if `TRUE`, checks the supplied parameters and
#'   provides warnings as necessary.
#' @param call The `call` used to construct the `Align` object, for reporting
#'   messages.
#'
#' @section Aligned Axis:
#' It is important to note that we consider rows as observations, meaning
#' `NROW(data)` must match the number of observations along the specified layout
#' axis (x-axis for a vertical stack layout, y-axis for a horizontal stack
#' layout).
#'
#'  - [`quad_layout()`]/[`ggheatmap()`]: for column annotation, the
#'    `layout matrix` will be transposed before using (If data is a function, it
#'    will be applied with the transposed matrix). This is necessary because
#'    column annotation uses columns as observations, but we need rows.
#'  - [`stack_layout()`]: the `layout matrix` will be used as it is since we
#'    place all plots along a single axis.
#'
#' @return A new `Align` object.
#'
#' @examples
#' align_gg()
#' align_dendro()
#'
#' @importFrom rlang caller_call current_call
#' @importFrom ggplot2 ggproto
#' @importFrom lifecycle deprecated
#' @export
#' @keywords internal
align <- function(align_class, params, data, size = NULL,
                  action = NULL, action_data = NA,
                  limits = TRUE, facet = TRUE, context = NULL,
                  free_guides = deprecated(), free_spaces = deprecated(),
                  plot_data = deprecated(), theme = deprecated(),
                  free_labs = deprecated(),
                  check.param = TRUE, call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }

    # check arguments ---------------------------------------------
    data <- allow_lambda(data)
    if (is.null(size)) {
        size <- unit(NA, "null")
    } else {
        size <- check_size(size, call = call)
    }
    assert_bool(facet, call = call)
    assert_bool(limits, call = call)

    if (!is.null(action)) {
        if (identical(action_data, NA)) {
            # set the default action data behaviour
            # if inherit from the layout data, we'll also inherit the
            # action data function
            action_data <- if (is.waive(data)) waiver() else NULL
        }
        action <- check_action(
            action %|w|% NULL,
            data = action_data,
            arg = "action",
            call = call
        )
        action <- deprecate_action(
            action, snake_class(align_class),
            plot_data, theme,
            free_spaces, free_labs,
            free_guides = free_guides,
            call = call
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

    # wrap all elements into this annotation ---------------------
    ggproto(
        NULL,
        align_class,
        isLock = FALSE,
        # Following fields will be initialzed when added into the layout
        # and will be saved and accessed across the plot rendering process
        statistics = NULL,
        direction = NULL,
        position = NULL,
        plot = NULL,
        data = NULL,
        params = NULL,
        labels = NULL, # the original rownames of the input data

        # user input -------------------------------
        size = size,
        # should we allow user switch between different plot with a string name?
        # Should I remove "name" argument from the user input?
        context = context,
        # use `NULL` if this align don't require any data
        # use `waiver()` to inherit from the layout data
        input_data = data,
        action = action,
        facet = facet,
        limits = limits,

        # collect parameters
        input_params = params[intersect(names(params), all)],

        # used to provide error message
        call = call
    )
}

#' @export
#' @keywords internal
plot.Align <- function(x, ...) {
    cli::cli_abort("You cannot plot {.obj_type_friendly {x}} object directly")
}

is_align <- function(x) inherits(x, "Align")

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
                c("panel", "index", "extra_panel", "extra_index")
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
    draw = function(self, panel, index, extra_panel, extra_index) self$plot
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
