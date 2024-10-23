#' Create a new `Align` object
#'
#' `Align` object will act with the `layout` object, reorder or split the
#' observations, some of them can also add plot components into the `layout`
#' object.
#'
#' @param data A matrix, data frame, or a simple vector. If an atomic vector is
#' provided, it will be converted into a one-column matrix. When `data = NULL`,
#' the internal `layout` data will be used by default. Additionally, `data` can
#' be a function (including purrr-like lambdas), which will be applied to the
#' `layout` data.
#'
#' It is important to note that we consider the `rows` as the observations. It
#' means the `NROW(data)` must return the same number with the specific `layout`
#' axis (meaning the x-axis for vertical stack layout, or y-axis for horizontal
#' stack layout).
#'
#'  - `heatmap_layout()`: for column annotation, the `layout` data will be
#'  transposed before using (If data is a `function`, it will be applied with
#'  the transposed matrix). This is necessary because column annotation uses
#'  heatmap columns as observations, but we need rows.
#'
#'  - `stack_layout()`: the `layout` data will be used as it is since we place
#'    all plots along a single axis.
#'
#' @param size Plot size, can be an [unit][grid::unit] object.
#' @param action A [plot_action()] object used for the plot.
#' @param action_data Default action data, which by default tries to infer 
#' from the `data` argument.
#' @param limits A boolean value indicates whether to set the layout limtis for
#' the plot.
#' @param facet A boolean value indicates whether to set the layout facet for
#' the plot. If this is `FALSE`, `limits` will always be `FALSE` too.
#' @param set_context A single boolean value indicates whether to set the active
#' context to current plot. If `TRUE`, all subsequent ggplot elements will be
#' added into this plot.
#' @param order An single integer for the plot area order.
#' @param name A string of the plot name. Used to switch the active context in
#' [hmanno()] or [stack_active()].
#' @inheritParams hmanno
#' @param check.param A single boolean value indicates whether to check the
#' supplied parameters and warn.
#' @param call The `call` used to construct the `Align` for reporting messages.
#' @return A new `Align` object.
#' @examples
#' align_gg()
#' align_dendro()
#' @importFrom rlang caller_call current_call
#' @importFrom ggplot2 ggproto
#' @importFrom vctrs vec_set_difference
#' @importFrom lifecycle deprecated
#' @export
#' @keywords internal
align <- function(align_class, params,
                  # this function is used for adding more `Align` Class
                  # Not used by the user.
                  # The `data` argument is different from the documents in which
                  # `NULL` means this `Align` object won't need any data,
                  # and `waiver()` will indicates inherit from the layout.
                  # So when adding a new `Align` object where user should input
                  # data, always remember transfrom `NULL` to `waiver()`
                  #
                  # Details see `initialize_align()`
                  data, size = NULL,
                  # The `action` argument is different from the documents in
                  # which `NULL` means this `Align` object won't plot,
                  # and `waiver()` will indicates we should set the default
                  # action for this `Align` object.
                  action = NULL, action_data = NA,
                  limits = TRUE, facet = TRUE,
                  set_context = TRUE, order = NULL, name = NULL,
                  free_guides = deprecated(), free_spaces = deprecated(),
                  plot_data = deprecated(), theme = deprecated(),
                  free_labs = deprecated(),
                  check.param = TRUE, call = caller_call()) {
    if (align_override_call(call)) {
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
    assert_bool(set_context, call = call)
    order <- check_order(order, call = call)
    assert_string(name,
        empty_ok = FALSE, na_ok = TRUE,
        null_ok = TRUE, call = call
    )

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
        name = name %||% NA_character_,
        order = order,
        set_context = set_context,
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

#' @importFrom utils packageName
align_override_call <- function(call = NULL) {
    if (is.null(call) || is.function(f <- .subset2(call, 1L))) {
        return(TRUE)
    }
    !identical(
        packageName(environment(eval(f))),
        pkg_nm()
    )
}

#' @export
#' @keywords internal
plot.Align <- function(x, ...) {
    cli::cli_abort("You cannot plot {.obj_type_friendly {x}} object directly")
}

is_align <- function(x) inherits(x, "Align")

#' @section Align:
#' Each of the `Align*` objects is just a [ggproto()][ggplot2::ggproto] object,
#' descended from the top-level `Align`, and each implements various
#' methods and fields.
#'
#' To create a new type of `Align*` object, you typically will want to
#' override one or more of the following:
#'  - `setup_params`: Prepare parameter or check parameters used by this
#'    annotation.
#'  - `setup_data`: Prepare data used by this annotation.
#'  - `compute`: A method used to compute statistics.
#'  - `layout`: A method used to group heamap rows/columns into panel or
#'    reorder heamtap rows/columns.
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
    # See `align_layout` function for details
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
    #    index, this will be checked in `align_layout` function.
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

#' @importFrom vctrs vec_set_difference
align_method_params <- function(f, remove = c("panel", "index")) {
    vec_set_difference(names(ggproto_formals(f)), c("self", remove))
}
