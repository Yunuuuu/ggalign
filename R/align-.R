#' Create `Align` object
#'
#' `Align` object will act with the `layout` object, reorder or split the
#' observations, some of them can also add plot components into the `layout`
#' object.
#'
#' @param data A matrix, a data frame, or even a simple vector that will be
#' converted into a one-column matrix. If the `data` argument is set to `NULL`,
#' the `Align` object will use the `layout` data. Additionally, the `data`
#' argument can also accept a function (purrr-like lambda is also okay), which
#' will be applied with the `layout` data,
#'
#' It is important to note that all `Align` objects consider the `rows` as the
#' observations. It means the `NROW(data)` must return the same number with the
#' parallel `layout` axis.
#'
#'  - `layout_heatmap`: for column annotation, the `layout` data will be
#'  transposed before using (If data is a `function`, it will be applied with
#'  the transposed matrix). This is necessary because column annotation uses
#'  heatmap columns as observations, but we need rows.
#'
#'  - `layout_stack`: the `layout` data will be used as it is since we place all
#'    plots along a single axis.
#'
#' @param size Plot size, can be a [unit][grid::unit] object.
#' @param labels Labels for axis parallelly with the layout, the default will
#' use the rownames of the `data`. One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels
#'   - A character vector giving labels (must be same length as the layout
#'     axis). Note the labels will be reordered based on the layout.
#'   - An expression vector (must be the same length as layout axis). See
#'     `?plotmath` for details.
#'   - A function that takes the default labels as the input and returns labels
#'     as output. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param labels_nudge A single numeric or a numeric value of length
#' `nrow(data)`, to nudge each text label away from the center. One of:
#'   - `NULL` for no breaks
#'   - `waiver()`: if `labels` is `NULL`, then `labels_nudge` will be `NULL`,
#'     otherwise it will inherit from the `layout` object.
#'   - A numeric.
#' @param set_context A single boolean value indicates whether to set the active
#' context to current `Align` object. If `TRUE`, all subsequent ggplot elements
#' will be added into this `Align` object.
#' @param order An single integer for the layout order.
#' @param name A string of the object name.
#' @param check.param A single boolean value indicates whether to check the
#' supplied parameters and warn.
#' @param call The `call` used to construct the `Align` for reporting messages.
#' @return A new `Align` object.
#' @importFrom rlang caller_call current_call
#' @export
#' @keywords internal
align <- function(align_class, params,
                  labels = NULL, labels_nudge = waiver(),
                  size = NULL, data = NULL,
                  set_context = TRUE, order = NA_integer_, name = NULL,
                  check.param = TRUE, call = caller_call()) {
    call <- call %||% current_call()
    # check arguments ---------------------------------------------
    size <- set_size(size)
    if (!is_scalar(size) || !is.unit(size)) {
        cli::cli_abort("{.arg size} must be a single {.cls unit} object.",
            call = call
        )
    }
    data <- allow_lambda(data)
    assert_bool(set_context, call = call)

    if (is.null(order)) {
        order <- NA_integer_
    } else if (!is_scalar(order)) {
        cli::cli_abort("{.arg order} must be a single number", call = call)
    } else if (is.double(order)) {
        order <- as.integer(order)
    } else if (!is.integer(order)) {
        cli::cli_abort("{.arg order} must be a single number", call = call)
    }
    if (!is.null(name) && (!is_string(name) || name == "")) {
        cli::cli_abort(
            "{.arg name} must be a single non-empty string",
            call = call
        )
    }

    # Warn about extra params or missing parameters ---------------
    all <- align_class$parameters()
    if (isTRUE(check.param)) {
        if (length(extra_param <- setdiff(names(params), all))) { # nolint
            cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
        }
        if (length(missing <- setdiff(all, names(params)))) { # nolint
            cli::cli_abort("missing parameters: {missing}", call = call)
        }
    }

    # wrap all elements into this annotation ---------------------
    ggplot2::ggproto(
        NULL,
        align_class,
        isLock = FALSE,
        # Following fields will be initialzed when added into the layout
        # and will be saved and accessed across the plot rendering process
        statistics = NULL,
        direction = NULL,
        plot = NULL,
        data = NULL,
        params = NULL,

        # user input -------------------------------
        size = size,
        name = name,
        order = order,
        set_context = set_context,
        input_data = data,
        # collect parameters
        input_params = params[intersect(names(params), all)],
        facetted_pos_scales = NULL,
        # used to control the labels and breaks of
        # the axis parallelly with the heatmap
        labels = labels,
        labels_nudge = labels_nudge,
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
    cli::cli_abort("You cannot plot {.obj_type_friendly x} object directly")
}

is.align <- function(x) inherits(x, "Align")

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
#'  - `layout`: A method used to group heamap rows/columns into panels or
#'    reorder heamtap rows/columns.
#'  - `draw`: A method used to draw the plot. Must return a `ggplot` object.
#' @export
#' @format NULL
#' @usage NULL
#' @rdname align
Align <- ggplot2::ggproto("Align",
    parameters = function(self) {
        c(
            align_method_params(self$compute),
            align_method_params(self$layout),
            align_method_params(self$ggplot, character()),
            align_method_params(self$draw),
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
    # removed when in `setup_params`. You should put these paramters here,
    # otherwise, they won't be collected.
    extra_params = character(),
    setup_params = function(data, params) params,
    setup_data = function(data, params) NULL,

    # Following fields should be defined for the new `Align` object.
    # argument name in the function doesn't matter.
    compute = function(self, panels, index) NULL,

    # Group heamap row/column and reorder, Must return a list of 2:
    #  - the first one should be the groups for heatmap row/column, the factor
    #    levels will determine the panel order, so it should always follow the
    #    index if you don't want the panel levels break the index. See
    #    `AlignDendro` for example.
    #  - the second one should be the heatmap row/column order index, and will
    #    determine the order in each grouped panels.
    #
    # See `align_layout` function for details
    # There will have following situations (the input is old index and old
    # panels):
    #
    # 1. old index is NULL and old panels is NULL, there is nothing wrong to
    #    define any new index or panels
    # 2. old index is `NULL` and old panels is not `NULL`, in this way, new
    #    index must follow the old panels.
    #
    #    For new `Align` object, which can do clustering, we must
    #    abort, if it can not do sub-clustering, if it can do sub-clustering, we
    #    should know if we want to change the order between the groups (panel
    #    levels).
    #
    #    Please check `AlignGroup` object and `Align` object
    #    For dendrogram, it can do sub-clustering within each group, it also
    #    allows reordering between groups (it provide `reorder_group` argument),
    #    so the new panels levels may be not the same with old panels
    #
    #    For `Align` object reordering the heatmap rows/columns.
    #    usually we provide a `strict` argument, to allow reorder heatmap within
    #    group only. See `AlignReorder`.
    #
    # 3. old index is not `NULL`, no matter whether old panels is `NULL` or not,
    #    in this way, we should always ensure the new index won't change the old
    #    index, this will be checked in `align_layout` function.
    layout = function(self, panels, index) list(panels, index),

    # initialize the ggplot object, if `NULL`, no plot area will be added.  we
    # must separate this method with draw method, since other ggplot elements
    # will be added for this plot.
    ggplot = function(self) NULL,

    # Following methods will be executed when building plot with the final
    # heatmap layout you shouldn't modify the `Align` object when drawing,
    # since all of above process will only run once.
    draw = function(self, panels, index) self$plot
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

align_method_params <- function(f, remove = c("panels", "index")) {
    setdiff(names(ggproto_formals(f)), c("self", remove))
}
