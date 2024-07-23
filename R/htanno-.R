#' Create a ggheatmap annotation object
#'
#' @param data A matrix, a data frame, or even a simple vector that will be
#' converted into a one-column matrix. If the `data` argument is set to `NULL`,
#' the function will use the heatmap matrix. Additionally, the `data` argument
#' can also accept a function (purrr-like lambda is also okay), which will be
#' applied with the heatmap matrix.
#'
#' It is important to note that all annotations consider the `rows` as the
#' observations. It means the `NROW` function must return the same number with
#' the heatmap parallel axis. So for column annotation, the heatmap matrix will
#' be transposed before using (If data is a `function`, it will be applied with
#' the transposed matrix).
#'
#' @param position A string of the annotation position, possible values are
#' `"top"`, `"left"`, `"bottom"`, and `"right"`. If `NULL`, the active context
#' of `ggheatmap` will be used.
#' @param size Annotation size, can be a [unit][grid::unit] object.
#' @param labels Labels for axis parallelly with heatmap, the default will use
#' the rownames of the `data`. One of:
#'   - `NULL` for no labels
#'   - `waiver()` for the default labels
#'   - A character vector giving labels (must be same length as the heatmap
#'     axis)
#'   - An expression vector (must be the same length as heatmap axis). See
#'     `?plotmath` for details.
#'   - A function that takes the default labels as the input and returns labels
#'     as output. Also accepts rlang [lambda][rlang::as_function()] function
#'     notation.
#' @param labels_nudge A single numeric or a numeric value of length
#' `nrow(data)`, to nudge each text label away from the center. One of:
#'   - `NULL` for no breaks
#'   - `waiver()`: if `labels` is `NULL`, then `labels_nudge` will be `NULL`,
#'     otherwise it will inherit from the heatmap.
#'   - A numeric.
#' @param set_context A logical value of length `2` indicates whether to set the
#' active context to the `position` for the [ggheatmap][ggheat] and whether to
#' set the active context to current annotation for the annotation list in
#' [ggheatmap][ggheat] when added.
#' @param order Annotation order, must be an single integer.
#' @param name A string of the annotation name.
#' @param check.param A single boolean value indicates whether to check the
#' supplied parameters and warn.
#' @param call The `call` used to construct the scale for reporting messages.
#' @return A new `Class` object.
#' @importFrom rlang caller_call current_call
#' @export
#' @keywords internal
htanno <- function(htanno_class, params, position = NULL,
                   mapping = NULL, labels = NULL, labels_nudge = waiver(),
                   size = NULL, data = NULL,
                   set_context = NULL, order = NULL, name = NULL,
                   check.param = TRUE, call = caller_call()) {
    call <- call %||% current_call()
    # check arguments ---------------------------------------------
    position <- match_context(position)
    size <- set_size(size)
    if (!is_scalar(size) || !is.unit(size)) {
        cli::cli_abort("{.arg size} must be a single {.cls unit} object.",
            call = call
        )
    }
    data <- allow_lambda(data)
    if (is.null(set_context)) {
        set_context <- TRUE
    } else if (!is.logical(set_context)) {
        cli::cli_abort("{.arg set_context} must be a logical value",
            call = call
        )
    }
    set_context <- rep_len(set_context, 2L)

    if (is.null(order)) {
        order <- NA_integer_
    } else if (!is_scalar(order)) {
        cli::cli_abort("{.arg order} must be a single number", call = call)
    } else if (is.numeric(order)) {
        order <- as.integer(order)
    }
    if (!is.null(name) && (!is_string(name) || name == "")) {
        cli::cli_abort(
            "{.arg name} must be a single non-empty string",
            call = call
        )
    }

    # Warn about extra params or missing parameters ---------------
    all <- htanno_class$parameters()
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
        htanno_class,
        isLock = FALSE,
        statistics = NULL,
        plot = NULL,
        data = data,
        position = position,
        size = size,
        name = name,
        order = order,
        set_context = set_context,
        params = params[intersect(names(params), all)], # collect parameters
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
htanno_override_call <- function(call = NULL) {
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
plot.htanno <- function(x, ...) {
    cli::cli_abort("You cannot plot {.cls {fclass(x)}} object directly")
}

#' @section HtannoProto:
#' Each of the `Htanno*` objects is just a [ggproto()][ggplot2::ggproto] object,
#' descended from the top-level `HtannoProto`, and each implements various
#' methods and fields.
#'
#' To create a new type of `Htanno*` object, you typically will want to
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
#' @rdname htanno
HtannoProto <- ggplot2::ggproto("HtannoProto",

    # prepare parameters
    parameters = function(self) {
        c(
            htanno_method_params(self$compute),
            htanno_method_params(self$layout),
            htanno_method_params(self$ggplot),
            htanno_method_params(self$draw),
            self$extra_params
        )
    },
    split_params = function(self, params) {
        # Split up params between compute, layout, and draw
        self$compute_params <- params[
            intersect(names(params), htanno_method_params(self$compute))
        ]
        self$layout_params <- params[
            intersect(names(params), htanno_method_params(self$layout))
        ]
    },
    lock = function(self) {
        assign("isLock", value = TRUE, envir = self)
    },
    unlock = function(self) {
        assign("isLock", value = FALSE, envir = self)
    },

    # Most parameters for the `HtannoProto` are taken automatically from
    # `compute()`, `layout()` and `draw()`. However, some additional parameters
    # may be removed when setup parameters. You should put these paramters here,
    # otherwise, they won't be collected.
    extra_params = character(),
    setup_params = function(self) .subset2(self, "params"),

    # use `NULL`, if you don't need any data from the heatmap
    # use `.subset2(self, "data")` if you want to attach the matrix
    setup_data = function(self) NULL,

    # Following fields should be defined for the new `Htanno` object.
    # argument name in the function doesn't matter.
    compute = function(panels, index) NULL,

    # Group heamap row/column and reorder, Must return a list of 2:
    #  - the first one should be the groups for heatmap row/column, the factor
    #    levels will determine the panel order, so it should always follow the
    #    index if you don't want the panel levels break the index. See
    #    `HtannoDendro` for example.
    #  - the second one should be the heatmap row/column order index, and will
    #    determine the order in each grouped panels.
    #
    # See `htanno_layout` function for details
    # There will have following situations (the input is old index and old
    # panels):
    #
    # 1. old index is NULL and old panels is NULL, there is nothing wrong to
    #    define any new index or panels
    # 2. old index is `NULL` and old panels is not `NULL`, in this way, new
    #    index must follow the old panels.
    #
    #    For new `HtannoProto` object, which can do clustering, we must
    #    abort, if it can not do sub-clustering, if it can do sub-clustering, we
    #    should know if we want to change the order between the groups (panel
    #    levels).
    #
    #    Please check `HtannoGroup` object and `HtannoProto` object
    #    For dendrogram, it can do sub-clustering within each group, it also
    #    allows reordering between groups (it provide `reorder_group` argument),
    #    so the new panels levels may be not the same with old panels
    #
    #    For `HtannoProto` object reordering the heatmap rows/columns.
    #    usually we provide a `strict` argument, to allow reorder heatmap within
    #    group only. See `HtannoReorder`.
    #
    # 3. old index is not `NULL`, no matter whether old panels is `NULL` or not,
    #    in this way, we should always ensure the new index won't change the old
    #    index, this will be checked in `htanno_layout` function.
    layout = function(panels, index) list(panels, index),

    # initialize the ggplot object, if `NULL`, no plot area will be added.
    ggplot = function(self, panels, index) NULL,

    # Following methods will be executed when building plot with the final
    # heatmap layout you shouldn't modify the `HtannoProto` object when drawing,
    # since all of above process will only run once.
    draw = function(self, panels, index) self$plot
)

# Used to lock the `HtannoProto` object
#' @export
`$<-.HtannoProto` <- function(x, name, value) {
    if (.subset2(x, "isLock")) {
        cli::cli_abort("{.fn {snake_class(x)}} is locked",
            call = .subset2(x, "call")
        )
    }
    assign(x = name, value = value, envir = x)
    invisible(x)
}

ggproto_formals <- function(x) formals(environment(x)$f)

htanno_method_params <- function(f, n) {
    setdiff(names(ggproto_formals(f)), c("self", "panels", "index"))
}
