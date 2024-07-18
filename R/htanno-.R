#' Heatmap annotation of `htanno`
#'
#' `htanno` is a special annotation, which can act with the main heatmap.
#' Especially control the order of the main heatmap or split the heatmap into
#' different slices.
#'
#' @inheritParams anno
#' @param params A list of parameters passed  to `htanno`.
#' @param check.param A single boolean value indicates whether to check the
#' supplied parameters and warn.
#' @param htanno_class A `HtannoProto` object
#' @return A `htanno` object.
htanno <- function(data = NULL,
                   params = list(),
                   position = NULL, size = NULL,
                   labels = NULL, labels_nudge = NULL,
                   set_context = NULL, order = NULL, name = NULL,
                   check.param = TRUE,
                   htanno_class = HtannoProto) {
    assert_s3_class(htanno_class, "HtannoProto")
    assert_bool(check.param)
    # always make a clone of `htanno_class`, otherwise, the params will be
    # reused across different annotation.
    htanno <- ggproto_clone(htanno_class)
    # Warn about extra params
    all <- htanno$parameters()
    if (check.param) {
        if (length(extra_param <- setdiff(names(params), all))) { # nolint
            cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
        }
    }
    call <- caller_call()
    if (anno_override_call(call)) {
        call <- current_call()
    }
    anno(
        "htanno",
        data = data, order = order, size = size,
        htanno = htanno,
        params = params[intersect(names(params), all)],
        name = name, position = position,
        set_context = set_context,
        labels = labels, labels_nudge = labels_nudge,
        call = call
    )
}

#' @include anno-.R
#' @keywords internal
methods::setClass(
    "htanno",
    contains = "anno",
    list(htanno = "ANY", params = "list"),
    prototype = list(htanno = NULL, params = list())
)

#' @section HtannoProto:
#' Each of the `Htanno*` objects is just a [ggproto()][ggplot2::ggproto] object,
#' descended from the top-level `HtannoProto`, and each implements various
#' methods and fields.
#'
#' To create a new type of `Htanno*` object, you typically will want to
#' override one or more of the following:
#'  - `setup_params`: Prepare parameter or check parameters used by this
#'    annotation.
#'  - `add`: A method used to add other components into this annotation. Usually
#'    `gg` components. Must modify the `HtannoProto` object itself, no results
#'    will be assign.
#'  - `compute`: A method used to compute statistics.
#'  - `layout`: A method used to group heamap rows/columns into panels or
#'    reorder heamtap rows/columns.
#'  - `draw`: A method used to draw the plot. You must not modify the
#'    `HtannoProto` object itself when drawing since all of calculating process
#'    will run only once when be added into the heatmap. It allows `htanno` to
#'    make layout of the heatmap when be added, and we can then check other
#'    annotation won't modifying the heatmap layout.
#' @export
#' @format NULL
#' @usage NULL
#' @rdname htanno
HtannoProto <- ggplot2::ggproto("HtannoProto",
    call = NULL,
    # each function can modify these parameters and statistics using methods
    # `compute`, `layout`, and `draw`.
    compute_params = NULL,
    layout_params = NULL,
    draw_params = NULL,
    statistics = NULL,
    parameters = function(self) {
        c(
            htanno_method_params(self$compute, 4L),
            htanno_method_params(self$layout, 4L),
            htanno_method_params(self$draw, 4L),
            self$extra_params
        )
    },
    split_params = function(self, params) {
        # Split up params between compute, layout, and draw
        self$compute_params <- params[
            intersect(names(params), htanno_method_params(self$compute, 4L))
        ]
        self$layout_params <- params[
            intersect(names(params), htanno_method_params(self$layout, 4L))
        ]
        self$draw_params <- params[
            intersect(names(params), htanno_method_params(self$draw, 4L))
        ]
    },
    # Most parameters for the `HtannoProto` are taken automatically from
    # `compute()`, `layout()` and `draw()`. However, some additional
    # parameters may be removed when setup parameters. You should put there
    # paramters here, otherwise, they won't be collected.
    extra_params = character(),
    setup_params = function(data, params, position) params,
    setup_data = function(data, params, position) data,
    # this method must modify the `HtannoProto` object itself, no results will
    # be assign.
    add = function(self, object, object_name) {
        cli::cli_abort(
            "cannot add {object_name} to {.fn {snake_class(self)}}",
            call = self$call
        )
    },

    # provide method for `&` to modify the annotation
    and = function(self, object, object_name) NULL,

    # Following fields should be defined for the new `Htanno` object.
    # argument name in the initial function doesn't matter.
    compute = function(data, panels, index, position) NULL,

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
    #    For new `HtannoProto` object, which can do clustering, we must abort,
    #    if it can not do sub-clustering, if it can do sub-clustering, we should
    #    know if we want to change the order between the groups (panel levels).
    #
    #    Please check `HtannoGroup` object and `HtannoProto` object
    #    For dendrogram, it can do sub-clustering within each group, it also
    #    allows reordering between groups (it provide `reorder_group` argument),
    #    so the new panels levels may be not the same with old panels
    #
    #    For `HtannoProto` object reordering the heatmap rows/columns.  usually
    #    we provide a `strict` argument, to allow reorder heatmap within group
    #    only. See `HtannoReorder`.
    #
    # 3. old index is not `NULL`, no matter whether old panels is `NULL` or not,
    #    in this way, we should always ensure the new index won't change the old
    #    index, this will be checked in `htanno_layout` function.
    layout = function(data, panels, index, position) {
        list(panels, index)
    },

    # Used to modify `HtannoProto` object after layout has been applied, but
    # before rendering. The default is to not modify anything. Use this hook to
    # add initial ggplot elements but want to let user override it.
    finish_layout = function(data, panels, index, position) NULL,

    # draw plot, you cannot modify `HtannoProto` object when drawing, since all
    # of above process will only run once
    draw = function(data, panels, index, position) {
        NULL
    }
)

ggproto_formals <- function(x) formals(environment(x)$f)

htanno_method_params <- function(f, n) {
    params <- setdiff(names(ggproto_formals(f)), "self")
    params[-seq_len(n)]
}
