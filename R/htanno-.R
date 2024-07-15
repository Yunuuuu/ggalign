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
                   set_context = NULL, order = NULL, name = NULL,
                   check.param = TRUE,
                   htanno_class = HtannoProto) {
    assert_s3_class(htanno_class, "HtannoProto")
    assert_bool(check.param)
    # Warn about extra params
    all <- htanno_class$parameters()
    if (check.param &&
        length(extra_param <- setdiff(names(params), all))) { # nolint
        cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
    }
    anno(
        "htanno",
        data = data, order = order, size = size, htanno = htanno_class,
        params = params[intersect(names(params), all)],
        # following attributes were used by `ggheatmap_add`
        name = name, position = position,
        set_context = set_context
    )
}

#' @include anno-.R
#' @keywords internal
methods::setClass(
    "htanno",
    contains = "anno",
    list(htanno = "ANY", statistics = "ANY", params = "list"),
    prototype = list(statistics = NULL, params = list())
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
#'    `gg` components.
#'  - `compute`: A method used to compute statistics.
#'  - `layout`: A method used to group heamap rows/columns into panels or
#'    reorder heamtap rows/columns.
#'  - `draw`: A method used to draw the plot.
#' @export
#' @usage NULL
#' @rdname htanno
HtannoProto <- ggplot2::ggproto("HtannoProto",
    compute_params = NULL,
    layout_params = NULL,
    draw_params = NULL,
    parameters = function(self) {
        c(
            htanno_method_params(self$compute, 2L),
            htanno_method_params(self$layout, 5L),
            htanno_method_params(self$draw, 5L),
            self$extra_params
        )
    },
    # Most parameters for the `HtannoProto` are taken automatically from
    # `compute()`, `layout()` and `draw()`. However, some additional
    # parameters may be removed when setup parameters. You should put there
    # paramters here, otherwise, they won't be collected.
    extra_params = character(),
    setup_params = function(data, params, position) params,
    split_params = function(self, params) {
        # Split up params between compute, layout, and draw
        self$compute_params <- params[
            intersect(names(params), htanno_method_params(self$compute, 2L))
        ]
        self$layout_params <- params[
            intersect(names(params), htanno_method_params(self$layout, 5L))
        ]
        self$draw_params <- params[
            intersect(names(params), htanno_method_params(self$draw, 5L))
        ]
    },
    # this method must modify the HtannoProto object itself, no results will be
    # assign.
    add = function(self, object, object_name) {
        cli::cli_abort("cannot add {object_name} to {.fn {snake_class(self)}}")
    },

    # Following fields should be defined for the new Htanno object.
    # argument name in the initial function doesn't matter.
    compute = function(data, position) NULL,

    # Group heamap row/column and reorder, Must return a list of 2:
    #  - the first one should be the groups for heatmap row/column, the factor
    #    levels will determine the panel order, so it should always follow the
    #    index if you don't want the panel levels break the index. See
    #    `HtannoDendro` for example.
    #  - the second one should be the heatmap row/column order index, and will
    #    determine the order in each grouped panels.
    layout = function(data, statistics, panels, index, position) {
        list(panels, index)
    },

    # draw plot
    draw = function(data, statistics, panels, index, position) NULL
)

ggproto_formals <- function(x) formals(environment(x)$f)

htanno_method_params <- function(f, n) {
    params <- setdiff(names(ggproto_formals(f)), "self")
    params[-seq_len(n)]
}
