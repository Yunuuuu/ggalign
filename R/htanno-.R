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
#' @param htanno A `HtannoProto` object
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

is.htanno <- function(x) methods::is(x, "htanno")

HtannoProto <- ggplot2::ggproto("HtannoProto",
    compute_params = NULL,
    panel_params = NULL,
    reorder_params = NULL,
    draw_params = NULL,
    parameters = function(self) {
        c(
            htanno_method_params(self$compute, 2L),
            htanno_method_params(self$make_panels, 4L),
            htanno_method_params(self$reorder, 4L),
            htanno_method_params(self$draw, 7L),
            self$extra_params
        )
    },
    extra_params = character(),
    setup_params = function(data, params, position) params,
    split_params = function(self, params) {
        # Split up params between stat, reorder, make_panels, and draw
        self$compute_params <- params[
            intersect(names(params), htanno_method_params(self$compute, 2L))
        ]
        self$panel_params <- params[
            intersect(names(params), htanno_method_params(self$make_panels, 4L))
        ]
        self$reorder_params <- params[
            intersect(names(params), htanno_method_params(self$reorder, 4L))
        ]
        self$draw_params <- params[
            intersect(names(params), htanno_method_params(self$draw, 7L))
        ]
    },

    add = function(object, object_name) {
        cli::cli_abort("cannot add {object_name} to {.fn {snake_class(self)}}")
    },

    # Following fields should be defined for the new Htanno object.
    # argument name in the initial function doesn't matter.
    compute = function(data, position) NULL,

    # group heamap row/column
    # the factor levels should always follow the index (reordr method) if you
    # don't want the panel levels break the index
    make_panels = function(data, statistics, panels, position) panels,

    # reorder heatmap row/column
    # the index will be reformat to follow the panel levels.
    reorder = function(data, statistics, panels, position) NULL,

    # draw plot
    draw = function(data, statistics, index,
                    panels, scales, facet, position) {
        NULL
    }
)

ggproto_formals <- function(x) formals(environment(x)$f)

htanno_method_params <- function(f, n) {
    params <- setdiff(names(ggproto_formals(f)), "self")
    params[-seq_len(n)]
}
