#' Build `htanno` object
#'
#' `htanno` is a special annotation, which can act with the main heatmap.
#' Especially control the order of the main heatmap or split the heatmap into
#' different slices.
#' @param htanno A `HtannoProto` object
#' @inheritParams new_anno
#' @param params A list of parameters passed  to `htanno`.
#' @param check.param A boolean value indicates whether to check the supplied
#' parameters and warn.
#' @return A `htanno` object.
htanno <- function(htanno, data = NULL,
                   position = NULL,
                   params = list(),
                   size = unit(10, "mm"),
                   active = NULL, name = NULL, order = NULL,
                   check.param = TRUE) {
    assert_s3_class(htanno, "HtannoProto")
    assert_bool(check.param)
    # Warn about extra params and aesthetics
    all <- htanno$parameters()
    if (check.param &&
        length(extra_param <- setdiff(names(params), all))) { # nolint
        cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
    }
    new_anno(
        "htanno",
        data = data, order = order, size = size, htanno = htanno,
        params = params[intersect(names(params), all)],
        # following attributes were used by `ggheatmap_add`
        name = name, position = position,
        active = active
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

#' @export
methods::setMethod("show", "htanno", function(object) {
    print("A htanno object")
    invisible(object)
})

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
    add_gg = function(gg, object_name) {
        cli::cli_abort(
            "{.fn {snake_class(self)}} cannot add {.cls ggplot2} elements"
        )
    },

    # Following fields should be defined for the new Htanno object.
    # argument name in the initial function doesn't matter.
    compute = function(data, position) NULL,

    # group heamap row/column
    make_panels = function(data, statistics, panels, position) panels,

    # reorder heatmap row/column
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
