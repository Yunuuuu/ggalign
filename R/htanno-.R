#' Build `htanno` object
#' 
#' `htanno` is a special annotation, which can act with the main heatmap.
#' Especially control the order of the main heatmap or split the heatmap into
#' different slices.
#' @param htanno A `HtannoProto` object
new_htanno <- function(htanno, data = NULL,
                       position = NULL,
                       params = list(), 
                       size = unit(10, "mm"),
                       active = NULL, name = NULL, order = NULL,
                       check.param = TRUE) {
    assert_s3_class(htanno, "HtannoProto")
    # Warn about extra params and aesthetics
    all <- htanno$parameters()
    extra_param <- setdiff(names(params), all)
    if (check.param && length(extra_param)) {
        cli::cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
    }
    new_gganno(
        "htanno",
        data = data, order = order,
        size = size,
        htanno = htanno,
        params = params[intersect(names(params), all)],
        # following attributes were used by `ggheatmap_add`
        name = name, position = position, active = active %||% c(TRUE, FALSE)
    )
}

#' @include anno-.R
#' @keywords internal
methods::setClass(
    "htanno",
    contains = "gganno",
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
    slice_params = NULL,
    reorder_params = NULL,
    draw_params = NULL,
    parameters = function(self) {
        c(
            htanno_method_params(self$compute, 2L),
            htanno_method_params(self$make_slice, 4L),
            htanno_method_params(self$reorder, 4L),
            htanno_method_params(self$draw, 3L),
            self$extra_params
        )
    },
    extra_params = character(),
    setup_params = function(data, position, params) params,
    split_params = function(self, params) {
        # Split up params between stat, reorder, make_slice, and draw
        self$compute_params <- params[
            intersect(names(params), htanno_method_params(self$compute, 2L))
        ]
        self$slice_params <- params[
            intersect(names(params), htanno_method_params(self$make_slice, 4L))
        ]
        self$reorder_params <- params[
            intersect(names(params), htanno_method_params(self$reorder, 4L))
        ]
        self$draw_params <- params[
            intersect(names(params), htanno_method_params(self$draw, 3L))
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
    make_slice = function(data, position, statistics, slice) slice,
    reorder = function(data, position, statistics, slice) NULL,
    draw = function(data, position, statistics) NULL
)

ggproto_formals <- function(x) formals(environment(x)$f)

htanno_method_params <- function(f, n) {
    params <- setdiff(names(ggproto_formals(f)), "self")
    params[-seq_len(n)]
}
