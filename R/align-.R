#' Create a New `align` Object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' An `AlignDiscrete` object interacts with the `Layout` object to reorder or
#' split observations and, in some cases, add plot components to the `Layout`.
#'
#' @param align An `AlignDiscrete` object.
#' @param ... Additional fields passed to the `align` object.
#' @param params A list of parameters for `align`.
#' @param plot A ggplot object.
#' @inheritParams ggalign
#' @param schemes Options for `schemes`:
#'  - `NULL`: Used when `align` do not add a plot.
#'  - [`waiver()`][ggplot2::waiver]: Try to infer `schemes` based on `data`.
#' @param check.param Logical; if `TRUE`, checks parameters and provides
#'   warnings as necessary.
#' @param call The `call` used to construct the `Align` object, for reporting
#'   messages.
#'
#' @section Discrete Axis Alignment:
#' It is important to note that we consider rows as observations, meaning
#' `vec_size(data)`/`NROW(data)` must match the number of observations along the
#' axis used for alignment (x-axis for a vertical stack layout, y-axis for a
#' horizontal stack layout).
#'
#' @return A new `ggalign_plot` object.
#' @examples
#' align_dendro()
#' @importFrom rlang caller_call current_call
#' @importFrom ggplot2 ggproto
#' @export
#' @keywords internal
align_discrete <- function(align, data, ...,
                           params = list(), plot = NULL,
                           size = NULL, schemes = NULL, no_axes = NULL,
                           active = NULL, check.param = TRUE,
                           call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }

    # check arguments ---------------------------------------------
    data <- allow_lambda(data)
    no_axes <- no_axes %||%
        getOption(sprintf("%s.align_no_axes", pkg_nm()), default = TRUE)
    schemes <- schemes %|w|% default_schemes(data)

    # Warn about extra params or missing parameters ---------------
    all <- align$parameters()
    input <- names(params) %||% character()
    if (isTRUE(check.param)) {
        if (length(extra_param <- vec_set_difference(input, all))) { # nolint
            cli_warn("Ignoring unknown parameters: {.arg {extra_param}}")
        }
        if (length(missing <- vec_set_difference(all, input))) { # nolint
            cli_abort("missing parameters: {missing}", call = call)
        }
    }
    input_params <- params[vec_set_intersect(input, all)]
    new_ggalign_plot(
        align = align,

        # additional field for `align` object
        no_axes = no_axes,
        ...,

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
        input_params = input_params,

        # object slots
        plot = plot,
        active = active,
        size = size,
        schemes = schemes,

        # call
        call = call
    )
}

#' @export
summary.AlignDiscrete <- function(object, ...) {
    # we always push user define summary method
    # since `Align` object should reorder observations or split observations
    # into groups
    cli_abort(sprintf(
        "You must define {.fn summary} method for {.cls %s}",
        .subset(class(object), 1L)
    ))
}

#' @details
#' Each of the `Align*` objects is just a [`ggproto()`][ggplot2::ggproto]
#' object, descended from the top-level `AlignDiscrete`, and each implements
#' various methods and fields.
#'
#' To create a new type of `Align*` object, you typically will want to
#' override one or more of the following:
#'  - `setup_params`: Prepare parameter or check parameters used by this plot.
#'  - `setup_data`: Prepare data used by this plot.
#'  - `compute`: A method used to compute statistics.
#'  - `align`: A method used to group observations into panel or reorder
#'    observations.
#'  - `draw`: A method used to draw the plot. Must return a `ggplot` object.
#' @importFrom ggplot2 ggproto
#' @export
#' @format NULL
#' @usage NULL
#' @rdname align_discrete
#' @include plot-.R
AlignDiscrete <- ggproto("AlignDiscrete", AlignProto,
    parameters = function(self) {
        c(
            align_method_params(
                self$compute,
                align_method_params(AlignDiscrete$compute)
            ),
            align_method_params(
                self$align,
                align_method_params(AlignDiscrete$align)
            ),
            self$extra_params
        )
    },
    setup_design = function(self, layout_data, design) {
        object_name <- .subset2(self, "object_name")
        layout_name <- .subset2(self, "layout_name")
        # check plot is compatible with the layout
        if (is_continuous_design(design)) {
            # `AlignDiscrete` object is special for discrete variables
            cli_abort(c(
                sprintf("Cannot add {.var {object_name}} to %s", layout_name),
                i = sprintf("%s cannot align discrete variables", layout_name)
            ))
        }

        call <- .subset2(self, "call")
        input_data <- .subset2(self, "input_data")
        input_params <- .subset2(self, "input_params")
        layout_panel <- .subset2(design, "panel")
        layout_index <- .subset2(design, "index")
        layout_nobs <- .subset2(design, "nobs")

        # we must have the same observations across all plots
        # 1. if `Align` require data, the `nobs` should be nrow(data)
        # 2. if not, we run `nobs()` method to initialize the layout nobs
        if (!is.null(input_data)) { # this `Align` object require data
            if (is.waive(input_data)) { # inherit from the layout
                if (is.null(data <- layout_data)) {
                    cli_abort(c(
                        "you must provide {.arg data} in {.var {object_name}}",
                        i = sprintf("no data was found in %s", layout_name)
                    ))
                }
            } else {
                if (is.function(input_data)) {
                    if (is.null(layout_data)) {
                        cli_abort(c(
                            "{.arg data} in {.var {object_name}} cannot be a function",
                            i = sprintf("no data was found in %s", layout_name)
                        ))
                    }
                    data <- input_data(layout_data)
                } else {
                    data <- input_data
                }
            }
            # we always regard rows as the observations
            if (is.null(layout_nobs)) {
                layout_nobs <- NROW(data)
            } else if (NROW(data) != layout_nobs) {
                cli_abort(sprintf(
                    "{.var %s} (nobs: %d) is not compatible with the %s (nobs: %d)",
                    object_name, NROW(data), layout_name, layout_nobs
                ))
            }
            # save the labels
            self$labels <- vec_names(data) %||% vec_names(layout_data)
            params <- self$setup_params(layout_nobs, input_params)
            self$data <- ggalign_attr_restore(
                self$setup_data(params, data),
                layout_data
            )
        } else { # this `Align` object doesn't require any data
            # we keep the names from the layout data for usage
            self$labels <- vec_names(layout_data)
            # If `nobs` is `NULL`, it means we don't initialize the layout
            # observations, we initialize `nobs` with the `Align` obect
            if (is.null(layout_nobs)) layout_nobs <- self$nobs(input_params)
            params <- self$setup_params(layout_nobs, input_params)
        }

        # save the parameters into the object ------------
        self$params <- params

        # prepare the data -------------------------------
        # compute statistics ---------------------------------
        self$statistics <- align_inject(
            self$compute,
            c(list(panel = layout_panel, index = layout_index), params)
        )

        # make the new layout -------------------------------
        new_design <- align_inject(
            self$align,
            c(list(panel = layout_panel, index = layout_index), params)
        )

        # we check the the design
        check_discrete_design(
            design,
            discrete_design(
                .subset2(new_design, 1L),
                .subset2(new_design, 2L),
                layout_nobs
            ),
            layout_name,
            object_name
        )
    },

    # Most parameters for the `Align` are taken automatically from `compute()`,
    # `align()` and `build_plot()`. However, some additional parameters may be
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
    align = function(self, panel, index) list(panel, index),

    # let AlignProto to add schemes and theme acoordingly
    finish_plot = function(self, plot, schemes, theme) {
        ggproto_parent(AlignGg, self)$finish_plot(plot, schemes, theme)
    }
)
