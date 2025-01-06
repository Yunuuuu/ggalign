#' Create a New `align` Object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' An `Align` object interacts with the `Layout` object to reorder or
#' split observations and, in some cases, add plot components to the `Layout`.
#'
#' @param align An `Align` object.
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
align <- function(align, data, ...,
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

#' @details
#' Each of the `Align*` objects is just a [`ggproto()`][ggplot2::ggproto]
#' object, descended from the top-level `Align`, and each implements
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
#' @rdname align
#' @include plot-.R
Align <- ggproto("Align", AlignProto,
    parameters = function(self) {
        c(
            align_method_params(
                self$compute,
                align_method_params(Align$compute)
            ),
            align_method_params(
                self$align,
                align_method_params(Align$align)
            ),
            self$extra_params
        )
    },
    interact_layout = function(self, layout) {
        layout_name <- self$layout_name
        object_name <- object_name(self)
        # check plot is compatible with the layout
        if (is_layout_continuous(layout)) {
            # `Align` object is special for discrete variables
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name, layout_name),
                i = sprintf("%s cannot align discrete variables", layout_name)
            ))
        }
        input_data <- self$input_data
        input_params <- self$input_params
        layout_data <- layout@data
        design <- layout@design
        layout_nobs <- .subset2(design, "nobs")
        # we must have the same observations across all plots
        # 1. if `Align` require data, the `nobs` should be nrow(data)
        # 2. if not, we run `nobs()` method to initialize the layout nobs
        if (!is.null(input_data)) { # this `Align` object require data
            if (is.waive(input_data)) { # inherit from the layout
                if (is.null(data <- layout_data)) {
                    cli_abort(c(
                        sprintf(
                            "you must provide {.arg data} in %s",
                            object_name
                        ),
                        i = sprintf("no data was found in %s", layout_name)
                    ))
                }
            } else {
                if (is.function(input_data)) {
                    if (is.null(layout_data)) {
                        cli_abort(c(
                            sprintf(
                                "{.arg data} in %s cannot be a function",
                                object_name
                            ),
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
                if (layout_nobs == 0L) {
                    cli_abort("{.arg data} cannot be empty", call = self$call)
                }
            } else if (NROW(data) != layout_nobs) {
                cli_abort(sprintf(
                    "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
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
            if (is.null(layout_nobs)) {
                layout_nobs <- self$nobs(input_params)
                if (!(is_scalar(layout_nobs) && is.integer(layout_nobs))) {
                    cli_abort(c(
                        sprintf(
                            "invalid {.field nobs} defined by %s",
                            object_name(self)
                        ),
                        i = "{.field nobs} must be a single integer"
                    ))
                }
            }
            params <- self$setup_params(layout_nobs, input_params)
        }
        design["nobs"] <- list(layout_nobs)
        layout@design <- design

        # save the parameters into the object ------------
        self$params <- params
        layout
    },
    setup_design = function(self, design) {
        old_panel <- .subset2(design, "panel")
        old_index <- .subset2(design, "index")
        # prepare the data -------------------------------
        # compute statistics ---------------------------------
        self$statistics <- align_inject(
            self$compute,
            c(list(panel = old_panel, index = old_index), self$params)
        )

        # make the new layout -------------------------------
        panel_and_index <- align_inject(
            self$align,
            c(list(panel = old_panel, index = old_index), self$params)
        )

        # check panel
        layout_name <- self$layout_name
        nobs <- .subset2(design, "nobs")
        new_panel <- .subset2(panel_and_index, 1L)
        if (!is.null(new_panel)) {
            if (!is.atomic(new_panel)) {
                cli_abort(c(
                    sprintf(
                        "invalid layout panels defined by %s",
                        object_name(self)
                    ),
                    i = "layout panels must be an atomic vector"
                ))
            } else if (anyNA(new_panel)) {
                cli_abort(sprintf(
                    "layout panels defined by %s contain `NA`",
                    object_name(self)
                ))
            } else if (is.null(nobs)) {
                # we have defined panel, but don't define the `nobs`
                cli_abort(sprintf(
                    "%s defined the panels but not define {.field nobs}", object_name(self)
                ))
            } else if (length(new_panel) != nobs) {
                # we have defined panel, but don't define the `nobs`
                cli_abort(sprintf(
                    "layout panels defined by %s (nobs: %d) is not compatible with the nobs: %d",
                    object_name(self), length(new_panel), nobs
                ))
            } else if (!is.null(old_panel) && !(new_panel %nest% old_panel)) {
                cli_abort(sprintf(
                    "%s disrupt the previously established panel groups of %s",
                    object_name(self), layout_name
                ))
            }
        } else if (!is.null(old_panel)) {
            # push developer to reset the panel in the layout
            cli_abort(c(
                sprintf("invalid %s", object_name(self)),
                i = sprintf(
                    "%s reset the {.field panel}, but don't change the {.field panel} of the layout", object_name(self)
                )
            ))
        }
        panel <- new_panel
        if (!is.null(panel) && !is.factor(panel)) panel <- factor(panel)

        # check index
        new_index <- .subset2(panel_and_index, 2L)
        if (!is.null(new_index)) {
            if (!is.integer(new_index)) {
                cli_abort(c(
                    sprintf(
                        "invalid layout ordering index defined by %s", object_name(self)
                    ),
                    i = "layout ordering index must be an integer"
                ))
            } else if (anyNA(new_index)) {
                cli_abort(sprintf(
                    "layout ordering index defined by %s contain `NA`",
                    object_name(self)
                ))
            } else if (is.null(nobs)) {
                # we have defined panel, but don't define the `nobs`
                cli_abort(sprintf(
                    "%s defined the ordering index but not define nobs", object_name(self)
                ))
            } else if (length(new_index) != nobs) {
                # we have defined index, but don't define the `nobs`
                cli_abort(sprintf(
                    "layout ordering index defined by %s (nobs: %d) is not compatible with the nobs (%d)",
                    object_name(self), length(new_index), nobs
                ))
            }
        } else if (!is.null(old_index)) {
            # push developer to reset the `index` in the layout
            cli_abort(c(
                sprintf("invalid %s", object_name(self)),
                i = sprintf(
                    "%s reset the {.field index}, but don't change the {.field index} of the layout",
                    object_name(self)
                )
            ))
        }
        index <- new_index

        # we always make the index following the panel
        if (!is.null(panel) && !is.null(index)) {
            index <- reorder_index(panel, index)
        }

        # we always prevent from reordering twice.
        if (!is.null(old_index) && !all(old_index == index)) {
            cli_abort(sprintf(
                "%s disrupt the previously established ordering index of %s",
                object_name(self), layout_name
            ))
        }
        discrete_design(panel, index, nobs)
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
    },
    summary = function(self, plot) {
        header <- ggproto_parent(AlignProto, self)$summary(plot)
        oo <- self$summary_align()
        nms <- c("plot", "reorder", "split")
        content <- c(
            if (is.null(plot)) "no" else "yes",
            if (isTRUE(oo[1L])) "yes" else "no",
            if (isTRUE(oo[2L])) "yes" else "no"
        )
        nms <- format(nms, justify = "right")
        content <- format(content, justify = "left")
        content <- paste0("  ", nms, ": ", content)
        c(header, content)
    },

    # Summary the action of `Align`
    #
    # @return A logical vector of length 2, indicating:
    # - Whether the object reorders the observations.
    # - Whether the object splits the observations into groups.
    # @keywords internal
    summary_align = function(self) c(FALSE, FALSE)
)
