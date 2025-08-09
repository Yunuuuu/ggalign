#' Create a New `CraftBox` Object with `CraftAlign` craftsman
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' An `CraftAlign` object interacts with the `Layout` object to reorder or split
#' observations and, in some cases, add plot components to the `Layout`.
#'
#' @param align An `CraftAlign` object.
#' @param ... Additional fields passed to the `align` object.
#' @param plot A ggplot object.
#' @inheritParams ggalign
#' @param schemes Options for `schemes`:
#'  - `NULL`: Used when `align` do not add a plot.
#'  - [`waiver()`][ggplot2::waiver]: Try to infer `schemes` based on `data`.
#' @param call The `call` used to construct the `align` object, for
#'   reporting messages.
#'
#' @section Discrete Axis Alignment:
#' It is important to note that we consider rows as observations, meaning
#' `vec_size(data)`/`NROW(data)` must match the number of observations along the
#' axis used for alignment (x-axis for a vertical stack layout, y-axis for a
#' horizontal stack layout).
#'
#' @return A new `CraftBox` object.
#' @examples
#' align_dendro()
#' @importFrom rlang caller_call current_call
#' @importFrom ggplot2 ggproto
#' @export
#' @keywords internal
align <- function(align, data = NULL, ..., plot = NULL,
                  size = NULL, schemes = NULL,
                  active = NULL, no_axes = deprecated(), call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }

    # check arguments ---------------------------------------------
    data <- allow_lambda(data)
    if (lifecycle::is_present(no_axes)) {
        lifecycle::deprecate_stop(
            "1.0.3",
            "align(no_axes = )",
            details = "Please add `theme()` to the ggplot instead"
        )
    }
    schemes <- schemes %|w|% default_schemes(data)

    new_craftbox(
        craftsman = align,

        # additional field for `align` object
        ...,

        # Following fields will be initialzed when added into the layout
        # and will be saved and accessed across the plot rendering process
        direction = NULL,
        position = NULL,
        data = NULL, # Used to save the modified `input_data`
        statistics = NULL, # `$compute` method
        labels = NULL, # the original `vec_names()` of the `input_data`

        # the input data
        input_data = data,

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
#' object, descended from the top-level `CraftAlign`, and each implements
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
#' @include craftsman.R
CraftAlign <- ggproto("CraftAlign", Craftsman,
    interact_layout = function(self, layout) {
        # check plot is compatible with the layout
        if (is_layout_continuous(layout)) {
            layout_name <- self$layout_name
            # `CraftAlign` object is special for discrete variables
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name(self), layout_name),
                i = sprintf("%s cannot align discrete variables", layout_name)
            ))
        }
        layout
    },
    setup_domain = function(self, domain) {
        old_panel <- prop(domain, "panel")
        old_index <- prop(domain, "index")
        # prepare the data -------------------------------
        # compute statistics ---------------------------------
        self$statistics <- self$compute(panel = old_panel, index = old_index)

        # make the new layout -------------------------------
        panel_and_index <- self$align(panel = old_panel, index = old_index)

        # check panel
        layout_name <- self$layout_name
        nobs <- prop(domain, "nobs")
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
            } else if (is.na(nobs)) {
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
            } else if (is.na(nobs)) {
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
        DiscreteDomain(panel, index, nobs)
    },

    # Following fields should be defined for the new `CraftAlign` object.
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
    # See `$setup_domain()` method for details
    # There will have following situations (the input is old index and old
    # panel):
    #
    # 1. old index is NULL and old panel is NULL, there is nothing wrong to
    #    define any new index or panel
    # 2. old index is `NULL` and old panel is not `NULL`, in this way, new
    #    index must follow the old panel.
    #
    #    For new `CraftAlign` object, which can do clustering, we must
    #    abort, if it can not do sub-clustering, if it can do sub-clustering, we
    #    should know if we want to change the order between the groups (panel
    #    levels).
    #
    #    Please check `AlignGroup` object and `CraftAlign` object
    #    For dendrogram, it can do sub-clustering within each group, it also
    #    allows reordering between groups (it provide `reorder_group` argument),
    #    so the new panel levels may be not the same with old panel
    #
    #    For `CraftAlign` object reordering the heatmap rows/columns.
    #    usually we provide a `strict` argument, to allow reorder heatmap within
    #    group only. See `AlignOrder2`.
    #
    # 3. old index is not `NULL`, no matter whether old panel is `NULL` or not,
    #    in this way, we should always ensure the new index won't change the old
    #    index, this will be checked in `$setup_domain()` method.
    align = function(self, panel, index) list(panel, index),

    # ========== Stack Layout ==========
    # Setup facet for stack layout
    # It's important we also set `drop = FALSE`, otherwise, plot is hard to
    # align, by convention, we always set `as.table = FALSE` to follow usual
    # ggplot2 coordinate direction
    setup_stack_facet = function(self, plot, domain, ...) {
        align_stack_discrete_facet(
            self$direction, plot, domain,
            self$layout_name
        )
    },

    # ========== Circle Layout ==========
    # Setup facet for circle layout
    # It's important we also set `drop = FALSE`, otherwise, plot is hard to
    # align
    setup_circle_facet = function(self, plot, domain, sector_spacing, ...) {
        align_circle_discrete_facet(
            plot, domain, sector_spacing,
            self$layout_name
        )
    },

    # let Craftsman to add schemes and theme acoordingly
    finish_plot = function(self, plot, schemes, theme) {
        ggproto_parent(AlignGg, self)$finish_plot(plot, schemes, theme)
    },
    summary = function(self, plot) {
        header <- ggproto_parent(Craftsman, self)$summary(plot)
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
