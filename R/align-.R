#' Create a New `align` Object
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' An `Align` object interacts with the `Layout` object to reorder or split
#' observations and, in some cases, add plot components to the `Layout`.
#'
#' @param align An `Align` object.
#' @param data Options for `data`:
#'  - A matrix, data frame, or atomic vector.
#'  - [`waiver()`][ggplot2::waiver]: Uses the `layout matrix`.
#'  - `NULL`: No data is set.
#'  - A `function` (including purrr-like lambda syntax) applied to the layout
#'    `matrix`.
#' @param params A list of parameters for `align`.
#' @param plot A ggplot object.
#'
#' @param size The relative size of the plot, can be specified as a
#' [`unit`][grid::unit].
#' @param schemes Options for `schemes`:
#'  - `NULL`: Used when `align` do not add a plot.
#'  - [`waiver()`][ggplot2::waiver]: Try to infer `schemes` based on `data`.
#'
#' @param limits Logical; if `TRUE`, sets layout limits for the plot.
#' @param facet Logical; if `TRUE`, applies facets to the layout. If `FALSE`,
#'   `limits` will also be set to `FALSE`.
#' @param no_axes `r lifecycle::badge('experimental')` Logical; if `TRUE`,
#'   removes axes elements for the alignment axis using [`theme_no_axes()`]. By
#'   default, will use the option-
#'   `r code_quote(sprintf("%s.align_no_axes", pkg_nm()))`.
#' @param active A [`active()`] object that defines the context settings when
#'   added to a layout.
#' @param free_guides `r lifecycle::badge("superseded")` Please use
#'   [`scheme_align()`] function instead.
#' @param free_spaces `r lifecycle::badge("deprecated")` Please use
#' [`scheme_align()`] function instead.
#' @param plot_data `r lifecycle::badge("deprecated")` Please use
#' [`scheme_data()`] function instead.
#' @param theme `r lifecycle::badge("deprecated")` Please use
#' [`scheme_theme()`] function instead.
#' @param free_labs `r lifecycle::badge("deprecated")` Please use
#' [`scheme_align()`] function instead.
#' @param check.param Logical; if `TRUE`, checks parameters and provides
#'   warnings as necessary.
#' @param call The `call` used to construct the `Align` object, for reporting
#'   messages.
#'
#' @section Axis Alignment for Observations:
#' It is important to note that we consider rows as observations, meaning
#' `vec_size(data)`/`NROW(data)` must match the number of observations along the
#' axis used for alignment (x-axis for a vertical stack layout, y-axis for a
#' horizontal stack layout).
#'
#' @return A new `align` object.
#'
#' @examples
#' align_gg()
#' align_dendro()
#'
#' @importFrom rlang caller_call current_call
#' @importFrom ggplot2 ggproto
#' @export
#' @keywords internal
align <- function(align, data, ...,
                  params = list(), plot = NULL,
                  size = NULL, schemes = NULL,
                  limits = TRUE, facet = TRUE, no_axes = NULL, active = NULL,
                  free_guides = deprecated(), free_spaces = deprecated(),
                  plot_data = deprecated(), theme = deprecated(),
                  free_labs = deprecated(),
                  check.param = TRUE, call = caller_call()) {
    if (override_call(call)) {
        call <- current_call()
    }
    deprecate_action(
        snake_class(align),
        plot_data = plot_data,
        theme = theme,
        free_guides = free_guides,
        free_spaces = free_spaces,
        free_labs = free_labs,
        call = call
    )

    # check arguments ---------------------------------------------
    data <- allow_lambda(data)
    assert_bool(facet, call = call)
    assert_bool(limits, call = call)
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

        # new fields
        facet = facet,
        limits = limits,

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

is_align_plot <- function(x) is_ggalign_plot(x) && is_align(x@align)

is_align <- function(x) inherits(x, "Align")

#' @export
summary.Align <- function(object, ...) {
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
#' object, descended from the top-level `Align`, and each implements various
#' methods and fields.
#'
#' To create a new type of `Align*` object, you typically will want to
#' override one or more of the following:
#'  - `setup_params`: Prepare parameter or check parameters used by this plot.
#'  - `setup_data`: Prepare data used by this plot.
#'  - `compute`: A method used to compute statistics.
#'  - `layout`: A method used to group observations into panel or reorder
#'    observations.
#'  - `draw`: A method used to draw the plot. Must return a `ggplot` object.
#' @importFrom ggplot2 ggproto
#' @export
#' @format NULL
#' @usage NULL
#' @rdname align
#' @include plot-.R
Align <- ggproto("Align", AlignProto,
    free_align = FALSE,
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
            align_method_params(
                self$draw,
                align_method_params(Align$draw)
            ),
            self$extra_params
        )
    },
    layout = function(self, layout_data, layout_coords, layout_name) {
        input_data <- .subset2(self, "input_data")
        input_params <- .subset2(self, "input_params")
        object_name <- .subset2(self, "object_name")
        call <- .subset2(self, "call")
        layout_panel <- .subset2(layout_coords, "panel")
        layout_index <- .subset2(layout_coords, "index")
        layout_nobs <- .subset2(layout_coords, "nobs")

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
        new_coords <- align_inject(
            self$align,
            c(list(panel = layout_panel, index = layout_index), params)
        )

        # we check the coords
        check_layout_coords(
            layout_coords,
            new_layout_coords(
                .subset2(new_coords, 1L),
                .subset2(new_coords, 2L),
                layout_nobs
            ),
            layout_name,
            object_name
        )
    },
    build_plot = function(self, plot, coords, extra_coords,
                          previous_coords = NULL) {
        direction <- self$direction
        panel <- .subset2(coords, "panel")
        index <- .subset2(coords, "index")
        if (is.null(extra_coords)) {
            extra_panel <- NULL
            extra_index <- NULL
        } else {
            extra_panel <- .subset2(extra_coords, "panel")
            extra_index <- .subset2(extra_coords, "index")
        }
        params <- .subset2(self, "params")
        plot <- align_inject(self$draw, c(params, list(
            plot = plot,
            panel = panel,
            index = index,
            extra_panel = extra_panel,
            extra_index = extra_index
        )))

        coords$labels <- .subset(.subset2(self, "labels"), index)
        # only when user use the internal facet, we'll setup the limits
        if (.subset2(self, "facet")) {
            # set up facets
            if (nlevels(panel) > 1L) {
                default_facet <- switch_direction(
                    direction,
                    ggplot2::facet_grid(
                        rows = ggplot2::vars(.data$.panel),
                        scales = "free_y", space = "free",
                        drop = FALSE, as.table = FALSE
                    ),
                    ggplot2::facet_grid(
                        cols = ggplot2::vars(.data$.panel),
                        scales = "free_x", space = "free",
                        drop = FALSE, as.table = FALSE
                    )
                )
            } else {
                default_facet <- facet_stack(direction = direction)
            }
            plot <- plot + align_melt_facet(default_facet, plot$facet)
            coords$limits <- .subset2(self, "limits")
        } else {
            coords$limits <- FALSE
        }

        # set limits and default scales
        plot + switch_direction(
            direction,
            coord_ggalign(y = coords),
            coord_ggalign(x = coords)
        )
    },

    # Most parameters for the `Align` are taken automatically from `compute()`,
    # `layout()` and `draw()`. However, some additional parameters may be
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

    # Following methods will be executed when building plot with the final
    # heatmap layout you shouldn't modify the `Align` object when drawing,
    # since all of above process will only run once.
    # Note: panel input will be reordered by index
    draw = function(self, plot, panel, index, extra_panel, extra_index) {
        plot
    },

    # let AlignProto to add schemes and theme acoordingly
    finish_plot = function(self, plot, schemes, theme) {
        direction <- self$direction
        # remove axis titles, text, ticks used for alignment
        if (isTRUE(self$no_axes)) {
            schemes$scheme_theme <- .subset2(schemes, "scheme_theme") +
                theme_no_axes(switch_direction(direction, "y", "x"))
        }
        plot <- plot_add_schemes(plot, schemes)
        if (is_horizontal(direction)) {
            theme <- theme(
                panel.spacing.y = calc_element("panel.spacing.y", theme)
            )
        } else {
            theme <- theme(
                panel.spacing.x = calc_element("panel.spacing.x", theme)
            )
        }
        plot + theme + theme_recycle()
    }
)
