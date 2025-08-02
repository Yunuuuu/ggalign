#' Arrange plots horizontally or vertically
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' If `limits` is provided, a continuous variable will be required and aligned
#' in the direction specified (`stack_continuous`). Otherwise, a discrete
#' variable will be required and aligned (`stack_discrete`).
#'
#' Several aliases are provided for convenience:
#' - `stack_vertical`: A special case of `stack_layout` that sets `direction
#'   = "v"`.
#' - `stack_horizontal`: A special case of `stack_layout` that sets `direction
#'   = "h"`.
#' - `stack_discretev`: A special case of `stack_discrete` that sets `direction
#'   = "v"`.
#' - `stack_discreteh`: A special case of `stack_discrete` that sets `direction
#'   = "h"`.
#' - `stack_continuousv()`: A special case of `stack_free` that sets `direction
#'   = "v"`.
#' - `stack_continuoush()`: A special case of `stack_free` that sets `direction
#'   = "h"`.
#'
#' For historical reasons, the following aliases are available:
#' - `stack_align` is an alias for `stack_discrete`.
#' - `stack_alignv` is an alias for `stack_discretev`.
#' - `stack_alignh` is an alias for `stack_discreteh`.
#' - `stack_free` is an alias for `stack_continuous`.
#' - `stack_freev` is an alias for `stack_continuousv`.
#' - `stack_freeh` is an alias for `stack_continuoush`.
#'
#' @param direction A string indicating the direction of the stack layout,
#' either `"h"`(`horizontal`) or `"v"`(`vertical`).
#' @param data `r rd_layout_data()`:
#'  - If `limits` is not provided, [`fortify_matrix()`] will be used to get a
#'    matrix.
#'  - If `limits` is specified, [`fortify_data_frame()`] will be used to get a
#'    data frame.
#'
#' @param ... Additional arguments passed to [`fortify_data_frame()`] or
#' [`fortify_matrix()`].
#' @param theme A [`theme()`][ggplot2::theme] object used to customize various
#' elements of the layout, including `guides`, `title`, `subtitle`, `caption`,
#' `margins`, `panel.border`, and `background`. By default, the theme will
#' inherit from the parent `layout`. It also controls the panel spacing for all
#' plots in the layout.
#'
#' @param sizes A numeric value or a [`unit`][grid::unit] object. When used for
#' the [`quad_layout()`] annotation, it must be of length `1`. When used in the
#' [`stack_layout()`] with a nested [`quad_layout()`], it should be of length
#' `3`, specifying the relative heights (for `direction = "h"`) or widths (for
#' `direction = "v"`) to be applied to the layout.
#' @param limits A [`continuous_limits()`] object specifying the left/lower
#' limit and the right/upper limit of the scale. Used to align the continuous
#' axis.
#' @return A `StackLayout` object.
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7L)
#'
#' stack_horizontal(small_mat) + align_dendro()
#'
#' # this is the same with:
#' stack_discrete("h", small_mat) + align_dendro()
#'
#' stack_discreteh(small_mat) + align_dendro()
#'
#' # For vertical layout:
#' stack_vertical(small_mat) + align_dendro()
#'
#' @export
stack_layout <- function(direction, data = NULL, ...,
                         theme = NULL, sizes = NA, limits = waiver()) {
    if (is.waive(limits)) {
        stack_discrete(
            data = data, direction = direction, ...,
            theme = theme, sizes = sizes
        )
    } else {
        stack_continuous(
            data = data, direction = direction, ...,
            theme = theme, sizes = sizes, limits = limits
        )
    }
}

#' @export
#' @rdname stack_layout
stack_horizontal <- function(data = NULL, ..., limits = waiver()) {
    stack_layout(data = data, direction = "h", limits = limits, ...)
}

#' @export
#' @rdname stack_layout
stack_vertical <- function(data = NULL, ..., limits = waiver()) {
    stack_layout(data = data, direction = "v", limits = limits, ...)
}

###################################################################
#' @export
#' @rdname stack_layout
stack_discrete <- function(direction, data = NULL, ...,
                           theme = NULL, sizes = NA) {
    UseMethod("stack_discrete", data)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_align <- stack_discrete

#' @export
#' @rdname stack_layout
stack_discretev <- function(data = NULL, ...) {
    stack_discrete(data = data, direction = "v", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_alignv <- stack_discretev

#' @export
#' @rdname stack_layout
stack_discreteh <- function(data = NULL, ...) {
    stack_discrete(data = data, direction = "h", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_alignh <- stack_discreteh

#' @export
stack_discrete.default <- function(direction, data = NULL, ...,
                                   theme = NULL, sizes = NA) {
    direction <- check_direction(direction)
    # the observations are rows, we use matrix to easily
    # reshape it into a long formated data frame for ggplot,
    # and we can easily determine the number of observations
    # from matrix
    data <- data %|w|% NULL
    data <- fortify_matrix(data = data, ...)
    schemes <- default_schemes()
    if (!is.null(data) && !is.function(data)) {
        # if we have provided data, we initialize the `nobs`
        nobs <- vec_size(data)

        # for data has dimention but one dimention is 0
        # as.matrix(data.frame(row.names = letters))
        if (nobs == 0L) {
            cli_abort("empty data is no allowed")
        }
    } else {
        nobs <- NA_integer_
    }
    new_stack_layout(
        name = "stack_discrete",
        data = data, direction = direction,
        domain = DiscreteDomain(nobs = nobs),
        schemes = schemes, theme = theme, sizes = sizes
    )
}

#' @export
stack_discrete.function <- function(direction, data = NULL, ...) {
    cli_abort(paste0(
        "{.arg data} must be a {.cls matrix}, ",
        "or an object coercible by {.fn fortify_matrix}, or a valid ",
        "{.cls matrix}-like object coercible by {.fn as.matrix}"
    ))
}

#' @export
stack_discrete.formula <- stack_discrete.function

################################################################
#' @export
#' @rdname stack_layout
stack_continuous <- function(direction, data = NULL, ..., limits = NULL,
                             theme = NULL, sizes = NA) {
    UseMethod("stack_continuous", data)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_free <- stack_continuous

#' @export
#' @rdname stack_layout
stack_continuousv <- function(data = NULL, ...) {
    stack_continuous(data = data, direction = "v", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_freev <- stack_continuousv

#' @export
#' @rdname stack_layout
stack_continuoush <- function(data = NULL, ...) {
    stack_continuous(data = data, direction = "h", ...)
}

#' @usage NULL
#' @export
#' @rdname stack_layout
stack_freeh <- stack_continuoush

#' @export
stack_continuous.default <- function(direction, data = NULL, ...,
                                     limits = NULL, theme = NULL, sizes = NA) {
    assert_limits(limits)
    direction <- check_direction(direction)
    data <- data %|w|% NULL
    data <- fortify_data_frame(data = data, ...)
    schemes <- default_schemes()
    new_stack_layout(
        name = "stack_continuous",
        data = data, direction = direction, domain = limits,
        schemes = schemes, theme = theme, sizes = sizes
    )
}

#' @export
stack_continuous.function <- function(direction, data = NULL, ...) {
    cli_abort(paste0(
        "{.arg data} must be a {.cls data.frame}, ",
        "or an object coercible by {.fn fortify_data_frame}, or a valid ",
        "{.cls data.frame}-like object coercible by {.fn as.data.frame}"
    ))
}

#' @export
stack_continuous.formula <- stack_continuous.function

#' @importFrom methods new
new_stack_layout <- function(data, direction, domain,
                             schemes = NULL, theme = NULL, sizes = NA,
                             name = NULL, call = caller_call()) {
    sizes <- check_stack_sizes(sizes, call = call)
    if (!is.null(theme)) assert_s3_class(theme, "theme", call = call)
    if (is.null(name)) {
        if (is_discrete_domain(domain)) {
            name <- "stack_discrete"
        } else {
            name <- "stack_continuous"
        }
    }
    StackLayout(
        name = name, data = data,
        direction = direction,
        theme = theme, schemes = schemes, # used by the layout
        sizes = sizes, domain = domain
    )
}

############################################################
#' Determine the active context of stack layout
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' `stack_active` is an alias for `stack_switch()`, which sets `what = NULL` by
#' default.
#'
#' @inheritParams rlang::args_dots_empty
#' @inheritParams quad_switch
#' @inheritParams stack_discrete
#' @param what What should get activated for the stack layout?
#' `r rd_chain_what()`, this is useful when the active context is a
#' [`quad_layout()`] object, where any `align_*()` will be added to the
#' [`quad_layout()`]. By removing the active context, we can add `align_*()`
#' into the [`stack_layout()`].
#' @return A `stack_switch` object which can be added to [stack_layout()].
#' @examples
#' stack_discrete("h", matrix(1:9, nrow = 3L)) +
#'     ggheatmap() +
#'     # ggheamtap will set the active context, directing following addition
#'     # into the heatmap plot area. To remove the heatmap active context,
#'     # we can use `stack_active()` which will direct subsequent addition into
#'     # the stack
#'     stack_active() +
#'     # here we add a dendrogram to the stack.
#'     align_dendro()
#' @export
stack_switch <- function(sizes = NULL, what = waiver(), ...) {
    rlang::check_dots_empty()
    if (!is.waive(what)) what <- check_stack_context(what)
    if (!is.null(sizes)) sizes <- check_stack_sizes(sizes)
    structure(list(what = what, sizes = sizes), class = "stack_switch")
}

#' @export
#' @rdname stack_switch
stack_active <- function(sizes = NULL, ...) {
    rlang::check_dots_empty()
    stack_switch(sizes, what = NULL)
}

#' @include layout-operator.R
S7::method(
    layout_add,
    list(StackLayout, S7::new_S3_class("ggalign_with_quad"))
) <-
    S7::method(
        layout_add,
        list(StackLayout, S7::new_S3_class("quad_active"))
    ) <-
    S7::method(
        layout_add,
        list(StackLayout, S7::new_S3_class("quad_anno"))
    ) <-
    S7::method(
        layout_add,
        list(StackLayout, StackLayout)
    ) <-
    function(layout, object, objectname) {
        if (is.null(current <- layout@current) ||
            !is_layout(box <- .subset2(layout@box_list, current))) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
                    object_name(layout)
                ),
                i = "Did you forget to add a {.fn quad_layout}?"
            ))
        } else {
            attr(layout, "box_list")[[current]] <- layout_add(
                box, object, objectname
            )
        }
        layout
    }

#' @include layout-operator.R
S7::method(
    layout_add,
    list(StackLayout, S7::new_S3_class("stack_switch"))
) <- function(layout, object, objectname) {
    layout <- switch_chain_plot(
        layout, .subset2(object, "what"),
        quote(stack_switch())
    )
    if (!is.null(sizes <- .subset2(object, "sizes"))) {
        layout@sizes <- sizes
    }
    layout
}

#' @include layout-operator.R
S7::method(layout_add, list(StackLayout, QuadLayout)) <-
    function(layout, object, objectname) {
        # preventing from adding `stack_cross` with the same direction
        # `cross_link()` cannot be added to the heatmap annotation
        # parallelly with the `stack_cross()`
        if (is_horizontal(direction <- layout@direction)) {
            if (is_cross_layout(object@left) || is_cross_layout(object@right)) {
                cli_abort(c(
                    sprintf(
                        "Cannot add {.var {objectname}} to %s",
                        object_name(layout)
                    ),
                    i = sprintf(
                        "{.field left} or {.field right} annotation contains %s",
                        "{.fn stack_cross}"
                    )
                ))
            }
        } else if (is_cross_layout(object@top) ||
            is_cross_layout(object@bottom)) {
            cli_abort(c(
                sprintf(
                    "Cannot add {.var {objectname}} to %s",
                    object_name(layout)
                ),
                i = sprintf(
                    "{.field top} or {.field bottom} annotation contains %s",
                    "{.fn stack_cross}"
                )
            ))
        }

        # check quad layout is compatible with stack layout
        quad_data <- object@data
        stack_domain <- layout@domain
        quad_domain <- slot(object, direction)
        if (!is_discrete_domain(quad_domain)) {
            if (is_discrete_domain(stack_domain)) {
                cli_abort(c(
                    sprintf(
                        "Cannot add %s to %s",
                        object_name(object), object_name(layout)
                    ),
                    i = sprintf(
                        "%s cannot align continuous variable",
                        object_name(layout)
                    )
                ))
            }
            # `quad_layout()` will align continuous variables,
            # `data` can be `NULL`
            extra_domain <- slot(object, vec_set_difference(
                c("vertical", "horizontal"), direction
            ))
            allow_null <- !is_discrete_domain(extra_domain)
            if (is.waive(quad_data) || is.function(quad_data)) {
                # check if we should initialize the `quad_layout()` data
                if (is.null(stack_data <- layout@data)) {
                    if (allow_null) {
                        quad_data <- NULL
                    } else {
                        cli_abort(c(
                            sprintf(
                                "you must provide {.arg data} argument in %s",
                                object_name(object)
                            ),
                            i = sprintf(
                                "no data was found in %s",
                                object_name(layout)
                            )
                        ))
                    }
                } else {
                    data <- stack_data # should be a data frame
                    if (is.waive(quad_data)) { # inherit from the stack layout
                        if (!allow_null) { # we need a matrix
                            cli_abort(c(
                                sprintf(
                                    "Cannot add %s to %s",
                                    object_name(object), object_name(layout)
                                ),
                                i = sprintf(
                                    "{.arg data} in %s is %s, but %s need a {.cls matrix}.",
                                    object_name(layout),
                                    "{.obj_type_friendly {data}}",
                                    object_name(object)
                                ),
                                i = sprintf(
                                    "Try provide {.arg data} in %s",
                                    object_name(object)
                                )
                            ))
                        }
                    } else { # `quad_data` is a function
                        data <- quad_data(data)
                        # check the data format is correct
                        if (allow_null) { # we need a data frame
                            if (!is.data.frame(data)) {
                                cli_abort(sprintf(
                                    "{.arg data} in %s must return a {.cls data.frame}",
                                    object_name(object)
                                ))
                            }
                        } else if (!is.matrix(data)) { # we need a matrix
                            cli_abort(sprintf(
                                "{.arg data} in %s must return a {.cls matrix}",
                                object_name(object)
                            ))
                        } else {
                            if (NROW(data) == 0L || ncol(data) == 0L) {
                                cli_abort(sprintf(
                                    "{.arg data} in %s return an empty matrix",
                                    object_name(object)
                                ))
                            }
                        }
                    }
                    # we initialize the `nobs` of the extra_domain for the
                    # `quad_layout()`
                    if (is_horizontal(direction)) {
                        if (is_discrete_domain(slot(object, "vertical"))) {
                            prop(slot(object, "vertical"), "nobs") <- ncol(data)
                        }
                    } else {
                        if (is_discrete_domain(slot(object, "horizontal"))) {
                            prop(slot(object, "horizontal"), "nobs") <- nrow(data)
                        }
                    }
                }
                # restore the ggalign attribute
                object@data <- ggalign_data_restore(data, stack_data)
            }
            layout_domain <- quad_domain
        } else if (is_discrete_domain(stack_domain)) {
            # both `quad_layout()` and `stack_layout()` will align discrete
            # variables
            if (is.waive(quad_data) || is.function(quad_data)) {
                if (is.null(stack_data <- layout@data)) {
                    cli_abort(c(
                        sprintf(
                            "you must provide {.arg data} argument in %s",
                            object_name(object)
                        ),
                        i = sprintf(
                            "no data was found in %s",
                            object_name(layout)
                        )
                    ))
                }
                # set `quad_layout()` data
                data <- switch_direction(direction, stack_data, t(stack_data))
                if (is.function(quad_data)) {
                    data <- quad_data(data)
                    if (!is.matrix(data)) {
                        cli_abort(sprintf(
                            "{.arg data} in %s must return a matrix",
                            object_name(object)
                        ))
                    }
                    if (NROW(data) == 0L || ncol(data) == 0L) {
                        cli_abort(sprintf(
                            "{.arg data} in %s return an empty matrix",
                            object_name(object)
                        ))
                    }
                } else {
                    if (NROW(data) == 0L || ncol(data) == 0L) {
                        cli_abort(c(
                            sprintf(
                                "Cannot use data from %s in %s",
                                object_name(layout), object_name(object)
                            ),
                            i = sprintf(
                                "{.arg data} in %s is an empty matrix",
                                object_name(layout)
                            )
                        ))
                    }
                }
                # set the `nobs` for `quad_layout()`
                if (is_horizontal(direction)) {
                    prop(quad_domain, "nobs") <- nrow(data)
                    if (is_discrete_domain(slot(object, "vertical"))) {
                        prop(slot(object, "vertical"), "nobs") <- ncol(data)
                    }
                } else {
                    prop(quad_domain, "nobs") <- ncol(data)
                    if (is_discrete_domain(slot(object, "horizontal"))) {
                        prop(slot(object, "horizontal"), "nobs") <- nrow(data)
                    }
                }
                # restore the ggalign attribute
                object@data <- ggalign_data_restore(data, stack_data)
            }
            layout_domain <- discrete_domain_update(
                stack_domain, quad_domain,
                old_name = object_name(layout),
                new_name = objectname
            )
        } else {
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(object), object_name(layout)
                ),
                i = sprintf(
                    "%s cannot align discrete variable",
                    object_name(layout)
                )
            ))
        }
        stack <- chain_add_plot(layout, object, object@plot_active, objectname)
        layout_update_domain(
            stack,
            domain = layout_domain, objectname = objectname
        )
    }
