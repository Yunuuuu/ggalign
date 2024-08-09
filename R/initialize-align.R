#' object is an environment, it won't be copied, and will modify in place
#' @noRd
initialize_align <- function(object, direction,
                             layout_data,
                             layout_panel, layout_index,
                             nobs, object_name) {
    object$direction <- direction
    input_data <- .subset2(object, "input_data")
    input_params <- .subset2(object, "input_params")
    call <- .subset2(object, "call")

    # we must have the same observations across all plots
    # 1. if Align require data, the `nobs` should be nrow(data)
    # 2. if not, we run `nobs()` method to get the `nobs`
    # we must know the `nobs` before check arguments since we need check
    # the arguments user passed are compatible with `nobs`
    if (!is.null(input_data)) { # this `Align` object require data
        if (is.waive(input_data)) { # inherit from the layout
            if (is.null(layout_data)) {
                cli::cli_abort(c(
                    paste(
                        "You must provide {.arg data} argument in",
                        style_code(object_name)
                    ),
                    i = "No data was found in the layout"
                ), call = call)
            }
            data <- layout_data
        } else {
            data <- align_setup_data(input_data, layout_data)
            # we always regard rows as the observations
            if (is.na(nobs)) {
                nobs <- NROW(data)
            } else if (NROW(data) != nobs) {
                cli::cli_abort(sprintf(
                    "%s from %s must have %s",
                    style_arg("data"), style_code(object_name),
                    "compatible observations with the layout"
                ), call = call)
            }
        }
        object$labels <- rownames(data)
        params <- object$setup_params(nobs, input_params)
        object$data <- object$setup_data(params, data)
    } else {
        if (is.na(nobs)) nobs <- object$nobs(input_params)
        params <- object$setup_params(nobs, input_params)
    }
    # setup parameters and check parameters ----------
    object$params <- params

    # prepare the data -------------------------------
    ans <- initialize_align_layout(
        object, nobs, direction,
        layout_panel, layout_index, object_name
    )
    c(ans, list(nobs = nobs))
}

initialize_align_layout <- function(object, nobs, direction,
                                    layout_panel, layout_index,
                                    object_name) {
    axis <- to_coord_axis(direction)

    # prepare and check parameters ----------------------
    call <- .subset2(object, "call")
    params <- .subset2(object, "params")

    # compute statistics ---------------------------------
    compute_params <- params[
        intersect(names(params), align_method_params(object$compute))
    ]
    object$statistics <- rlang::inject(
        object$compute(layout_panel, layout_index, !!!compute_params)
    )

    # make the new layout -------------------------------
    layout_params <- params[
        intersect(names(params), align_method_params(object$layout))
    ]
    layout <- rlang::inject(
        object$layout(layout_panel, layout_index, !!!layout_params)
    )

    new_panel <- .subset2(layout, 1L)
    if (!is.null(new_panel)) {
        if (anyNA(new_panel)) {
            cli::cli_abort(
                "{.fn {snake_class(object)}}: find `NA` in layout panel",
                call = call
            )
        } else if (!is.atomic(new_panel)) {
            cli::cli_abort(
                c(
                    "{.fn {snake_class(object)}}: invalid layout panel",
                    i = "layout panel must be an atomic vector"
                ),
                call = call
            )
        } else if (length(new_panel) != nobs) {
            cli::cli_abort(
                paste(
                    "{.fn layout} panel of {.fn {snake_class(object)}}",
                    "is not compatible with {axis}-axis"
                ),
                call = call
            )
        }
    }

    # the panel factor level determine the group order
    if (!is.null(new_panel) && !is.factor(new_panel)) {
        new_panel <- factor(new_panel)
    }

    new_index <- .subset2(layout, 2L)
    if (!is.null(new_index)) {
        if (anyNA(new_index)) {
            cli::cli_abort(
                "{.fn {snake_class(object)}}: find `NA` in layout index",
                call = call
            )
        } else if (!is.integer(new_index)) {
            cli::cli_abort(
                c(
                    "{.fn {snake_class(object)}}: invalid layout index",
                    i = "layout index must be integer"
                ),
                call = call
            )
        } else if (length(new_index) != nobs) {
            cli::cli_abort(
                paste(
                    "layout index of {.fn {snake_class(object)}}",
                    "is not compatible with {axis}-axis"
                ),
                call = call
            )
        }
    }

    # we always make the index following the panel
    # if the `layout_index` is not `NULL` and we create a new group,
    # this will also ensure the `new_index` is not the same with `layout_index`
    # which finally cause error.
    if (!is.null(new_panel) && !is.null(new_index)) {
        new_index <- reorder_index(new_panel, new_index)
    }

    # we always prevent from reordering layout twice.
    if (!is.null(layout_index)) {
        if (!all(layout_index == new_index)) {
            cli::cli_abort(sprintf(
                "{.fn {snake_class(object)}} disrupt the previously %s %s-axis",
                "established order of the layout",
                axis
            ), call = call)
        }
    }

    # in the finally, Let us initialize the annotation plot -----
    # must return a ggplot object
    ggplot_params <- params[
        intersect(
            names(params),
            align_method_params(object$ggplot, character())
        )
    ]
    p <- rlang::inject(object$ggplot(!!!ggplot_params))
    object$plot <- p

    # add annotation -------------------------------------
    list(new_panel, new_index)
}

reorder_index <- function(panel, index = NULL, reverse = FALSE) {
    index <- index %||% seq_along(panel)
    unlist(split(index, panel[index]), recursive = FALSE, use.names = FALSE)
}

####################################################################
#' @keywords internal
align_setup_data <- function(data, layout_data) {
    UseMethod("align_setup_data")
}

#' @export
align_setup_data.data.frame <- function(data, layout_data) {
    data
}

#' @export
align_setup_data.matrix <- align_setup_data.data.frame

#' @export
align_setup_data.numeric <- function(data, layout_data) {
    as.matrix(data)
}

#' @export
align_setup_data.character <- align_setup_data.numeric

#' @export
align_setup_data.function <- function(data, layout_data) {
    # fix R CMD check note: Found the following calls to data() loading into the
    # global environment
    fn <- data
    data <- fn(layout_data)
    align_setup_data(data, layout_data)
}
