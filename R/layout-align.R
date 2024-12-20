#' Set Expansion for the Layout
#'
#' @description
#' To align axes, it is important to keep the expansion consistent across all
#' plots in the layout. You can add a `layout_expand` object to the layout. For
#' the `quad_layout()` function, you must specify `x` and `y` arguments. For
#' other layouts, you can pass the expansion values using `...` directly.
#'
#' @param ... A list of range expansion constants, used to add padding around
#' the data to ensure they are placed some distance away from the axes. Use the
#' convenience function [`expansion()`][ggplot2::expansion()] to generate the
#' values.
#' @param x,y Same as `...`, but specifically for `quad_layout()`.
#'
#' @importFrom rlang list2
#' @export
layout_expand <- function(..., x = waiver(), y = waiver()) {
    if (...length() > 0L && (!is.waive(x) || !is.waive(y))) {
        cli_abort(
            "Cannot mix the usage of {.arg ...} with {.arg x}/{.arg y} argument"
        )
    }
    if (...length() > 0L) {
        ans <- list2(...)
        names(ans) <- NULL
    } else {
        ans <- list(x = x, y = y)
    }
    structure(ans, class = "ggalign_layout_expand")
}

#' Set continuous limits for the layout
#'
#' @description
#' To align continuous axes, it is important to keep the limits consistent
#' across all plots in the layout. You can set the limits by passing a function
#' directly to the `limits` or `xlim`/`ylim` argument, using `...` only.
#' Alternatively, you can add a `continuous_limits()` object to the layout. For
#' the `quad_layout()` function, you must specify `x`/`y` arguments. For other
#' layouts, you should pass the limits using `...` directly.
#'
#' @param ... A list of two numeric values, specifying the left/lower limit and
#' the right/upper limit of the scale.
#' @inheritParams layout_expand
#' @importFrom rlang list2
#' @export
continuous_limits <- function(..., x = waiver(), y = waiver()) {
    if (...length() > 0L && (!is.waive(x) || !is.waive(y))) {
        cli_abort(
            "Cannot mix the usage of {.arg ...} with {.arg x}/{.arg y} argument"
        )
    }
    if (...length() > 0L) {
        ans <- list2(...)
        names(ans) <- NULL
    } else {
        ans <- list(x = x, y = y)
    }
    structure(ans, class = c("continuous_limits", "layout_design"))
}

################################################################
is_continuous_design <- function(x) {
    is.null(x) || inherits(x, "continuous_limits")
}

is_discrete_design <- function(x) inherits(x, "discrete_design")

#' Layout can align ordinal variable or continuous variable
#'
#' @param x A `LayoutProto` object.
#' @noRd
is_layout_discrete <- function(x, ...) UseMethod("is_layout_discrete")

is_layout_continuous <- function(x, ...) UseMethod("is_layout_continuous")

################################################################
# layout params are used to align the observations
discrete_design <- function(panel = NULL, index = NULL, nobs = NULL) {
    structure(
        list(panel = panel, index = index, nobs = nobs),
        class = c("discrete_design", "layout_design")
    )
}

# Initialize the index and panel
# Reorder the panel based the ordering index and
setup_discrete_design <- function(design) {
    if (is.null(design)) return(NULL) # styler: off
    # if `nobs` is not initialized, it means no `Align` object exist
    # it's not necessary to initialize the `panel` and `index`
    # this is for `stack_layout` which may have no data
    if (is.null(nobs <- .subset2(design, "nobs"))) {
        return(design)
    }
    panel <- .subset2(design, "panel") %||% factor(rep_len(1L, nobs))
    index <- .subset2(design, "index") %||% reorder_index(panel)
    discrete_design(panel[index], index, nobs)
}

reorder_index <- function(panel, index = NULL) {
    index <- index %||% seq_along(panel)
    unlist(split(index, panel[index]), recursive = FALSE, use.names = FALSE)
}

############################################################
#' @keywords internal
update_design <- function(layout, ..., design, object_name) {
    UseMethod("update_design")
}

#' @importFrom methods slot slot<-
#' @export
update_design.QuadLayout <- function(layout, ..., direction, design,
                                     object_name) {
    slot(layout, direction) <- design
    if (is_horizontal(direction)) {
        if (!is.null(left <- layout@left)) {
            layout@left <- update_design(left,
                design = design, object_name = object_name
            )
        }
        if (!is.null(right <- layout@right)) {
            layout@right <- update_design(right,
                design = design, object_name = object_name,
                from_head = TRUE
            )
        }
    } else {
        if (!is.null(top <- layout@top)) {
            layout@top <- update_design(top,
                design = design, object_name = object_name
            )
        }
        if (!is.null(bottom <- layout@bottom)) {
            layout@bottom <- update_design(bottom,
                design = design, object_name = object_name,
                from_head = TRUE
            )
        }
    }
    layout
}

#' @importFrom methods slot slot<-
#' @export
update_design.StackLayout <- function(layout, ..., design, object_name) {
    layout@design <- design
    layout@plot_list <- lapply(layout@plot_list, function(plot) {
        if (is_ggalign_plot(plot)) return(plot) # styler: off
        update_design(plot,
            direction = layout@direction,
            design = design
        )
    })
    layout
}

#' @importFrom methods slot slot<-
#' @export
update_design.CrossLayout <- function(layout, ..., design, object_name,
                                      from_head = FALSE) {
    if (from_head && !is_empty(layout@cross_points)) {
        layout@design["nobs"] <- list(.subset2(design, "nobs"))
        layout@design["panel"] <- list(.subset2(design, "panel"))
        layout@index_list[1L] <- list(.subset2(design, "index"))
    } else {
        layout@design <- design
    }
    n_plots <- length(plot_list <- layout@plot_list)
    if (n_plots == 0L) {
        return(layout)
    }
    if (n_breaks <- length(layout@cross_points)) {
        # we also check the panel doesn't break the original index
        if (!is.null(panel <- .subset2(design, "panel"))) {
            index_list <- layout@index_list
            for (i in seq_along(index_list)) {
                if (is.null(old <- .subset2(index_list, i))) {
                    next
                }
                new <- reorder_index(panel, old)
                # we always prevent from reordering twice.
                if (!is.null(old) && !all(old == new)) {
                    cli_abort(sprintf(
                        "%s disrupt the previously established ordering index of %s (%d)",
                        object_name, object_name(layout), i
                    ))
                }
                index_list[[i]] <- new
            }
            layout@index_list <- index_list
        }
        # extract the the first or the last `cross_points`
        if (from_head) { # update the head plots
            index <- layout@cross_points[1L]
            if (index == 0L) {
                return(layout)
            }
            index <- seq_len(index)
        } else { # update the tail plots
            # one for the `ggcross()` plot itself
            index <- layout@cross_points[n_breaks] + 1L + 1L
            if (index > n_plots) {
                return(layout)
            }
            index <- index:n_plots
        }
    } else { # if no breaks, update all plots
        index <- seq_len(n_plots)
    }

    layout@plot_list[index] <- lapply(plot_list[index], function(plot) {
        if (is_ggalign_plot(plot)) return(plot) # styler: off
        update_design(plot,
            direction = layout@direction,
            design = design
        )
    })
    layout
}

############################################################
check_discrete_design <- function(old, new, old_name, new_name,
                                  call = caller_call()) {
    old_nobs <- .subset2(old, "nobs")
    new_nobs <- .subset2(new, "nobs")
    if (is.null(new_nobs)) { # no `nobs` provided
        nobs <- old_nobs
    } else if (is.null(old_nobs)) {
        nobs <- new_nobs
    } else if (!identical(new_nobs, old_nobs)) {
        cli_abort(sprintf(
            "%s (nobs: %d) is not compatible with the %s (nobs: %d)",
            new_name, new_nobs, old_name, old_nobs
        ), call = call)
    } else {
        nobs <- new_nobs
    }

    # check panel
    old_panel <- .subset2(old, "panel")
    new_panel <- .subset2(new, "panel")

    if (is.null(new_panel)) { # no panel provided
        panel <- old_panel
    } else if (anyNA(new_panel)) {
        cli_abort(sprintf(
            "layout panels defined by %s contain `NA`",
            new_name
        ), call = call)
    } else if (!is.atomic(new_panel)) {
        cli_abort(c(
            sprintf("invalid layout panels defined by %s", new_name),
            i = "layout panels must be an atomic vector"
        ), call = call)
    } else if (is.null(nobs) || length(new_panel) != nobs) {
        # we have defined panel, but don't define the `nobs`
        cli_abort(sprintf(
            "layout panels defined by %s (nobs: %d) is not compatible with the nobs: %d",
            new_name, length(new_panel), nobs %||% 0L
        ), call = call)
    } else if (!is.null(old_panel) && !(new_panel %nest% old_panel)) {
        cli_abort(sprintf(
            "%s disrupt the previously established panel groups of %s",
            new_name, old_name
        ), call = call)
    } else {
        panel <- new_panel
        if (!is.factor(panel)) panel <- factor(panel)
    }

    # check index
    old_index <- .subset2(old, "index")
    new_index <- .subset2(new, "index")
    if (is.null(new_index)) {
        index <- old_index
    } else if (anyNA(new_index)) {
        cli_abort(sprintf(
            "layout ordering index defined by %s contain `NA`",
            new_name
        ), call = call)
    } else if (!is.integer(new_index)) {
        cli_abort(c(
            sprintf("invalid layout ordering index defined by %s", new_name),
            i = "layout ordering index must be integer"
        ), call = call)
    } else if (is.null(nobs) || length(new_index) != nobs) {
        # we have defined index, but don't define the `nobs`
        cli_abort(sprintf(
            "layout ordering index defined by %s (nobs: %d) is not compatible with the nobs: %d",
            new_name, length(new_index), nobs %||% 0L
        ), call = call)
    } else {
        index <- new_index
    }

    # we always make the index following the panel
    if (!is.null(panel) && !is.null(index)) {
        index <- reorder_index(panel, index)
    }

    # we always prevent from reordering twice.
    if (!is.null(old_index) && !all(old_index == index)) {
        cli_abort(sprintf(
            "%s disrupt the previously established ordering index of %s",
            new_name, old_name
        ), call = call)
    }
    discrete_design(panel, index, nobs)
}
