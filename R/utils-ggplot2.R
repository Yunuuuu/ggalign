ggfun <- local({
    ggplot2_namespace <- NULL
    function(x, mode = "any") {
        if (is.null(ggplot2_namespace)) {
            ggplot2_namespace <<- getNamespace("ggplot2")
        }
        get(x, envir = ggplot2_namespace, inherits = FALSE, mode = mode)
    }
})

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) rlang::as_function(x) else x
}

is.waive <- function(x) inherits(x, "waiver")

`%|w|%` <- function(x, y) if (inherits(x, "waiver")) y else x

snake_class <- function(x) ggfun("snake_class")(x)

add_default_mapping <- function(plot, default_mapping) {
    mapping <- .subset2(plot, "mapping")
    for (nm in names(mapping)) {
        default_mapping[[nm]] <- .subset2(mapping, nm)
    }
    plot$mapping <- default_mapping
    plot
}

#' @importFrom rlang env_clone
ggproto_clone <- function(ggproto) {
    ans <- env_clone(ggproto)
    class(ans) <- class(ggproto)
    ans
}

#######################################################################
#' @keywords internal
coord_ggalign <- function(xlim_list = NULL, ylim_list = NULL) {
    if (!is.null(xlim_list) && !is.list(xlim_list)) xlim_list <- list(xlim_list)
    if (!is.null(ylim_list) && !is.list(ylim_list)) ylim_list <- list(ylim_list)
    structure(
        list(xlim_list = xlim_list, ylim_list = ylim_list),
        class = "coord_ggalign"
    )
}

set_limits <- function(scale_name, layout) {
    panel <- .subset2(layout, "panel")
    index <- .subset2(layout, "index")

    # rearrange panel based on the index
    panel <- panel[index]
    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (scale_name == "y") panel <- fct_rev(panel)
    lapply(split(seq_along(index), panel), function(plot_coord) {
        range(plot_coord) + c(-0.5, 0.5)
    })
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @importFrom vctrs vec_unique_count
#' @export
ggplot_add.coord_ggalign <- function(object, plot, object_name) {
    Parent <- .subset2(plot, "coordinates")
    plot$coordinates <- ggproto(
        NULL, Parent,
        num_of_panels = NULL,
        panel_counter = NULL,
        setup_layout = function(self, layout, params) {
            # we always initialize the number of panels and a panel counter
            self$num_of_panels <- vec_unique_count(.subset2(layout, "PANEL"))
            self$panel_counter <- 0L
            # call the parent method
            ggproto_parent(Parent, self)$setup_layout(layout, params)
        },
        setup_panel_params = function(self, scale_x, scale_y, params = list()) {
            cur_panel <- self$panel_counter + 1L
            on.exit(self$panel_counter <- cur_panel)
            if (!is.null(xlim_list <- .subset2(object, "xlim_list")) &&
                length(xlim_list) >= cur_panel) {
                self$limits$x <- .subset2(xlim_list, cur_panel)
            }
            if (!is.null(ylim_list <- .subset2(object, "ylim_list")) &&
                length(ylim_list) >= cur_panel) {
                self$limits$y <- .subset2(ylim_list, cur_panel)
            }
            ggproto_parent(Parent, self)$setup_panel_params(
                scale_x = scale_x, scale_y = scale_y, params = params
            )
        }
    )
    plot
}

###############################################################
#' @keywords internal
facet_ggalign <- function(x = NULL, y = NULL) {
    structure(list(x = x, y = y), class = "facet_ggalign")
}

# ggplot2 add default scales in `compute_aesthetics` process
# then ggplot2 transform all scales
#  layout:
#    - `setup`:
#       - call `setup_params`
#       - attach `plot_env`
#       - call `setup_data`
#       - call `compute_layout`
#       - call `map_data`
#    - `train_position` call `init_scales`
#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.facet_ggalign <- function(object, plot, object_name) {
    Parent <- .subset2(plot, "facet")
    plot$facet <- ggproto(
        NULL, Parent,
        init_scales = function(self, layout,
                               x_scale = NULL, y_scale = NULL, params) {
            scales <- ggproto_parent(Parent, self)$init_scales(
                layout, x_scale, y_scale, params
            )
            # for each scale, we set the `breaks` and `labels`
            if (!is.null(x_scale) &&
                !is.null(x_layout <- .subset2(object, "x"))) {
                scales$x <- init_scales(
                    x_scale, "x", x_layout,
                    panel_scales = .subset2(scales, "x")
                )
            }
            if (!is.null(y_scale) &&
                !is.null(y_layout <- .subset2(object, "y"))) {
                scales$y <- init_scales(
                    y_scale, "y", y_layout,
                    panel_scales = .subset2(scales, "y")
                )
            }
            scales
        }
    )
    plot
}

#' @importFrom vctrs vec_slice
init_scales <- function(scale, scale_name, layout, panel_scales) {
    panel <- .subset2(layout, "panel")
    index <- .subset2(layout, "index")
    labels <- .subset2(layout, "labels")

    # rearrange panel based on the index
    panel <- panel[index]
    # For y-axis, ggplot arrange panel from top to bottom,
    # we always choose to reverse the panel order
    if (scale_name == "y") panel <- fct_rev(panel)

    # we always use the discrete scale to determine labels and breaks
    # https://github.com/tidyverse/ggplot2/blob/7fb4c382f9ea332844d469663a8047355a88dd7a/R/scale-.R#L927
    if (is.null(labels) && is.waive(scale$labels) && is.waive(scale$breaks)) {
        # special case for data have no labels
        # By default we remove breaks and labels
        breaks <- NULL
        labels <- NULL
    } else {
        breaks <- get_breaks(scale, seq_along(index), labels)
        labels <- get_labels(scale, breaks, labels)
    }

    # by default we elways remove any expansion
    default_expand <- ggplot2::expansion()
    # the global expand should be added in the two flank
    flank_expand <- scale$expand %|w|% default_expand

    # setup the expand for usage of each scale
    if (nlevels(panel) > 1L) {
        expand <- rep_len(list(default_expand), nlevels(panel))
        # for expand in the head
        expand[[1L]] <- c(vec_slice(flank_expand, 1:2), rep_len(0, 2L))
        # for expand in the tail
        expand[[nlevels(panel)]] <- c(
            rep_len(0, 2L), vec_slice(flank_expand, 3:4)
        )
    } else {
        expand <- list(flank_expand)
    }

    # fill `NULL` with the global scale --------------------
    .mapply(function(s, data_index, plot_coord, e) {
        if (is.null(breaks)) {
            s$breaks <- NULL
            s$labels <- NULL
        } else {
            # setup breaks and labels --------------------
            in_domain <- match(data_index, breaks)
            keep <- !is.na(in_domain)
            s$breaks <- plot_coord[keep]
            s$labels <- labels[in_domain[keep]]
        }
        s$expand <- e
        s
    }, list(
        s = panel_scales,
        data_index = split(index, panel),
        plot_coord = split(seq_along(index), panel),
        e = expand
    ), NULL)
}

identity_trans <- function(scale) {
    # for continuous scale, we don't allow the trans
    if (!scale$is_discrete() && !identical(scale$trans$name, "identity")) {
        cli::cli_warn(sprintf(
            "{.arg trans} must be {.field identity} in {.code %s}",
            deparse(scale$call)
        ))
        scale$trans <- scales::as.transform("identity")
    }
    scale
}

#' @importFrom vctrs vec_cast
get_breaks <- function(scale, layout_breaks, layout_labels) {
    breaks <- scale$breaks
    if (identical(breaks, NA)) {
        cli::cli_abort(c(
            "Invalid {.arg breaks} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }
    if (is.null(breaks)) {
        return(NULL)
    }

    if (is.waive(breaks)) {
        breaks <- layout_breaks
    } else {
        if (is.function(breaks)) {
            breaks <- breaks(layout_labels %||% layout_breaks)
        }

        if (is.factor(breaks) || is.character(breaks)) {
            # we interpreted the character breaks as the names of the original
            # matrix data.
            breaks <- layout_breaks[
                match(as.character(breaks), layout_labels %||% layout_breaks)
            ]
        } else {
            # breaks must be a character or an integer
            breaks <- vec_cast(breaks, integer(), call = scale$call)
        }
    }

    # Breaks only occur only on values in domain
    in_domain <- intersect(breaks, layout_breaks)
    structure(in_domain, pos = match(in_domain, breaks))
}

#' @importFrom rlang is_empty
get_labels <- function(scale, breaks, layout_labels) {
    labels <- scale$labels
    if (is_empty(breaks)) { # if no breaks, no labels
        return(NULL)
    }

    if (is.null(labels)) {
        return(NULL)
    }

    if (identical(labels, NA)) {
        cli::cli_abort(c(
            "Invalid {.arg labels} specification.",
            i = "Use {.code NULL}, not {.code NA}."
        ), call = scale$call)
    }

    # if layout have no names, use the breaks directly
    if (!is.null(layout_labels)) {
        user_breaks <- layout_labels[breaks]
    } else {
        user_breaks <- breaks
    }

    if (is.waive(labels)) {
        user_breaks
    } else if (is.function(labels)) {
        labels(user_breaks)
    } else if (!is.null(names(labels))) {
        # If labels have names, use them to match with breaks
        map <- match(names(labels), user_breaks, nomatch = 0L)
        user_breaks[map] <- labels[map != 0L]
        user_breaks
    } else {
        # Need to ensure that if breaks were dropped, corresponding labels
        # are too
        if (!is.null(pos <- attr(breaks, "pos"))) {
            labels <- labels[pos]
        }
        labels
    }
}
