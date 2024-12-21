ggfun <- local({
    ggplot2_namespace <- NULL
    function(x, mode = "any") {
        if (is.null(ggplot2_namespace)) {
            ggplot2_namespace <<- getNamespace("ggplot2")
        }
        get(x, envir = ggplot2_namespace, inherits = FALSE, mode = mode)
    }
})

#' @importFrom ggplot2 .pt
NULL

allow_lambda <- function(x) {
    if (rlang::is_formula(x)) rlang::as_function(x) else x
}

#' @importFrom rlang try_fetch
fill_alpha <- function(colour, alpha = NA) {
    try_fetch(
        # for version >= 3.5.0
        ggplot2::fill_alpha(colour, alpha),
        error = function(cnd) {
            # for version < 3.5.0
            ggplot2::alpha(colour, alpha)
        }
    )
}

is.waive <- function(x) inherits(x, "waiver")

`%|w|%` <- function(x, y) if (inherits(x, "waiver")) y else x

snake_class <- function(x) ggfun("snake_class")(x)

ggadd_default <- function(plot, mapping = NULL, theme = NULL) {
    if (!is.null(mapping)) {
        plot <- plot + mapping + .subset2(plot, "mapping")
    }
    if (!is.null(theme)) plot$theme <- theme + .subset2(plot, "theme")
    plot
}

######################################################
default_expansion <- function(x = NULL, y = NULL) {
    structure(list(x = x, y = y), class = c("ggalign_default_expansion"))
}

#' @importFrom ggplot2 ggplot_add ggproto ggproto_parent
#' @export
ggplot_add.ggalign_default_expansion <- function(object, plot, object_name) {
    if (is.null(.subset2(object, "x")) && is.null(.subset2(object, "y"))) {
        return(plot)
    }
    ParentFacet <- .subset2(plot, "facet")
    plot$facet <- ggproto(
        NULL, ParentFacet,
        init_scales = function(self, layout, x_scale = NULL,
                               y_scale = NULL, params) {
            if (!is.null(x_scale) && !is.null(.subset2(object, "x"))) {
                x_scale$expand <- x_scale$expand %|w|% .subset2(object, "x")
            }
            if (!is.null(y_scale) && !is.null(.subset2(object, "y"))) {
                y_scale$expand <- y_scale$expand %|w|% .subset2(object, "y")
            }
            ggproto_parent(ParentFacet, self)$init_scales(
                layout = layout,
                x_scale = x_scale,
                y_scale = y_scale,
                params = params
            )
        }
    )
    plot
}

######################################################
gguse_data <- function(plot, data) {
    # ggplot use waiver() to indicate no data
    plot["data"] <- list(data %||% waiver())
    plot
}

######################################################
# this will remove the old coordinate,
# so always run firstly
gguse_linear_coord <- function(plot, layout_name) {
    coord <- plot$coordinates
    if (!inherits(coord, "CoordTrans") && !coord$is_linear()) {
        cli_warn(c(
            sprintf(
                "{.fn %s} is not supported in %s",
                snake_class(coord), layout_name
            ),
            i = "Will use {.fn coord_cartesian} instead"
        ))
        plot$coordinates <- ggplot2::coord_cartesian()
    }
    plot
}

view_scales_polar <- function(scale, theta, coord_limits, expand = TRUE) {
    aesthetic <- scale$aesthetics[1]
    is_theta <- theta == aesthetic
    name <- if (is_theta) "theta" else "r"
    default_expansion <- function(scale, discrete = expansion(add = 0.6),
                                 continuous = expansion(mult = 0.05), 
                                 expand = TRUE) {
        out <- expansion()
        if (!any(expand)) {
            return(out)
        }
        scale_expand <- scale$expand %|w|%
            if (scale$is_discrete()) discrete else continuous

        # for backward compatibility, we ensure expansions have expected length
        expand <- rep_len(expand, 2L)
        scale_expand <- rep_len(scale_expand, 4)

        if (expand[1]) {
            out[1:2] <- scale_expand[1:2]
        }
        if (expand[2]) {
            out[3:4] <- scale_expand[3:4]
        }
        out
    }
    expansion <- default_expansion(scale, expand = expand)
    limits <- scale$get_limits()
    continuous_range <- ggfun("expand_limits_scale")(
        scale, expansion, limits, coord_limits = coord_limits
    )

    primary <- ggfun("view_scale_primary")(scale, limits, continuous_range)
    view_scales <- list(
        primary,
        sec = ggfun("view_scale_secondary")(scale, limits, continuous_range),
        major = primary$map(primary$get_breaks()),
        minor = primary$map(primary$get_breaks_minor()),
        range = continuous_range
    )

    names(view_scales) <- c(name, paste0(name, ".", names(view_scales)[-1]))
    view_scales
}

gguse_radial_coord <- function(plot, coord, ..., layout_name) {
    setup_panel_params <- function(self, scale_x, scale_y, params = list()) {
        params <- c(
            view_scales_polar(
                scale_x, self$theta,
                self$limits$x,
                expand = params$expand[c(4, 2)]
            ),
            view_scales_polar(
                scale_y, self$theta,
                self$limits$y,
                expand = params$expand[c(3, 1)]
            ),
            list(
                bbox = ggfun("polar_bbox")(
                    self$arc, inner_radius = self$inner_radius),
                arc = self$arc, inner_radius = self$inner_radius
            )
        )
        axis_rotation <- self$r_axis_inside
        if (is.numeric(axis_rotation)) {
            theta_scale <- switch(self$theta,
                x = scale_x,
                y = scale_y
            )
            axis_rotation <- theta_scale$transform(axis_rotation)
            axis_rotation <- ggfun("oob_squish")(
                axis_rotation, params$theta.range
            )
            axis_rotation <- ggfun("theta_rescale")(
                axis_rotation, params$theta.range,
                params$arc, 1
            )
            params$axis_rotation <- rep_len(axis_rotation, length.out = 2)
        } else {
            params$axis_rotation <- params$arc
        }

        params
    }
    if (inherits(plot_coord <- plot$coordinates, "CoordRadial")) {
        plot$coordinates <- ggproto(
            NULL, plot_coord,
            theta = coord$theta,
            r = coord$r,
            arc = coord$arc,
            direction = coord$direction,
            r_axis_inside = coord$r_axis_inside,
            expand = coord$expand,
            ...,
            setup_panel_params = setup_panel_params
        )
    } else {
        if (!isTRUE(plot$coordinates$default)) {
            cli_warn(c(
                sprintf(
                    "{.fn %s} is not supported in %s",
                    snake_class(plot_coord), layout_name
                ),
                i = sprintf("Will use {.fn %s} instead", snake_class(coord))
            ))
        }
        plot$coordinates <- ggproto(NULL, coord, ...,
            setup_panel_params = setup_panel_params
        )
    }
    plot
}

#' @param axes A character indicates which axes should be restricted.
#' @noRd
cartesian_coord <- function(axes, layout_name) {
    structure(list(axes = axes, layout_name = layout_name),
        class = "ggalign_cartesian_coord"
    )
}

#' @export
ggplot_add.ggalign_cartesian_coord <- function(object, plot, object_name) {
    plot$coordinates <- gguse_cartesian_coord(
        .subset2(plot, "coordinates"),
        .subset2(object, "axes"),
        .subset2(object, "layout_name")
    )
    plot
}

gguse_cartesian_coord <- function(coord, axes, layout_name) {
    UseMethod("gguse_cartesian_coord")
}

#' @export
gguse_cartesian_coord.CoordCartesian <- function(coord, axes, layout_name) coord

#' @export
gguse_cartesian_coord.CoordTrans <- function(coord, axes, layout_name) {
    # we only allow identity trans in the axis used to align observations
    identity_trans <- vapply(
        coord$trans[axes],
        function(trans) identical(trans$name, "identity"), logical(1L),
        USE.NAMES = FALSE
    )
    if (all(identity_trans)) {
        coord
    } else {
        cli_warn(c(
            sprintf(
                "Transformations in {.field {axes}} is not supported in %s",
                snake_class(coord), layout_name
            ),
            i = "Will use {.fn coord_cartesian} instead"
        ))
        ggplot2::coord_cartesian()
    }
}

#' @export
gguse_cartesian_coord.default <- function(coord, axes, layout_name) {
    cli_warn(c(
        sprintf(
            "{.fn %s} is not supported in %s",
            snake_class(coord), layout_name
        ),
        i = "Will use {.fn coord_cartesian} instead"
    ))
    ggplot2::coord_cartesian()
}

######################################################
gguse_facet <- function(plot, facet, ...) {
    plot$facet <- align_melt_facet(facet, plot$facet, ...)
    plot
}

align_melt_facet <- function(default, facet, ...) UseMethod("align_melt_facet")

#' @importFrom ggplot2 ggproto
#' @export
align_melt_facet.FacetGrid <- function(default, facet, ..., strict = FALSE) {
    if (inherits(facet, "FacetGrid")) {
        # re-dispatch parameters
        params <- facet$params
        # we always fix the grid rows and cols
        if (strict) { # Don't allow user change the rows and cols
            params$rows <- default$params$rows
            params$cols <- default$params$cols
        } else {
            params$rows <- default$params$rows %||% params$rows
            params$cols <- default$params$cols %||% params$cols
        }

        params$drop <- default$params$drop
        params$as.table <- default$params$as.table

        # if the default is free, it must be free
        params$free$x <- params$free$x || default$params$free$x
        params$space_free$x <- params$space_free$x ||
            default$params$space_free$x
        params$free$y <- params$free$y || default$params$free$y
        params$space_free$y <- params$space_free$x ||
            default$params$space_free$y
        ggproto(NULL, facet, params = params)
    } else {
        default
    }
}

#' @importFrom ggplot2 ggproto
#' @export
align_melt_facet.FacetWrap <- function(default, facet, ...) {
    if (inherits(facet, "FacetWrap")) {
        # re-dispatch parameters
        params <- facet$params

        # we always fix the grid rows and cols
        params$facets <- default$params$facets
        params$nrow <- default$params$nrow
        params$ncol <- default$params$ncol
        params$drop <- default$params$drop
        params$as.table <- default$params$as.table
        ggproto(NULL, facet, params = params)
    } else {
        default
    }
}

#' @export
align_melt_facet.FacetNull <- function(default, facet, ...) {
    if (inherits(facet, "FacetNull")) {
        facet
    } else {
        default
    }
}

#' @export
align_melt_facet.FacetStack <- function(default, facet, ...) {
    if (inherits(facet, "FacetGrid")) {
        params <- facet$params
        if (is_horizontal(.subset2(default, "direction"))) {
            # for horizontal stack, we cannot facet by rows
            if (!is.null(params$rows)) {
                cli_warn("Canno facet by rows in {.field {direction}} stack")
                params$rows <- NULL
            }
        } else if (!is.null(params$cols)) {
            # for vertical stack, we cannot facet by cols
            cli_warn("Canno facet by cols in {.field {direction}} stack")
            params$cols <- NULL
        }
        ggproto(NULL, facet, params = params)
    } else if (inherits(facet, "FacetNull")) {
        facet
    } else {
        ggplot2::facet_null()
    }
}

facet_stack <- function(direction) {
    structure(list(direction = direction), class = "FacetStack")
}
