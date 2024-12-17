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
#' @param axes A character indicates which axes should be restricted.
#' @noRd
cartesian_coord <- function(axes, layout_name) {
    structure(list(axes = axes, layout_name = layout_name),
        class = "ggalign_cartesian_coord"
    )
}

#' @export
ggplot_add.ggalign_cartesian_coord <- function(object, plot, object_name) {
    plot$coordinates <- use_cartesian_coord(
        .subset2(plot, "coordinates"),
        .subset2(object, "axes"),
        .subset2(object, "layout_name")
    )
    plot
}

use_cartesian_coord <- function(coord, axes, layout_name) {
    UseMethod("use_cartesian_coord")
}

#' @export
use_cartesian_coord.CoordCartesian <- function(coord, axes, layout_name) coord

#' @export
use_cartesian_coord.CoordTrans <- function(coord, axes, layout_name) {
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
use_cartesian_coord.default <- function(coord, axes, layout_name) {
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
