#' Layer with a customized shape graphic using grid functions.
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @details
#' `geom_gshape` depends on the new aesthetics `gshape` (shape with grid
#' functions), which should always be provided with [`scale_gshape_manual()`],
#' in which, we can provide a list of grobs or functions that define how each
#' value should be drawn. Any ggplot2 aesthetics can be used as the arguments.
#'
#' @eval rd_gg_aesthetics("geom", "gshape")
#' @examples
#' library(grid)
#' ggplot(data.frame(value = letters[seq_len(5)], y = seq_len(5))) +
#'     geom_gshape(aes(x = 1, y = y, gshape = value, fill = value)) +
#'     scale_gshape_manual(values = list(
#'         a = function(x, y, width, height, fill) {
#'             rectGrob(x, y,
#'                 width = width, height = height,
#'                 gp = gpar(fill = fill),
#'                 default.units = "native"
#'             )
#'         },
#'         b = function(x, y, width, height, fill) {
#'             rectGrob(x, y,
#'                 width = width, height = height,
#'                 gp = gpar(fill = fill),
#'                 default.units = "native"
#'             )
#'         },
#'         c = function(x, y, width, height, fill) {
#'             rectGrob(x, y,
#'                 width = width, height = height,
#'                 gp = gpar(fill = fill),
#'                 default.units = "native"
#'             )
#'         },
#'         d = function(x, y, width, height, shape) {
#'             gList(
#'                 pointsGrob(x, y, pch = shape),
#'                 # To ensure the rectangle color is shown in the legends, you
#'                 # must explicitly provide a color argument and include it in
#'                 # the `gpar()` of the graphical object
#'                 rectGrob(x, y, width, height,
#'                     gp = gpar(col = "black", fill = NA)
#'                 )
#'             )
#'         },
#'         e = function(xmin, xmax, ymin, ymax) {
#'             segmentsGrob(
#'                 xmin, ymin,
#'                 xmax, ymax,
#'                 gp = gpar(lwd = 2)
#'             )
#'         }
#'     )) +
#'     scale_fill_brewer(palette = "Dark2") +
#'     theme_void()
#'
#' @importFrom rlang list2
#' @export
geom_gshape <- function(mapping = NULL, data = NULL, stat = "identity",
                        position = "identity", ..., na.rm = FALSE,
                        show.legend = NA, inherit.aes = TRUE) {
    dots <- list2(...)
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomGshape,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = c(list(na.rm = na.rm), dots, list(.__gshape_dots__ = dots))
    )
}

#' @inherit ggplot2::draw_key_point
#' @description
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument. The `draw_key_gshape` function provides this interface for custom
#' key glyphs used with [`geom_gshape()`].
#'
#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom grid gTree
#' @export
draw_key_gshape <- function(data, params, size) {
    gshape <- .subset2(data$gshape, 1L)
    data$gshape <- NULL
    if (is.function(gshape)) {
        for (d in formalArgs(gshape)) {
            if (is.null(.subset2(data, d))) {
                data[[d]] <- switch(d,
                    x = ,
                    y = 0.5,
                    xmin = ,
                    ymin = 0,
                    xmax = ,
                    ymax = 1,
                    width = ,
                    height = 1,
                    color = data$colour %||% GeomGshape$default_aes[["colour"]],
                    fill = data$colour %||% GeomGshape$default_aes[["fill"]],
                    GeomGshape$default_aes[[d]]
                )
            }
        }
    }
    make_gshape(gshape, data, .subset2(params, ".__gshape_dots__"))
}

#' @return A [grob][grid::grob] object.
#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom ggplot2 zeroGrob
#' @keywords internal
#' @noRd
make_gshape <- function(gshape, data, dots) {
    if (is.function(gshape)) {
        args <- formalArgs(gshape)
        if (any(args == "...")) {
            gshape <- inject(gshape(!!!data, !!!dots))
        } else {
            gshape <- inject(gshape(
                !!!.subset(data, intersect(names(data), args)),
                !!!.subset(dots, intersect(names(dots), args))
            ))
        }
    }
    if (is.gList(gshape)) gshape <- gTree(children = gshape)
    if (is.grob(gshape)) {
        gshape
    } else {
        zeroGrob()
    }
}

#' @importFrom ggplot2 ggproto zeroGrob
#' @importFrom rlang inject
#' @importFrom grid gList
GeomGshape <- ggproto(
    "GeomGshape",
    ggplot2::Geom,
    required_aes = c("x", "y", "gshape"),
    default_aes = aes(
        shape = 19,
        colour = "black",
        size = 1.5,
        fill = NA,
        alpha = NA,
        stroke = 0.5,
        linewidth = 0.5,
        linetype = 1
    ),
    setup_data = ggplot2::GeomTile$setup_data,
    draw_panel = function(data, panel_params, coord, .__gshape_dots__) {
        coords <- coord$transform(data, panel_params)

        if (!is.null(coords$colour) && is.null(coords$color)) {
            coords$color <- coords$colour
        }
        if (!is.null(coords$color) && is.null(coords$colour)) {
            coords$colour <- coords$color
        }

        # restore width and height
        if (!is.null(coords$xmin) && !is.null(coords$xmax)) {
            coords$width <- coords$xmax - coords$xmin
        }
        if (!is.null(coords$ymin) && !is.null(coords$ymax)) {
            coords$height <- coords$ymax - coords$ymin
        }

        groups <- vec_group_loc(.subset2(coords, "gshape"))
        coords$gshape <- NULL

        # reordering by drawing order
        ordering <- vapply(
            .subset2(groups, "key"),
            function(gshape) {
                attr(gshape, "gshape_ordering", exact = TRUE) %||% NA_integer_
            },
            integer(1L),
            USE.NAMES = FALSE
        )
        groups <- vec_slice(groups, order(ordering))
        coords <- vec_chop(coords, indices = .subset2(groups, "loc"))

        grobs <- .mapply(
            make_gshape,
            list(gshape = .subset2(groups, "key"), data = coords),
            list(dots = .__gshape_dots__)
        )
        grobs <- grobs[vapply(grobs, is.grob, logical(1L), USE.NAMES = FALSE)]
        if (is_empty(grobs)) {
            zeroGrob()
        } else {
            gTree(children = inject(gList(!!!grobs)))
        }
    },
    draw_key = draw_key_gshape
)

#' Scale for `gshape` aesthetic
#'
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics -palette -scale_name
#' @param values A list of grobs or functions (including purrr-like lambda
#' syntax) that define how each cell's grob (graphical object) should be drawn.
#' @inheritParams ggplot2::scale_discrete_manual
#' @inherit geom_gshape
#' @export
scale_gshape_manual <- function(..., values, breaks = waiver(), na.value = NA) {
    ggplot2::scale_discrete_manual(
        aesthetics = "gshape",
        values = .mapply(function(f, i) {
            f <- allow_lambda(f)
            attr(f, "gshape_ordering") <- i # save the drawing order
            f
        }, list(values, seq_along(values)), NULL),
        breaks = breaks,
        na.value = na.value,
        ...
    )
}

# `gshape` should be provided manually
scale_gshape_discrete <- function(name = waiver(), ...) {
    cli_abort(paste(
        "You must provide {.fn scale_gshape_manual}",
        "to use {.field draw} aesthetic"
    ))
}

# `gshape` should be provided manually
scale_gshape_continuous <- scale_gshape_discrete
