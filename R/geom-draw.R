#' Layer with customized draw function
#'
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_point
#' @details
#' `geom_draw` depends on the new aesthetics `draw`, which should always be
#' provided with [`scale_draw_manual()`], in which, we can provide a list of
#' functions that define how each cell's grob (graphical object) should be
#' drawn. This aesthetic allows you to replace the default rendering of cells
#' with custom behavior, making it possible to tailor the plot to specific
#' requirements.
#' @eval rd_gg_aesthetics("geom", "draw")
#' @examples
#' library(grid)
#' ggplot(data.frame(value = letters[seq_len(5)], y = seq_len(5))) +
#'     geom_draw(aes(x = 1, y = y, draw = value, fill = value)) +
#'     scale_draw_manual(values = list(
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
#' @export
geom_draw <- function(mapping = NULL, data = NULL, stat = "identity",
                      position = "identity", ...,
                      na.rm = FALSE,
                      show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomDraw,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#' @inherit ggplot2::draw_key_point
#' @description
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `⁠draw_key_*()`⁠,
#' where `*` stands for the name of the respective key glyph. The key glyphs can
#' be customized for individual geoms by providing a geom with the `key_glyph`
#' argument. The `draw_key_draw` function provides this interface for custom key
#' glyphs used with [`geom_draw()`].
#'
#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @export
draw_key_draw <- function(data, params, size) {
    if (is.null(draw <- .subset2(data$draw, 1L))) {
        cli::cli_abort(
            "{.fn draw_key_draw} can be used only for {.field draw} aesthetic"
        )
    }
    data$draw <- NULL
    if (!is.null(data$.draw)) data$.draw <- NULL
    args <- formalArgs(draw)
    for (aes in args) {
        if (is.null(.subset2(data, aes))) {
            data[[aes]] <- switch(aes,
                x = ,
                y = 0.5,
                xmin = ,
                ymin = 0,
                xmax = ,
                ymax = 1,
                width = ,
                height = 1,
                color = data$colour %||% GeomDraw$default_aes[["colour"]],
                fill = data$colour %||% GeomDraw$default_aes[["fill"]],
                GeomDraw$default_aes[[aes]]
            )
        }
    }
    if (any(args == "...")) {
        ans <- inject(draw(!!!data))
    } else {
        ans <- inject(draw(!!!.subset(data, args)))
    }
    if (inherits(ans, "gList")) ans <- grid::gTree(children = ans)
    ans
}

combine_aes <- function(...) {
    ans <- ...elt(1L)
    for (i in 2:...length()) {
        mapping <- ...elt(i)
        for (nm in names(mapping)) {
            ans[[nm]] <- .subset2(mapping, nm)
        }
    }
    ans
}

#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom ggplot2 ggproto
GeomDraw <- ggproto(
    "GeomDraw",
    ggplot2::Geom,
    optional_aes = c("x", "y"),
    default_aes = combine_aes(
        ggplot2::GeomPoint$default_aes,
        ggplot2::GeomSegment$default_aes,
        ggplot2::GeomRect$default_aes,
        aes(lineend = "butt", linejoin = "mitre")
    ),
    required_aes = "draw",
    setup_data = ggplot2::GeomTile$setup_data,
    draw_panel = function(data, panel_params, coord, ...) {
        coords <- coord$transform(data, panel_params)
        # restore width and height
        coords$width <- coords$xmax - coords$xmin
        coords$height <- coords$ymax - coords$ymin
        coords$color <- coords$colour
        grobs <- .mapply(function(draw, ..., PANEL, group) {
            args <- formalArgs(draw)
            if (any(args == "...")) {
                draw(...)
            } else {
                inject(draw(!!!.subset(list(...), args)))
            }
        }, coords, NULL)
        inject(grid::gList(!!!grobs))
    },
    draw_key = draw_key_draw
)

#' Scale for `draw` aesthetic
#'
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics -palette -scale_name
#' @param values A list of functions that define how each cell's grob (graphical
#' object) should be drawn.
#' @inheritParams ggplot2::scale_discrete_manual
#' @inherit geom_draw
#' @export
scale_draw_manual <- function(..., values, aesthetics = "draw",
                              breaks = waiver(), na.value = NA) {
    ggplot2::scale_discrete_manual(
        aesthetics = aesthetics,
        values = values,
        breaks = breaks,
        na.value = na.value,
        ...
    )
}

# `draw` should be provided manually
scale_draw_discrete <- function(name = waiver(), ...) {
    cli::cli_abort(
        "You must provide {.field draw} scale with {.fn scale_draw_manual}"
    )
}
