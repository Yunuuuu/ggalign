#' Layer with customized draw function
#'
#' @inheritParams ggplot2::layer
#' @param ... Additional arguments passed on to `draw` aesthetic.
#' @inheritParams ggplot2::geom_point
#' @details
#' `geom_draw2` depends on the new aesthetics `draw`, which should always be
#' provided with [`scale_draw_manual()`], in which, we can provide a list of
#' functions that define how each value should be drawn. Any ggplot2 aesthetics
#' can be used as the arguments.
#'
#' @eval rd_gg_aesthetics("geom", "draw2")
#' @examples
#' library(grid)
#' ggplot(data.frame(value = letters[seq_len(5)], y = seq_len(5))) +
#'     geom_draw2(aes(x = 1, y = y, draw = value, fill = value)) +
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
#'
#' @importFrom rlang list2
#' @export
geom_draw2 <- function(mapping = NULL, data = NULL, stat = "identity",
                       position = "identity", ..., na.rm = FALSE,
                       show.legend = NA, inherit.aes = TRUE) {
    ggplot2::layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomDraw2,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, params = list2(...))
    )
}

call_draw2 <- function(draw, data, params) {
    args <- formalArgs(draw)
    if (any(args == "...")) {
        inject(draw(!!!data, !!!params))
    } else {
        inject(draw(
            !!!.subset(data, vec_set_intersect(names(data), args)),
            !!!params
        ))
    }
}

#' @inherit ggplot2::draw_key_point
#' @description
#' Each geom has an associated function that draws the key when the geom needs
#' to be displayed in a legend. These functions are called `draw_key_*()`, where
#' `*` stands for the name of the respective key glyph. The key glyphs can be
#' customized for individual geoms by providing a geom with the `key_glyph`
#' argument. The `draw_key_draw2` function provides this interface for custom
#' key glyphs used with [`geom_draw2()`].
#'
#' @importFrom rlang inject
#' @importFrom methods formalArgs
#' @importFrom ggplot2 zeroGrob
#' @importFrom grid gTree
#' @export
draw_key_draw2 <- function(data, params, size) {
    draw <- .subset2(data$draw, 1L)
    if (!is.function(draw)) return(zeroGrob()) # styler: off
    data$draw <- NULL
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
                color = data$colour %||% GeomDraw2$default_aes[["colour"]],
                fill = data$colour %||% GeomDraw2$default_aes[["fill"]],
                GeomDraw2$default_aes[[aes]]
            )
        }
    }
    ans <- call_draw2(draw, data, .subset2(params, "params"))
    if (is.gList(ans)) ans <- gTree(children = ans)
    if (is.grob(ans)) {
        ans
    } else {
        zeroGrob()
    }
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

#' @importFrom ggplot2 ggproto
#' @importFrom rlang inject
#' @importFrom grid gList
GeomDraw2 <- ggproto(
    "GeomDraw2",
    ggplot2::GeomTile, # will respect width and height
    required_aes = "draw",
    default_aes = combine_aes(
        aes(x = 0.5, y = 0.5),
        ggplot2::GeomPoint$default_aes,
        ggplot2::GeomSegment$default_aes
    ),
    draw_panel = function(data, panel_params, coord, params) {
        coords <- coord$transform(data, panel_params)

        # restore width and height
        coords$width <- coords$xmax - coords$xmin
        coords$height <- coords$ymax - coords$ymin
        coords$color <- coords$colour

        # restore width and height
        indices <- vec_group_loc(.subset2(coords, "draw"))

        # reordering by drawing order
        ordering <- vapply(
            .subset2(indices, "key"),
            function(draw) {
                attr(draw, "drawing_order", exact = TRUE) %||% NA_integer_
            }, integer(1L),
            USE.NAMES = FALSE
        )
        indices <- vec_slice(indices, order(ordering))
        coords$draw <- NULL
        coords <- vec_chop(coords, indices = .subset2(indices, "loc"))

        grobs <- .mapply(function(draw, data) {
            if (!is.function(draw)) return(NULL) # styler: off
            ans <- call_draw2(draw, data, params)
            if (is.gList(ans)) {
                gTree(children = ans)
            } else {
                ans
            }
        }, list(draw = .subset2(indices, "key"), data = coords), NULL)
        grobs <- grobs[vapply(grobs, is.grob, logical(1L), USE.NAMES = FALSE)]
        if (is_empty(grobs)) {
            zeroGrob()
        } else {
            gTree(children = inject(gList(!!!grobs)))
        }
    },
    draw_key = draw_key_draw2
)

#' Scale for `draw` aesthetic
#'
#' @inheritDotParams ggplot2::discrete_scale -expand -position -aesthetics -palette -scale_name
#' @param values A list of functions (including purrr-like lambda syntax) that
#' define how each cell's grob (graphical object) should be drawn.
#' @inheritParams ggplot2::scale_discrete_manual
#' @inherit geom_draw
#' @export
scale_draw_manual <- function(..., values, aesthetics = "draw",
                              breaks = waiver(), na.value = NA) {
    ggplot2::scale_discrete_manual(
        aesthetics = aesthetics,
        values = .mapply(function(f, i) {
            f <- allow_lambda(f)
            attr(f, "drawing_order") <- i # save the drawing order
            f
        }, list(values, seq_along(values)), NULL),
        breaks = breaks,
        na.value = na.value,
        ...
    )
}

# `draw` should be provided manually
scale_draw_discrete <- function(name = waiver(), ...) {
    cli_abort(
        "You must provide {.fn scale_draw_manual} to use {.fn geom_draw2}"
    )
}
