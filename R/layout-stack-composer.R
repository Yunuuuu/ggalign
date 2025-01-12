#' @keywords internal
stack_composer <- function(direction) {
    structure(
        list(
            t = integer(), l = integer(),
            b = integer(), r = integer(),
            plots = list(), sizes = list(),
            direction = direction,
            align = 1L,
            # attributes used by `stack_layout()`
            left_or_top = FALSE, right_or_bottom = FALSE
        ),
        class = "stack_composer"
    )
}

stack_composer_add_plot <- function(composer, plot, t, l, b = t, r = l) {
    composer$t <- c(.subset2(composer, "t"), t)
    composer$l <- c(.subset2(composer, "l"), l)
    composer$b <- c(.subset2(composer, "b"), b)
    composer$r <- c(.subset2(composer, "r"), r)
    composer$plots <- c(.subset2(composer, "plots"), list(plot))
    composer
}

#' @importFrom rlang is_empty
stack_composer_align_plot <- function(composer, plot, size) {
    if (is.null(plot)) {
        return(composer)
    }
    if (is_horizontal(.subset2(composer, "direction"))) {
        r_border <- .subset2(composer, "r")
        if (is_empty(r_border)) {
            l <- 1L
        } else {
            l <- max(r_border) + 1L
        }
        t <- .subset2(composer, "align")
    } else {
        b_border <- .subset2(composer, "b")
        if (is_empty(b_border)) {
            t <- 1L
        } else {
            t <- max(b_border) + 1L
        }
        l <- .subset2(composer, "align")
    }
    composer$sizes <- c(.subset2(composer, "sizes"), list(size))
    stack_composer_add_plot(composer, plot, t, l)
}

stack_composer_add <- function(plot, composer, ...) {
    UseMethod("stack_composer_add")
}

#' @importFrom utils packageVersion
#' @export
stack_composer_add.ggalign_plot <- function(plot, composer, design, ...,
                                            schemes, theme,
                                            released_spaces,
                                            direction, position) {
    size <- plot@size

    # for `released_spaces`, release the `free_spaces` in a single plot
    plot_schemes <- inherit_schemes(plot@schemes, schemes)
    if (!is.null(released_spaces)) {
        plot_spaces <- .subset2(
            .subset2(plot_schemes, "scheme_align"), "free_spaces"
        )
        if (is_string(plot_spaces)) {
            plot_spaces <- setdiff_position(plot_spaces, released_spaces)
            if (!nzchar(plot_spaces)) plot_spaces <- NULL
            plot_schemes$scheme_align["free_spaces"] <- list(plot_spaces)
        }
    }

    # let `Align` to determine how to build the plot
    align <- plot@align # `AlignProto` object
    plot <- plot@plot
    if (!align$free_facet && is_discrete_design(design)) {
        if (nlevels(.subset2(design, "panel")) > 1L) {
            facet <- switch_direction(
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
            facet <- facet_stack(direction, align$layout_name)
        }
        plot <- gguse_facet(plot, facet)
    }
    if (!align$free_coord) {
        plot <- gguse_linear_coord(plot, layout_name = align$layout_name)
    }

    # set limits and default scales
    if (!align$free_limits) {
        if (is_horizontal(direction)) {
            plot <- plot + ggalign_design(
                y = design,
                ylabels = .subset(align$labels, .subset2(design, "index"))
            )
        } else {
            plot <- plot + ggalign_design(
                x = design,
                xlabels = .subset(align$labels, .subset2(design, "index"))
            )
        }
    }

    # let `align` add other components
    plot <- align$build_plot(plot, design = design, ...)
    plot <- align$finish_plot(plot, plot_schemes, theme)
    stack_composer_align_plot(composer, plot, size)
}

#' @importFrom grid unit.c unit
stack_composer_add.QuadLayout <- function(plot, composer, schemes, theme,
                                          direction, ...) {
    patches <- quad_build(plot, schemes, theme, direction)
    plots <- .subset2(patches, "plots")
    sizes <- .subset2(patches, "sizes")

    if (is_horizontal(.subset2(composer, "direction"))) {
        composer$left_or_top <- .subset2(composer, "left_or_top") ||
            !is.null(.subset2(plots, "top"))
        composer$right_or_bottom <- .subset2(composer, "right_or_bottom") ||
            !is.null(.subset2(plots, "bottom"))
        composer <- stack_composer_align_plot(
            composer,
            .subset2(plots, "left"),
            .subset2(sizes, "left")
        )
        composer <- stack_composer_align_plot(
            composer,
            .subset2(plots, "main"),
            .subset2(.subset2(sizes, "main"), "width")
        )
        l <- max(.subset2(composer, "r"))
        if (!is.null(top <- .subset2(plots, "top"))) {
            if (.subset2(composer, "align") == 1L) {
                composer$t <- .subset2(composer, "t") + 1L
                composer$b <- .subset2(composer, "b") + 1L
                composer$align <- .subset2(composer, "align") + 1L
            }
            if (!is_null_unit(size <- .subset2(sizes, "top"))) {
                attr(top, "vp")$height <- size
            }
            composer <- stack_composer_add_plot(composer, top, t = 1L, l = l)
        }
        if (!is.null(bottom <- .subset2(plots, "bottom"))) {
            if (!is_null_unit(size <- .subset2(sizes, "bottom"))) {
                attr(bottom, "vp")$height <- size
            }
            composer <- stack_composer_add_plot(composer, bottom,
                t = .subset2(composer, "align") + 1L, l = l
            )
        }
        composer <- stack_composer_align_plot(
            composer,
            .subset2(plots, "right"),
            .subset2(sizes, "right")
        )
    } else {
        composer$left_or_top <- .subset2(composer, "left_or_top") ||
            !is.null(.subset2(plots, "left"))
        composer$right_or_bottom <- .subset2(composer, "right_or_bottom") ||
            !is.null(.subset2(plots, "right"))
        composer <- stack_composer_align_plot(
            composer,
            .subset2(plots, "top"),
            .subset2(sizes, "top")
        )
        composer <- stack_composer_align_plot(
            composer,
            .subset2(plots, "main"),
            .subset2(.subset2(sizes, "main"), "height")
        )
        t <- max(.subset2(composer, "b"))
        if (!is.null(left <- .subset2(plots, "left"))) {
            if (.subset2(composer, "align") == 1L) {
                composer$l <- .subset2(composer, "l") + 1L
                composer$r <- .subset2(composer, "r") + 1L
                composer$align <- .subset2(composer, "align") + 1L
            }
            if (!is_null_unit(size <- .subset2(sizes, "left"))) {
                attr(left, "vp")$width <- size
            }
            composer <- stack_composer_add_plot(composer, left, t = t, l = 1L)
        }
        if (!is.null(right <- .subset2(plots, "right"))) {
            if (!is_null_unit(size <- .subset2(sizes, "right"))) {
                attr(right, "vp")$width <- size
            }
            composer <- stack_composer_add_plot(composer, right,
                t = t, l = .subset2(composer, "align") + 1L
            )
        }
        composer <- stack_composer_align_plot(
            composer,
            .subset2(plots, "bottom"),
            .subset2(sizes, "bottom")
        )
    }
    composer
}

#' @export
stack_composer_add.list <- function(plot, composer, ...) {
    for (p in plot) {
        composer <- stack_composer_add(plot = p, composer = composer, ...)
    }
    composer
}
