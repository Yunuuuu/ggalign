#' @export
#' @rdname align_link
align_range <- function(data = waiver(), mapping = aes(),
                        ranges = waiver(), position = waiver(),
                        size = NULL, active = NULL) {
    assert_layout_position(position)
    if (inherits(data, "uneval")) {
        cli_abort(c(
            "{.arg data} cannot be {.obj_type_friendly {data}}",
            "i" = "Have you misspelled the {.arg data} argument in {.fn ggalign}"
        ))
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    align(
        new_align_link(
            "AlignRange",
            arg = "ranges",
            class = "align_range_plot",
            element = "plot.ggalign_ranges"
        ),
        plot = ggplot(mapping = mapping),
        size = size, data = data,
        params = list(ranges = ranges, position = position),
        schemes = new_schemes(),
        active = active
    )
}

#' @importFrom ggplot2 ggproto
#' @export
alignpatch.align_range_plot <- function(x) {
    ggproto(NULL, PatchAlignRangePlot, plot = x)
}

#' @export
`[.alignRangeGtable` <- function(x, i, j) {
    # subset will violate the `alignRangeGtable` `shape`
    # we always use the next method
    class(x) <- setdiff(class(x), "alignRangeGtable")
    x$links_data <- NULL
    NextMethod()
}

#' @include align-link.R
PatchAlignRangePlot <- ggproto(
    "PatchAlignRangePlot", PatchAlignLinkProto,
    patch_gtable = function(self, plot = self$plot) {
        ans <- ggproto_parent(PatchAlignLinkProto, self)$patch_gtable(
            plot = plot
        )
        # re-define the draw method, we assign new class
        ans <- add_class(ans, "alignRangeGtable")
        ans$links_data <- .subset2(plot, "links_data")
        ans
    }
)

# preDraw:
#  - makeContext
#  - pushvpgp
#  - preDrawDetails: by default, do noting
# makeContent:
# drawDetails:
# postDraw:
#  - postDrawDetails: by default, do noting
#  - popgrobvp
#' @importFrom grid makeContent unit convertHeight convertWidth viewport
#' @export
makeContent.alignRangeGtable <- function(x) {
    # Grab viewport information
    width <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
    height <- convertHeight(unit(1, "npc"), "mm", valueOnly = TRUE)

    # Grab grob metadata
    plot_widths <- compute_null_width(.subset2(x, "widths"),
        valueOnly = TRUE
    )
    # from top to the bottom
    plot_heights <- compute_null_height(.subset2(x, "heights"),
        valueOnly = TRUE
    )
    panel_loc <- find_panel(x)
    range_data <- .subset2(x, "links_data")
    breaks <- .subset2(range_data, "breaks")
    link_position <- .subset2(range_data, "link_position")
    full_breaks <- .subset2(range_data, "full_breaks")
    direction <- .subset2(range_data, "direction")
    spacing <- convertHeight(
        .subset2(range_data, "spacing"), "mm",
        valueOnly = TRUE
    )

    # each break represent an `observation`, for panel space, we use `NA`
    # obs arranged from left to top, and from bottom to top
    obs <- unlist(vec_interleave(full_breaks, list(NA)), FALSE, FALSE)
    obs <- obs[-length(obs)] # remove the last panel space, shouldn't exist
    sizes <- numeric(length(obs))
    sizes[is.na(obs)] <- spacing
    n_spacing <- length(full_breaks) - 1L

    # then, we define the link grobs
    coord_x <- coord_y <- numeric()
    if (is_horizontal(direction)) { # the link should be in left or right
        sizes[!is.na(obs)] <- (height - spacing * n_spacing) /
            sum(lengths(full_breaks)) # nobs
        cum_sizes <- cumsum(sizes)
        # from bottom to the top, following the ordering of the `breaks`
        panel_index <- seq(
            from = .subset2(panel_loc, "b"),
            to = .subset2(panel_loc, "t"),
            length.out = length(breaks)
        )
        # we'll reverse the `plot_cum_heights`, so the ordering index should
        # also be reversed
        panel_index <- nrow(x) - panel_index + 1L
        l_border <- plot_widths[seq_len(.subset2(panel_loc, "l") - 1L)]
        r_border <- plot_widths[-seq_len(.subset2(panel_loc, "r"))]
        # for a gtable, heights are from top to the bottom,
        # we reverse the heights
        plot_cum_heights <- cumsum(rev(plot_heights))
        for (position in link_position) {
            for (i in seq_along(panel_index)) {
                # we match the observations
                pos <- match(.subset2(breaks, i), obs)
                coord_y <- c(
                    coord_y,
                    # for height next to the plot panel
                    plot_cum_heights[panel_index[i] + (-1:0)],
                    # for height in the border
                    c(
                        cum_sizes[max(pos)],
                        cum_sizes[min(pos)] - sizes[min(pos)]
                    )
                )
                if (position == "left") {
                    coord_x <- c(
                        coord_x,
                        vec_rep_each(c(sum(l_border), 0), 2L)
                    )
                } else {
                    coord_x <- c(
                        coord_x,
                        vec_rep_each(c(width - sum(r_border), width), 2L)
                    )
                }
            }
        }
    } else {
        sizes[!is.na(obs)] <- (width - spacing * n_spacing) /
            sum(lengths(full_breaks)) # nobs
        cum_sizes <- cumsum(sizes)
        panel_index <- seq(
            from = .subset2(panel_loc, "l"),
            to = .subset2(panel_loc, "r"),
            length.out = length(breaks)
        )
        t_border <- plot_heights[seq_len(.subset2(panel_loc, "t") - 1L)]
        b_border <- plot_heights[-seq_len(.subset2(panel_loc, "b"))]
        plot_cum_widths <- cumsum(plot_widths)
        for (position in link_position) {
            for (i in seq_along(panel_index)) {
                # we match the observations
                pos <- match(.subset2(breaks, i), obs)
                coord_x <- c(
                    coord_x,
                    # for width next to the plot panel
                    plot_cum_widths[panel_index[i] + (-1:0)],
                    # for width in the border
                    c(
                        cum_sizes[max(pos)],
                        cum_sizes[min(pos)] - sizes[min(pos)]
                    )
                )
                if (position == "bottom") {
                    coord_y <- c(
                        coord_y,
                        vec_rep_each(c(sum(b_border), 0), 2L)
                    )
                } else {
                    coord_y <- c(
                        coord_y,
                        vec_rep_each(c(height - sum(t_border), height), 2L)
                    )
                }
            }
        }
    }

    layout <- .subset2(x, "layout")
    panels <- layout[grepl("^panel", .subset2(layout, "name")), , drop = FALSE]
    x <- gtable_add_grob(
        x,
        grobs = ggplot2::element_grob(
            .subset2(range_data, "element"),
            x = coord_x, y = coord_y,
            id.lengths = vec_rep(4L, length(coord_x) / 4L),
            default.units = "mm"
        ),
        t = 1L, l = 1L, b = -1L, r = -1L,
        # always draw with panel area
        z = min(panels$z)
    )
    NextMethod()
}
