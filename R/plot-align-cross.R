#' @importFrom ggplot2 ggproto aes
cross_link <- function(mapping = aes(), size = NULL,
                       no_axes = NULL, active = NULL) {
    assert_mapping(mapping)
    active <- update_active(active, new_active(
        use = TRUE, order = NA_integer_, name = NA_character_
    ))
    new_align_plot(
        align_class = ggproto(
            NULL,
            CrossLink,
            params = list(mapping = mapping)
        ),
        size = size, no_axes = no_axes, active = active,
        class = "ggalign_cross_link"
    )
}

is_cross_link <- function(x) inherits(x, "ggalign_cross_link")

#' @importFrom ggplot2 ggproto ggplot
CrossLink <- ggproto("CrossLink", AlignProto,
    initialize = function(self, direction, position, layout_coords,
                          object_name, layout_name) {
        if (is.null(.subset2(layout_coords, "nobs"))) {
            cli_abort(sprintf(
                "layout observations for %s must be initialized before adding {.var {object_name}}",
                layout_name
            ))
        }
        layout_coords["index"] <- list(NULL)
        layout_coords
    },
    ggplot = function(self, mapping, direction) {
        if (is_horizontal(direction)) {
            default_mapping <- aes(y = .data$y)
            default_expand <- default_expansion(x = expansion())
        } else {
            default_mapping <- aes(x = .data$x)
            default_expand <- default_expansion(y = expansion())
        }
        ggplot(mapping = add_default_mapping(mapping, default_mapping)) +
            default_expand
    },
    finish = function(layout, layout_coords) {
        # udpate cross_points
        layout@cross_points <- c(layout@cross_points, length(layout@plot_list))
        # update index
        layout@index_list <- c(
            layout@index_list,
            list(.subset2(layout_coords, "index"))
        )
        layout
    },
    build = function(plot, coords, previous_coords, direction) {
        data <- data_frame0(
            panel = vec_c(
                .subset2(previous_coords, "panel"),
                .subset2(coords, "panel")
            ),
            index = vec_c(
                .subset2(previous_coords, "index"),
                .subset2(coords, "index")
            ),
            hand = factor(
                vec_rep_each(
                    c("left", "right"),
                    c(.subset2(previous_coords, "nobs"), .subset2(coords, "nobs"))
                ),
                c("left", "right")
            )
        )
        data[[switch_direction(direction, "y", "x")]] <- vec_c(
            seq_len(.subset2(previous_coords, "nobs")),
            seq_len(.subset2(coords, "nobs"))
        )
        if (nlevels(.subset2(coords, "panel")) > 1L) {
            default_facet <- switch_direction(
                direction,
                ggplot2::facet_grid(
                    rows = ggplot2::vars(fct_rev(.data$panel)),
                    scales = "free_y", space = "free",
                    drop = FALSE
                ),
                ggplot2::facet_grid(
                    cols = ggplot2::vars(.data$panel),
                    scales = "free_x", space = "free",
                    drop = FALSE
                )
            )
        } else {
            default_facet <- ggplot2::facet_null()
        }
        plot$data <- data
        plot +
            align_melt_facet(plot$facet, default_facet, direction) +
            switch_direction(
                direction,
                coord_ggalign(y = coords),
                coord_ggalign(x = coords)
            )
    }
)
