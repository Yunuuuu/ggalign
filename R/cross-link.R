#' Add a plot to connect selected observations
#'
#' @param link A [`link_draw()`] object that defines how to draw the links,
#'   such as [`link_line()`].
#' @param on_top A boolean value indicating whether to draw the link on top of
#'   the plot panel (`TRUE`) or below (`FALSE`).
#' @inheritParams cross_none
#' @inheritParams ggalign
#'
#' @section ggplot2 Specification:
#' The `cross_link` function initializes a `ggplot` object but does not
#' initialize any data. Using [`scheme_data()`] to change the internal data if
#' needed.
#'
#' @export
cross_link <- function(link, data = waiver(), on_top = TRUE,
                       inherit_panel = NULL, inherit_nobs = NULL,
                       size = NULL, active = NULL) {
    if (!inherits(link, "ggalign_link_draw")) {
        cli_abort("{.arg link} must be a {.fn link_draw} object")
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    cross(CrossLink,
        data = data, link = link, plot = ggplot(), size = size,
        schemes = default_schemes(th = theme_no_panel()),
        active = active,
        on_top = on_top,
        inherit_panel = inherit_panel,
        inherit_nobs = inherit_nobs
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include cross-none.R
CrossLink <- ggproto("CrossLink", CrossNone,
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        if (is.null(.subset2(previous_design, "nobs"))) {
            cli_abort(
                sprintf(
                    "layout {.field nobs} for %s before %s is not initialized ",
                    self$layout_name, object_name(self)
                )
            )
        }
        if (is.null(.subset2(design, "nobs"))) {
            cli_abort(
                sprintf(
                    "layout {.field nobs} for %s after %s is not initialized ",
                    self$layout_name, object_name(self)
                )
            )
        }

        direction <- self$direction
        position <- self$position

        # parse links --------------------------------------------
        link <- self$link
        design1 <- previous_design
        design2 <- design
        full_data1 <- split(
            seq_len(.subset2(design1, "nobs")),
            .subset2(design1, "panel")
        )
        full_data2 <- split(
            seq_len(.subset2(design2, "nobs")),
            .subset2(design2, "panel")
        )
        links <- .subset2(link, "links")
        # set default links for link_line()
        if (is_empty(links) &&
            inherits(link, "ggalign_link_line") &&
            identical(.subset2(design1, "nobs"), .subset2(design2, "nobs"))) {
            links <- lapply(seq_len(.subset2(design1, "nobs")), function(i) {
                rlang::new_formula(i, i)
            })
            links <- pair_links(!!!links)
        }
        link_index <- lapply(links, make_pair_link_data,
            design1 = design1, design2 = design2,
            labels1 = self$labels0, labels2 = self$labels
        )
        names(link_index) <- names_or_index(links)
        data_index <- lapply(link_index, function(index) {
            if (is.null(index)) {
                return(NULL)
            }
            hand1 <- .subset2(index, "hand1")
            hand2 <- .subset2(index, "hand2")
            list(
                hand1 = .subset2(design1, "index")[hand1],
                hand2 = .subset2(design2, "index")[hand2]
            )
        })
        plot$ggalign_link_data <- list(
            full_data1 = full_data1,
            full_data2 = full_data2,
            link_index = link_index,
            data_index = data_index,
            direction = direction,
            draw = .subset2(link, "draw")
        )
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_schemes(plot, schemes)

        # save spacing for usage
        spacing <- calc_element(
            switch_direction(
                self$direction,
                "panel.spacing.y",
                "panel.spacing.x"
            ),
            theme
        ) %||% unit(0, "mm")

        # setup the grob
        grob <- inject(grid::grob(
            !!!.subset2(plot, "ggalign_link_data"),
            spacing1 = spacing,
            spacing2 = spacing,
            cl = "ggalignLinkGrob"
        ))
        plot$ggalign_link_data <- NULL

        # insert the grob
        plot + inset(grob, on_top = self$on_top) + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(CrossNone, self)$summary(plot)
        c(header, "  Add plot to connect selected observations")
    }
)
