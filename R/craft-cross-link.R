#' Add a plot to connect selected observations
#'
#' @param link A [`link_draw()`] object that defines how to draw the links,
#'   such as [`link_line()`].
#' @param on_top A boolean value indicating whether to draw the link on top of
#'   the plot panel (`TRUE`) or below (`FALSE`).
#' @inheritParams cross_none
#' @inheritParams ggmark
#'
#' @section ggplot2 Specification:
#' The `cross_link` function initializes a `ggplot` object but does not
#' initialize any data. Using [`scheme_data()`] to change the internal data if
#' needed.
#'
#' @export
cross_link <- function(link, data = waiver(), ...,
                       on_top = TRUE, obs_size = 1,
                       inherit_index = NULL, inherit_panel = NULL,
                       inherit_nobs = NULL,
                       size = NULL, active = NULL) {
    if (!inherits(link, "ggalign_link_draw")) {
        cli_abort("{.arg link} must be a {.fn link_draw} object")
    }
    assert_obs_size(obs_size)
    assert_active(active)
    active <- active(use = TRUE) + active
    cross(CrossLink,
        data = data, data_params = list2(...),
        link = link, obs_size = obs_size,
        plot = ggplot(), size = size,
        schemes = default_schemes(),
        active = active,
        on_top = on_top,
        inherit_nobs = inherit_nobs,
        inherit_panel = inherit_panel,
        inherit_index = inherit_index
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @importFrom grid gTree
#' @include craft-cross-.R
CrossLink <- ggproto("CrossLink", CraftCross,
    interact_layout = function(self, layout) {
        if (!self$in_linear) { # only used for linear coordinate
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(self), layout_name
                ),
                i = sprintf(
                    "%s can only be used in linear layout",
                    object_name(self)
                )
            ))
        }
        ggproto_parent(CraftCross, self)$interact_layout(layout)
    },
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        if (is.na(prop(previous_domain, "nobs"))) {
            cli_abort(
                sprintf(
                    "layout {.field nobs} for %s before %s is not initialized ",
                    self$layout_name, object_name(self)
                )
            )
        }
        if (is.na(prop(domain, "nobs"))) {
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
        domain1 <- previous_domain
        domain2 <- domain
        full_data1 <- split(
            seq_len(prop(domain1, "nobs")),
            prop(domain1, "panel")
        )
        full_data2 <- split(
            seq_len(prop(domain2, "nobs")),
            prop(domain2, "panel")
        )
        links <- .subset2(link, "links")
        # set default links for link_line()
        if (is_empty(links) &&
            inherits(link, "ggalign_link_line") &&
            identical(prop(domain1, "nobs"), prop(domain2, "nobs"))) {
            links <- lapply(seq_len(prop(domain1, "nobs")), function(i) {
                rlang::new_formula(i, i)
            })
            links <- pair_links(!!!links)
        }
        link_index <- make_links_data(
            links,
            domain1 = domain1, domain2 = domain2,
            labels1 = self$labels0, labels2 = self$labels
        )
        data_index <- lapply(link_index, function(index) {
            if (is.null(index)) {
                return(NULL)
            }
            hand1 <- .subset2(index, "hand1")
            hand2 <- .subset2(index, "hand2")
            list(
                hand1 = prop(domain1, "index")[hand1],
                hand2 = prop(domain2, "index")[hand2]
            )
        })
        plot$ggalign_link_data <- list(
            full_data1 = full_data1,
            full_data2 = full_data2,
            link_index = link_index,
            data_index = data_index,
            direction = direction,
            draw = .subset2(link, "draw"),
            obs_size = self$obs_size
        )
        plot
    },
    finish_plot = function(self, plot, schemes, theme) {
        plot <- plot_add_scheme(plot, schemes)

        # save spacing for usage
        spacing <- calc_element(
            switch_direction(
                self$direction,
                "panel.spacing.y",
                "panel.spacing.x"
            ),
            plot$theme
        ) %||% unit(0, "mm")

        # setup the grob
        grob <- inject(gTree(
            !!!plot$ggalign_link_data,
            spacing1 = spacing,
            spacing2 = spacing,
            cl = "ggalignLinkTree"
        ))
        plot$ggalign_link_data <- NULL

        # insert the grob
        plot <- plot + inset(grob, on_top = self$on_top)
        ggremove_margin(plot, self$direction) + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(CraftCross, self)$summary(plot)
        c(header, "  Add plot to connect selected observations")
    }
)
