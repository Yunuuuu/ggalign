#' Add a plot to annotate observations
#'
#' @inheritParams cross_none
#' @inheritParams ggmark
#'
#' @section ggplot2 Specification:
#' The `cross_mark` function initializes a `ggplot` object. The underlying data
#' contains following columns:
#'
#'  - `.panel`: the panel for the aligned axis. It means `x-axis` for vertical
#'    stack layout (including top and bottom annotation), `y-axis` for
#'    horizontal stack layout (including left and right annotation).
#'
#'  - `.names` ([`vec_names()`][vctrs::vec_names]) and `.index`
#'    ([`vec_size()`][vctrs::vec_size()]/[`NROW()`]): a character names (only
#'    applicable when names exists) and an integer of index of the original
#'    data.
#'
#'  - `.hand`: A factor with levels `c("left", "right")` for horizontal stack
#'    layouts, or `c("top", "bottom")` for vertical stack layouts, indicating
#'    the position of the linked observations.
#'
#' You can use [`scheme_data()`] to modify the internal data if needed.
#'
#' @export
cross_mark <- function(mark, data = waiver(), ...,
                       obs_size = 1, inherit_index = NULL, inherit_panel = NULL,
                       inherit_nobs = NULL,
                       size = NULL, active = NULL) {
    if (!inherits(mark, "ggalign_mark_draw")) {
        cli_abort("{.arg mark} must be a {.fn mark_draw} object")
    }
    assert_obs_size(obs_size)
    assert_active(active)
    active <- active(use = TRUE) + active
    cross(CrossMark,
        data = data, data_params = list2(...),
        mark = mark, obs_size = obs_size,
        plot = ggplot(), size = size,
        schemes = default_schemes(th = theme_panel_border()),
        active = active,
        inherit_nobs = inherit_nobs,
        inherit_panel = inherit_panel,
        inherit_index = inherit_index
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include craft-cross-.R
CrossMark <- ggproto("CrossMark", CraftCross,
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

        # parse links --------------------------------------------
        mark <- self$mark
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
        link_index <- make_links_data(
            .subset2(mark, "links"),
            domain1 = domain1, domain2 = domain2,
            labels1 = self$labels0, labels2 = self$labels
        )
        if (vec_duplicate_any(nms <- names(link_index))) { # nolint
            cli_abort(
                c(
                    "panel names must be unique in {.arg mark}",
                    i = "duplicated names: {.val {nms[vec_duplicate_detect(nms)]}}"
                ),
                call = self$call
            )
        }
        data_index <- lapply(link_index, function(link) {
            if (is.null(link)) {
                return(NULL)
            }
            hand1 <- .subset2(link, "hand1")
            hand2 <- .subset2(link, "hand2")
            list(
                hand1 = prop(domain1, "index")[hand1],
                hand2 = prop(domain2, "index")[hand2]
            )
        })

        # prepare data for the plot
        plot_data <- lapply(data_index, function(index) {
            if (is.null(index)) {
                return(NULL)
            }
            hand1 <- .subset2(index, "hand1")
            hand2 <- .subset2(index, "hand2")
            hand <- switch_direction(
                direction,
                c("left", "right"),
                c("top", "bottom")
            )
            data_frame0(
                .hand = vec_rep_each(hand, c(length(hand1), length(hand2))),
                .names = vec_c(self$labels0[hand1], self$labels[hand2]),
                .index = vec_c(hand1, hand2)
            )
        })
        plot_data <- vec_rbind(!!!plot_data, .names_to = ".panel")
        plot_data$.panel <- factor(plot_data$.panel, names(data_index))
        plot_data$.hand <- factor(plot_data$.hand, switch_direction(
            direction, c("left", "right"), c("bottom", "top")
        ))

        # set up facets
        if (nlevels(plot_data$.panel) > 1L) {
            facet <- ggplot2::vars(.data$.panel)
        } else {
            facet <- NULL
        }
        plot <- gguse_facet(plot, align_stack_facet(
            direction, plot$facet, facet, "wrap",
            self$layout_name
        ))

        # prepare data for the plot ------------------------------
        plot <- gguse_data(plot, plot_data)

        plot$ggalign_link_data <- list(
            full_data1 = full_data1,
            full_data2 = full_data2,
            link_index = link_index,
            data_index = data_index,
            direction = direction,
            draw = .subset2(mark, "draw"),
            obs_size = self$obs_size
        )
        add_class(plot, "ggalign_mark_plot", "patch_ggplot")
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

        # save spacing for usage
        plot$ggalign_link_data$spacing1 <-
            plot$ggalign_link_data$spacing2 <- spacing

        plot + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(CraftCross, self)$summary(plot)
        c(header, "  Add plot to annotate observations")
    }
)

mark_use_facet <- function(plot, facet) {

}
