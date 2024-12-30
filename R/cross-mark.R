#' Add a plot to connect selected observations
#'
#' @inheritParams cross_link
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
cross_mark <- function(mark, data = waiver(),
                       inherit_panel = NULL, inherit_nobs = NULL,
                       size = NULL, active = NULL) {
    if (!inherits(mark, "ggalign_mark_draw")) {
        cli_abort("{.arg mark} must be a {.fn mark_draw} object")
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    cross(CrossMark,
        mark = mark,
        plot = ggplot(), size = size,
        schemes = default_schemes(th = theme_add_panel()),
        active = active,
        input_data = data,
        inherit_panel = inherit_panel,
        inherit_nobs = inherit_nobs
    )
}

#' @importFrom ggplot2 ggproto ggproto_parent
#' @include cross-link.R
CrossMark <- ggproto("CrossMark", CrossLink,
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

        # parse links --------------------------------------------
        mark <- self$mark
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
        links <- .subset2(mark, "links")
        if (vec_duplicate_any(nms <- names(links))) { # nolint
            cli_abort(c(
                "panel names must be unique",
                i = "duplicated names: {.val {nms[vec_duplicate_detect(nms)]}}"
            ))
        }
        link_index <- lapply(links, make_pair_link_data,
            design1 = design1, design2 = design2,
            labels1 = self$labels0, labels2 = self$labels
        )
        names(link_index) <- nms

        data_index <- lapply(link_index, function(link) {
            if (is.null(link)) {
                return(NULL)
            }
            hand1 <- .subset2(link, "hand1")
            hand2 <- .subset2(link, "hand2")
            list(
                hand1 = .subset2(design1, "index")[hand1],
                hand2 = .subset2(design2, "index")[hand2]
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

        # prepare data for the plot ------------------------------
        plot <- gguse_data(plot, plot_data)

        # set up facets
        if (nlevels(plot_data$.panel) > 1L) {
            if (inherits(plot$facet, "FacetGrid")) {
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
                facet <- switch_direction(
                    direction,
                    ggplot2::facet_wrap(
                        facets = ggplot2::vars(.data$.panel),
                        ncol = 1L, as.table = FALSE
                    ),
                    ggplot2::facet_wrap(
                        facets = ggplot2::vars(.data$.panel),
                        nrow = 1L, as.table = FALSE
                    )
                )
            }
        } else {
            facet <- facet_stack(direction, object_name(self))
        }
        # free_row and free_column have nothing with `facet_stack`
        # it's safe to use it directly
        plot <- gguse_facet(plot, facet, free_row = TRUE, free_column = TRUE)
        plot$ggalign_link_data <- list(
            full_data1 = full_data1,
            full_data2 = full_data2,
            link_index = link_index,
            data_index = data_index,
            direction = direction,
            draw = .subset2(mark, "draw")
        )
        add_class(plot, "ggalign_mark_plot", "patch_ggplot")
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

        # save spacing for usage
        plot$ggalign_link_data$spacing1 <-
            plot$ggalign_link_data$spacing2 <- spacing

        plot + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(AlignProto, self)$summary(plot)
        c(header, "  Add plot to annotate observations")
    }
)
