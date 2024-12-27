#' Add a plot to annotate selected observations
#'
#' - `align_line`: Annotate a list of spread observations. Observations will be
#'   connected to the panel by a line.
#' - `align_range`: Annotate a list of ranges of observations. Observation
#'   ranges will be connected to the panel by a polygon.
#'
#' @param mark A [`mark_draw()`] object to define how to draw the links. Like
#' [`mark_line()`], [`mark_tetragon()`].
#' @inheritParams ggalign
#'
#' @section ggplot2 specification:
#' `ggmark` initializes a ggplot object. The underlying data is created using
#' [`fortify_data_frame()`]. Please refer to it for more details.
#'
#' In addition, the following columns will be added to the data frame:
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
#' @examples
#' set.seed(123)
#' small_mat <- matrix(rnorm(56), nrow = 7)
#' rownames(small_mat) <- paste0("row", seq_len(nrow(small_mat)))
#' colnames(small_mat) <- paste0("column", seq_len(ncol(small_mat)))
#'
#' # mark_line
#' ggheatmap(small_mat) +
#'     theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
#'     anno_right() +
#'     align_kmeans(3L) +
#'     ggmark(mark_line(link((I(1:3))))) +
#'     geom_boxplot(aes(.names, value)) +
#'     theme(plot.margin = margin(l = 0.1, t = 0.1, unit = "npc"))
#'
#' # mark_tetragon
#' ggheatmap(small_mat) +
#'     theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
#'     anno_right() +
#'     align_kmeans(3L) +
#'     ggmark(mark_tetragon(link((I(1:3))))) +
#'     geom_boxplot(aes(.names, value)) +
#'     theme(plot.margin = margin(l = 0.1, t = 0.1, unit = "npc"))
#' @importFrom rlang list2
#' @export
ggmark <- function(mark, data = waiver(), mapping = aes(), ...,
                   size = NULL, active = NULL) {
    if (!inherits(mark, "ggalign_mark_draw")) {
        cli_abort("{.arg mark} must be a {.fn mark_draw} object")
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    new_ggalign_plot(
        MarkGg,
        # fields added to `MarkGg`
        input_data = allow_lambda(data), # used by AlignGg
        params = list2(...), # used by AlignGg
        mark = mark, # used by MarkGg

        # slot
        plot = ggplot(mapping = mapping),
        size = size,
        schemes = default_schemes(th = theme_add_panel()),
        active = active
    )
}

#' @importFrom ggplot2 ggproto ggplot margin element_rect
MarkGg <- ggproto("MarkGg", AlignProto,
    free_facet = TRUE,
    free_limits = TRUE,
    setup_layout = function(self, layout) {
        if (!self$in_linear) { # only used for linear coordinate
            cli_abort(c(
                sprintf(
                    "Cannot add %s to %s",
                    object_name(self), self$layout_name
                ),
                i = sprintf(
                    "%s can only be used in {.fn stack_layout}",
                    object_name(self)
                )
            ))
        }
        layout
    },
    setup_design = function(self, data, design) {
        if (is_continuous_design(design)) { # only used for discrete variable
            layout_name <- self$layout_name
            # `AlignDiscrete` object is special for discrete variables
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name(self), layout_name),
                i = sprintf("%s cannot align discrete variables", layout_name)
            ))
        }
        ggproto_parent(AlignGg, self)$setup_design(data, design)
    },

    #' @importFrom stats reorder
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        if (is.null(.subset2(design, "nobs"))) {
            cli_abort(
                c(
                    "you must provide {.arg data} to initialize the layout",
                    i = sprintf("no data was found in %s", self$layout_name)
                ),
                call = self$call
            )
        }
        direction <- self$direction
        position <- self$position

        # parse links --------------------------------------------
        mark <- self$mark
        design1 <- previous_design %||% design
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
        link1 <- .subset2(mark, "link1")
        link2 <- .subset2(mark, "link2")
        if (is_empty(links) && is.null(link1) && is.null(link2)) {
            # guess link1 and link2 from position
            if (is.null(position)) { # a normal stack layout
                link1 <- TRUE
            } else if (any(position == c("top", "left"))) {
                link2 <- TRUE
            } else {
                link1 <- TRUE
            }
        }
        if (isTRUE(link1) && isTRUE(link2)) {
            extra_links <- mapply(function(l1, l2) {
                new_link(I(l1), I(l2))
            }, full_data1, full_data2, SIMPLIFY = FALSE)
        } else if (isTRUE(link1)) {
            extra_links <- lapply(full_data1, function(l) {
                new_link(link1 = I(l))
            })
        } else if (isTRUE(link2)) {
            extra_links <- lapply(full_data2, function(l) {
                new_link(link2 = I(l))
            })
        } else {
            extra_links <- NULL
        }
        links <- c(links, extra_links)
        link_index <- lapply(links, function(l) {
            l1 <- .subset2(l, "link1")
            l2 <- .subset2(l, "link2")
            if (.subset2(design1, "nobs") == .subset2(design2, "nobs")) {
                if (is.waive(l1) && is.waive(l2)) {
                    l1 <- l2 <- NULL
                } else if (is.waive(l1) && !is.waive(l2)) {
                    l1 <- l2
                } else if (!is.waive(l1) && is.waive(l2)) {
                    l2 <- l1
                }
            } else {
                l1 <- l1 %|w|% NULL
                l2 <- l2 %|w|% NULL
            }
            link1 <- make_link_data(l1, design1, labels = self$labels)
            link2 <- make_link_data(l2, design2, labels = self$labels)
            if (is.null(link1) && is.null(link2)) {
                return(NULL)
            }
            list(link1 = link1, link2 = link2)
        })
        link_index <- link_index[
            !vapply(link_index, is.null, logical(1L), USE.NAMES = FALSE)
        ]
        data_index <- lapply(link_index, function(link) {
            link1 <- .subset2(link, "link1")
            link2 <- .subset2(link, "link2")
            list(
                link1 = .subset2(design1, "index")[link1],
                link2 = .subset2(design2, "index")[link2]
            )
        })

        # prepare data for the plot
        plot_data <- lapply(data_index, function(index) {
            link1 <- .subset2(index, "link1")
            link2 <- .subset2(index, "link2")
            hand <- switch_direction(
                direction,
                c("left", "right"),
                c("top", "bottom")
            )
            data_frame0(
                .hand = vec_rep_each(hand, c(length(link1), length(link2))),
                .index = vec_c(link1, link2)
            )
        })
        plot_panel <- names(link_index) %||% seq_along(link_index)
        plot_data <- vec_rbind(!!!plot_data, .names_to = ".panel")
        plot_data$.panel <- factor(plot_data$.panel, unique(plot_panel))
        plot_data$.hand <- factor(plot_data$.hand, switch_direction(
            direction, c("left", "right"), c("bottom", "top")
        ))

        # prepare data for the plot ------------------------------
        if (!is.null(self$labels)) {
            plot_data[[".names"]] <- .subset(
                self$labels, .subset2(plot_data, ".index")
            )
        }
        if (!is.null(data <- self$data)) {
            plot_data <- inner_join(plot_data, data, by = ".index")
        }
        plot <- gguse_data(plot, ggalign_attr_restore(plot_data, data))

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
        plot$ggalign_link_data$spacing1 <-
            plot$ggalign_link_data$spacing2 <- calc_element(
                switch_direction(
                    self$direction,
                    "panel.spacing.y",
                    "panel.spacing.x"
                ),
                theme
            ) %||% unit(0, "mm")
        plot + theme_recycle()
    },
    summary = function(self, plot) {
        header <- ggproto_parent(AlignProto, self)$summary(plot)
        c(header, "  Add plot to annotate discrete variable")
    }
)
