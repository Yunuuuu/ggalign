#' Add a plot to annotate selected observations
#'
#' @param mark A [`mark_draw()`] object to define how to draw the links. Like
#' [`mark_line()`], [`mark_tetragon()`]. Note the names of the pair links will
#' be used to define the panel names so must be unique.
#' @inheritParams ggalign
#' @param group1,group2 A single boolean value indicating whether to use the
#'   panel group information from the layout as the paired groups. By default,
#'   if no specific observations are selected in `mark`, `ggmark()` will
#'   automatically connect all observations and group them according to the
#'   layout's defined groups.
#' @param obs_size A single numeric value that indicates the size of a single
#'   observation, ranging from `(0, 1]`.
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
#'     ggmark(mark_line(I(1:3) ~ NULL)) +
#'     geom_boxplot(aes(.names, value)) +
#'     theme(plot.margin = margin(l = 0.1, t = 0.1, unit = "npc"))
#'
#' # mark_tetragon
#' ggheatmap(small_mat) +
#'     theme(axis.text.x = element_text(hjust = 0, angle = -60)) +
#'     anno_right() +
#'     align_kmeans(3L) +
#'     ggmark(mark_tetragon(I(1:3) ~ NULL)) +
#'     geom_boxplot(aes(.names, value)) +
#'     theme(plot.margin = margin(l = 0.1, t = 0.1, unit = "npc"))
#' @importFrom rlang list2
#' @export
ggmark <- function(mark, data = waiver(), mapping = aes(), ...,
                   group1 = NULL, group2 = NULL,
                   obs_size = 1, size = NULL, active = NULL) {
    if (!inherits(mark, "ggalign_mark_draw")) {
        cli_abort("{.arg mark} must be a {.fn mark_draw} object")
    }
    assert_obs_size(obs_size)
    assert_active(active)
    active <- active(use = TRUE) + active
    assert_bool(group1, allow_null = TRUE)
    assert_bool(group2, allow_null = TRUE)
    new_craftbox(
        MarkGg,
        # fields added to `MarkGg`
        input_data = allow_lambda(data), # used by AlignGg
        data_params = list2(...), # used by AlignGg
        mark = mark, # used by MarkGg
        group1 = group1, group2 = group2,
        obs_size = obs_size,

        # slot
        plot = ggplot(mapping = mapping),
        size = size,
        schemes = default_schemes(data, th = theme_panel_border()),
        active = active
    )
}

#' @importFrom ggplot2 ggproto ggplot margin element_rect
MarkGg <- ggproto("MarkGg", Craftsman,
    interact_layout = function(self, layout) {
        layout_name <- self$layout_name
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
        if (is_layout_continuous(layout)) { # only used for discrete variable
            # ggmark special for discrete variables
            cli_abort(c(
                sprintf("Cannot add %s to %s", object_name(self), layout_name),
                i = sprintf("%s cannot align discrete variables", layout_name)
            ))
        }
        ans <- ggproto_parent(AlignGg, self)$interact_layout(layout)
        self$labels0 <- self$labels # CrossMark uses `labels0`
        ans
    },
    setup_stack_facet = function(self, plot, ...) plot,
    setup_stack_plot = function(self, plot, ...) plot,
    setup_circle_facet = function(self, plot, ...) plot,
    setup_circle_plot = function(self, plot, ...) plot,
    build_plot = function(self, plot, domain, extra_domain = NULL,
                          previous_domain = NULL) {
        if (is.na(prop(domain, "nobs"))) {
            cli_abort(sprintf(
                "you must initialize %s before drawing %s",
                self$layout_name, object_name(self)
            ), call = self$call)
        }
        mark <- self$mark

        # parse links --------------------------------------------
        links <- .subset2(mark, "links")
        group1 <- self$group1
        group2 <- self$group2
        position <- self$position
        if (is_empty(links) && is.null(group1) && is.null(group2)) {
            # guess group1 and group2 from position
            if (is.null(position)) { # a normal stack layout
                group1 <- TRUE
            } else if (any(position == c("top", "left"))) {
                group2 <- TRUE
            } else {
                group1 <- TRUE
            }
        }
        full_data <- split(
            seq_len(prop(domain, "nobs")),
            prop(domain, "panel")
        )
        if (isTRUE(group1) && isTRUE(group2)) {
            extra_links <- mapply(function(l1, l2) {
                new_pair_link(I(l1), I(l2))
            }, full_data, full_data, SIMPLIFY = FALSE)
        } else if (isTRUE(group1)) {
            extra_links <- lapply(full_data, function(l) {
                new_pair_link(hand1 = I(l))
            })
        } else if (isTRUE(group2)) {
            extra_links <- lapply(full_data, function(l) {
                new_pair_link(hand2 = I(l))
            })
        } else {
            extra_links <- NULL
        }
        self$mark$links <- vec_c(extra_links, links)
        on.exit(self$mark <- mark, add = TRUE) # restore the original `mark`

        # setup the plot
        plot <- ggproto_parent(CrossMark, self)$build_plot(
            plot,
            domain,
            extra_domain,
            previous_domain %||% domain
        )
        plot_data <- plot$data

        # prepare data for the plot ------------------------------
        if (!is.null(data <- self$data)) {
            plot_data <- inner_join(plot_data, data, by = ".index")
        }
        gguse_data(plot, ggalign_data_restore(plot_data, data))
    },
    finish_plot = function(self, plot, schemes, theme) {
        ggproto_parent(CrossMark, self)$finish_plot(plot, schemes, theme)
    },
    summary = function(self, plot) {
        header <- ggproto_parent(Craftsman, self)$summary(plot)
        c(header, "  Add plot to annotate observations")
    }
)
