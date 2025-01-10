#' Plot dendrogram tree
#'
#' @param ... <[dyn-dots][rlang::dyn-dots]> Additional arguments passed to
#' [`geom_segment()`][ggplot2::geom_segment].
#' @param plot_dendrogram A boolean value indicates whether plot the dendrogram
#' tree.
#' @param plot_cut_height A boolean value indicates whether plot the cut height.
#' @section ggplot2 specification:
#' `align_dendro` initializes a ggplot `data` and `mapping`.
#'
#' The internal `ggplot` object will always use a default mapping of
#' `aes(x = .data$x, y = .data$y)`.
#'
#' The default ggplot data is the `node` coordinates with `edge` data attached
#' in [`ggalign`][ggalign_attr()] attribute, in addition, a
#' [`geom_segment`][ggplot2::geom_segment] layer with a data frame of the `edge`
#' coordinates will be added when `plot_dendrogram = TRUE`.
#'
#' See [`fortify_data_frame.dendrogram()`] for details.
#' @param merge_dendrogram A single boolean value, indicates whether we should
#' merge multiple dendrograms, only used when previous groups have been
#' established. Default: `FALSE`.
#' @inheritParams align_hclust
#' @inheritParams fortify_data_frame.dendrogram
#' @inheritParams ggalign
#' @inheritSection align Discrete Axis Alignment
#' @examples
#' # align_dendro will always add a plot area
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro()
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     anno_top() +
#'     align_dendro(k = 3L)
#'
#' @importFrom ggplot2 aes
#' @importFrom rlang list2
#' @export
align_dendro <- function(mapping = aes(), ...,
                         distance = "euclidean",
                         method = "complete",
                         use_missing = "pairwise.complete.obs",
                         reorder_dendrogram = FALSE,
                         merge_dendrogram = FALSE,
                         reorder_group = FALSE,
                         k = NULL, h = NULL, cutree = NULL,
                         plot_dendrogram = TRUE,
                         plot_cut_height = NULL, root = NULL,
                         center = FALSE, type = "rectangle",
                         size = NULL, data = NULL,
                         no_axes = NULL, active = NULL) {
    assert_bool(plot_cut_height, allow_null = TRUE)
    assert_bool(merge_dendrogram)

    # setup the default value for `plot_cut_height`
    plot_cut_height <- plot_cut_height %||% (
        # we by default don't draw the height of the user-provided cutree
        # since function like `dynamicTreeCut` will merge tree
        (!is.null(k) || !is.null(h)) && is.null(cutree)
    )
    plot <- ggplot(mapping = mapping)
    if (plot_dendrogram) {
        plot <- plot + ggplot2::geom_segment(
            mapping = aes(
                x = .data$x, y = .data$y,
                xend = .data$xend, yend = .data$yend
            ),
            ...,
            stat = "identity",
            data = function(data) ggalign_attr(data, "edge")
        )
    }
    assert_active(active)
    active <- update_active(active, new_active(use = TRUE))
    .align_hclust(
        align = AlignDendro,
        distance = distance,
        method = method,
        use_missing = use_missing,
        merge_dendro = merge_dendrogram,
        plot_cut_height = plot_cut_height,
        type = type, root = root, center = center,
        reorder_dendrogram = reorder_dendrogram,
        reorder_group = reorder_group,
        schemes = default_schemes(th = theme_no_strip()),
        k = k, h = h, cutree = cutree, data = data, active = active,
        size = size, no_axes = no_axes, plot = plot
    )
}

#' @importFrom ggplot2 aes ggplot
#' @importFrom rlang inject
#' @include align-hclust.R
AlignDendro <- ggproto("AlignDendro", AlignHclust,
    setup_plot = function(self, plot) {
        ggadd_default(plot, aes(x = .data$x, y = .data$y)) + switch_direction(
            self$direction,
            ggplot2::labs(x = "height"),
            ggplot2::labs(y = "height")
        )
    },
    build_plot = function(self, plot, design, extra_design = NULL,
                          previous_design = NULL) {
        plot_cut_height <- self$plot_cut_height
        center <- self$center
        type <- self$type
        root <- self$root
        panel <- .subset2(design, "panel")
        index <- .subset2(design, "index")

        statistics <- .subset2(self, "statistics")
        direction <- self$direction
        priority <- switch_direction(direction, "left", "right")
        dendrogram_panel <- self$panel[index]
        if (!is.null(dendrogram_panel) &&
            # we allow to change the panel level name, but we prevent
            # from changing the underlying factor level (the underlying
            # ordering)
            !all(as.integer(dendrogram_panel) == as.integer(panel))) {
            cli_abort("you cannot do sub-splitting in dendrogram groups")
        }

        if (self$multiple_tree) {
            branches <- levels(panel)
            data <- vector("list", length(statistics))
            start <- 0L
            for (i in seq_along(data)) {
                tree <- .subset2(statistics, i)
                n <- stats::nobs(tree)
                end <- start + n
                data[[i]] <- fortify_data_frame(
                    tree,
                    priority = priority,
                    center = center,
                    type = type,
                    leaf_pos = seq(start + 1L, end),
                    leaf_braches = rep_len(.subset(branches, i), n),
                    reorder_branches = FALSE,
                    root = root,
                    double = self$in_linear
                )
                start <- end
            }
            data <- lapply(
                list(
                    node = data,
                    edge = lapply(data, ggalign_attr, "edge")
                ),
                function(dat) {
                    ans <- vec_rbind(!!!dat, .names_to = "parent")
                    ans$.panel <- factor(.subset2(ans, ".panel"), branches)
                    ans
                }
            )
            edge <- .subset2(data, "edge")
            node <- .subset2(data, "node")
        } else {
            if (nlevels(panel) > 1L && type == "triangle" && self$in_linear) {
                cli_warn(c(paste(
                    "{.arg type} of {.arg triangle}",
                    "is not well support for facet dendrogram"
                ), i = "will use {.filed rectangle} dendrogram instead"))
                type <- "rectangle"
            }
            data <- fortify_data_frame(
                statistics,
                priority = priority,
                center = center,
                type = type,
                leaf_braches = as.character(panel),
                # panel has been reordered by the dendrogram index
                reorder_branches = FALSE,
                root = root,
                double = self$in_linear
            )
            edge <- ggalign_attr(data, "edge")
            node <- data
        }

        # add names
        if (!is.null(self$labels)) {
            node$.names <- .subset(self$labels, .subset2(node, ".index"))
        }
        if (is_horizontal(direction)) {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }

        # we do some tricks, since ggplot2 won't remove the attributes
        # we attach the `edge` data
        plot <- gguse_data(plot, ggalign_data_set(node, edge = edge))

        if (plot_cut_height && !is.null(height <- .subset2(self, "height"))) {
            plot <- plot +
                switch_direction(
                    direction,
                    ggplot2::geom_vline(
                        xintercept = height, linetype = "dashed"
                    ),
                    ggplot2::geom_hline(
                        yintercept = height, linetype = "dashed"
                    )
                )
        }
        position <- .subset2(self, "position")
        if (!self$in_linear || # for circular layout
            # for bottom annotation, reverse y-axis
            (!is.null(position) && position == "bottom")) {
            plot <- reverse_continuous_scale(plot, "y")
        } else if (!is.null(position) && position == "left") {
            # for left annotation, reverse x-axis
            plot <- reverse_continuous_scale(plot, "x")
        }

        # always turn off clip, this is what dendrogram dependends on
        old_coord <- plot$coordinates
        if (!identical(old_coord$clip, "off")) {
            # to prevent from changing the input of user.
            plot$coordinates <- ggproto(NULL, old_coord, clip = "off")
        }
        plot
    }
)

tree_one_node <- function(index, label) {
    structure(
        index,
        class = "dendrogram",
        leaf = TRUE,
        height = 0,
        label = label,
        members = 1L
    )
}
