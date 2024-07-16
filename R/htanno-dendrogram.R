#' Heatmap dendrogram
#'
#' @inheritParams hclust2
#' @inheritParams ggdendrogram
#' @inheritParams htanno
#' @inherit htanno return
#' @export
htanno_dendro <- function(mapping = aes(), ...,
                          distance = "euclidean",
                          method = "complete",
                          use_missing = "pairwise.complete.obs",
                          k = NULL, h = NULL,
                          plot_cut_height = NULL, root = NULL,
                          center = FALSE, type = "rectangle", data = NULL,
                          labels = NULL, labels_nudge = NULL,
                          position = NULL, size = NULL,
                          set_context = NULL, order = NULL, name = NULL,
                          check.param = TRUE) {
    htanno(
        htanno_class = HtannoDendro,
        position = position,
        params = list(
            distance = distance, method = method, use_missing = use_missing,
            k = k, h = h, plot_cut_height = plot_cut_height,
            plot = ggplot2::ggplot(mapping = mapping),
            segment_params = rlang::list2(...),
            center = center, type = type, root = root
        ),
        labels = labels, labels_nudge = labels_nudge,
        set_context = set_context, name = name, order = NULL,
        size = size, check.param = check.param, data = data
    )
}

htanno_dendro_add <- function(object, plot, object_name) {
    UseMethod("htanno_dendro_add")
}

#' @export
htanno_dendro_add.gg <- function(object, plot, object_name) {
    ggplot2::ggplot_add(object, plot, object_name)
}

#' @importFrom ggplot2 aes
HtannoDendro <- ggplot2::ggproto("HtannoDendro", HtannoProto,
    setup_params = function(self, data, position, params) {
        assert_number(.subset2(params, "k"),
            null_ok = TRUE, arg = "k",
            call = self$call
        )
        assert_number(.subset2(params, "h"),
            null_ok = TRUE, arg = "h", call = self$call
        )
        assert_bool(
            .subset2(params, "plot_cut_height"),
            null_ok = TRUE,
            arg = "plot_cut_height", call = self$call
        )
        params
    },
    add = function(self, object, object_name) {
        plot <- .subset2(self$draw_params, "plot")
        self$draw_params$plot <- htanno_dendro_add(object, plot, object_name)
        self
    },
    compute = function(data, position, distance, method, use_missing) {
        hclust2(data, distance, method, use_missing)
    },
    layout = function(self, data, statistics, panels, index, position, k, h) {
        if (!is.null(panels)) {
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = "group of heatmap {to_matrix_axis(position)} already exists"
            ), call = self$call)
        }
        if (!is.null(index)) {
            cli::cli_warn(
                "{.fn {snake_class(self)}} will break the index into pieces",
                call = self$call
            )
        }
        if (!is.null(k)) {
            height <- cutree_k_to_h(statistics, k)
            panels <- stats::cutree(statistics, h = height)
        } else if (!is.null(h)) {
            panels <- stats::cutree(statistics, h = height <- h)
        } else {
            height <- NULL
        }
        self$draw_params$height <- height
        index <- statistics$order
        # reorder panel factor levels to following the dendrogram order
        panels <- factor(panels, unique(panels[index]))
        list(panels, index)
    },
    draw = function(self, data, statistics, panels, index,
                    position, plot, height, plot_cut_height, center, type,
                    root, segment_params) {
        if (nlevels(panels) > 1L && type == "triangle") {
            cli::cli_warn(c(
                paste(
                    "{.arg type} of {.arg triangle}",
                    "is not well support for facet dendrogram"
                ),
                i = "will use {.filed rectangle} dendrogram instead"
            ), call = self$call)
            type <- "rectangle"
        }
        data <- dendrogram_data(
            statistics,
            center = center,
            type = type,
            leaf_braches = panels,
            root = root
        )

        node <- .subset2(data, "node")
        edge <- .subset2(data, "edge")
        leaves <- node[.subset2(node, "leaf"), ]
        ranges <- split(.subset2(leaves, "x"), .subset2(leaves, "panel"))
        ranges <- ranges[order(vapply(ranges, min, numeric(1L)))]
        edge <- tree_edge_double(edge, ranges)
        edge <- rename(edge, c(ggpanel = ".panel"))
        node <- rename(node, c(ggpanel = ".panel"))
        plot$data <- node

        edge_mapping <- aes(
            x = .data$x, y = .data$y,
            xend = .data$xend, yend = .data$yend
        )
        plot$layers <- append(plot$layers,
            rlang::inject(ggplot2::geom_segment(
                mapping = edge_mapping,
                !!!segment_params,
                stat = "identity",
                data = edge
            )),
            after = 0L
        )
        plot_cut_height <- plot_cut_height %||% !is.null(height)
        if (plot_cut_height && !is.null(height)) {
            plot <- plot +
                ggplot2::geom_hline(yintercept = height, linetype = "dashed")
        }
        if (!identical(plot$coordinates$clip, "off")) {
            coord <- ggproto_clone(plot$coordinates)
            coord$clip <- "off" # this'll change the input of user.
            plot$coordinates <- coord
        }
        plot + ggplot2::labs(y = "height")
    }
)

tree_edge_double <- function(edge, ranges) {
    # we draw horizontal lines twice
    #     if one of the node is out of the panel or if the horizontal lines span
    #     across different panels.
    double_index <- (is.na(.subset2(edge, "panel1")) |
        is.na(.subset2(edge, "panel2")) |
        .subset2(edge, "panel1") != .subset2(edge, "panel2")) &
        (.subset2(edge, "x") != .subset2(edge, "xend")) &
        (.subset2(edge, "y") == .subset2(edge, "yend"))
    doubled_edge <- edge[double_index, ]
    doubled_edge$panel1 <- as.character(doubled_edge$panel1)
    doubled_edge$panel2 <- as.character(doubled_edge$panel2)
    doubled_edge <- .mapply(
        function(x, xend, y, yend, branch, panel1, panel2, ranges, ...) {
            if (is.na(panel1) && is.na(panel2)) {
                x0 <- (x + xend) / 2L
                midpoint <- tree_find_panel(ranges, x0)
                if (is.na(panel0 <- .subset2(panel0, "panel"))) {
                    panel0 <- .subset2(midpoint, "right")
                }
                panel <- .subset2(tree_find_panel(ranges, x), "left")
                panel_end <- .subset2(tree_find_panel(ranges, xend), "right")
                data_frame0(
                    x = c(x, x0, x0, xend),
                    xend = c(x0, x, xend, x0),
                    y = y, yend = yend,
                    branch = branch,
                    ggpanel = c(panel, panel0, panel0, panel_end)
                )
            } else if (is.na(panel1)) {
                panel <- .subset2(tree_find_panel(ranges, x), "left")
                data_frame0(
                    x = c(x, xend),
                    xend = c(xend, x),
                    y = y, yend = yend,
                    branch = branch,
                    ggpanel = c(panel, panel2)
                )
            } else if (is.na(panel2)) {
                panel <- .subset2(tree_find_panel(ranges, xend), "right")
                data_frame0(
                    x = c(x, xend),
                    xend = c(xend, x),
                    y = y, yend = yend,
                    branch = branch,
                    ggpanel = c(panel1, panel)
                )
            } else {
                data_frame0(
                    x = c(x, xend),
                    xend = c(xend, x),
                    y = y, yend = yend,
                    branch = branch,
                    ggpanel = c(panel1, panel2)
                )
            }
        }, doubled_edge, list(ranges = ranges)
    )
    edge <- edge[!double_index, ]
    edge <- edge[, c("x", "xend", "y", "yend", "branch", "ggpanel")]
    do.call(rbind, c(list(edge), doubled_edge))
}

tree_find_panel <- function(ranges, x) {
    panels <- names(ranges)
    # not possible in the right most, but we also provide this option
    left_panel <- .subset(panels, length(panels))
    panel <- right_panel <- NA
    for (i in seq_along(ranges)) {
        if (x < min(.subset2(ranges, i))) {
            panel <- NA
            if (i == 1L) {
                left_panel <- NA
            } else {
                left_panel <- .subset(panels, i - 1L)
            }
            right_panel <- .subset(panels, i)
            break
        } else if (x <= max(.subset2(ranges, i))) {
            panel <- .subset(panels, i)
            if (i == 1L) {
                left_panel <- NA
            } else {
                left_panel <- .subset(panels, i - 1L)
            }
            right_panel <- .subset(panels, i + 1L)
            break
        }
    }
    list(panel = panel, left = left_panel, right = right_panel)
}
