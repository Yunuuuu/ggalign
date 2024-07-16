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

htanno_dendro_add <- function(object, plot, object_name, self) {
    UseMethod("htanno_dendro_add")
}

#' @export
htanno_dendro_add.gg <- function(object, plot, object_name, self) {
    ggplot2::ggplot_add(object, plot, object_name)
}

#' @export
htanno_dendro_add.labels <- function(object, plot, object_name, self) {
    htanno_dendro_add.gg(object, plot, object_name, self)
}

#' @export
htanno_dendro_add.CoordFlip <- function(object, plot, object_name, self) {
    cli::cli_abort(paste(
        "Can't add {.var {object_name}} to a",
        "{.fn {snake_class(self)}} annotation"
    ), call = self$call)
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
        self$draw_params$plot <- htanno_dendro_add(
            object, plot, object_name, self
        )
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
                "{.fn {snake_class(self)}} will break the original order"
            )
        }
        if (!is.null(k)) {
            height <- cutree_k_to_h(statistics, k)
            panels <- stats::cutree(statistics, h = height)
        } else if (!is.null(h)) {
            panels <- stats::cutree(statistics, h = height <- h)
        } else {
            panels <- NULL
            height <- NULL
        }
        # fix error height not supplied to `$draw()` method
        self$draw_params["height"] <- list(height)
        index <- statistics$order
        # reorder panel factor levels to following the dendrogram order
        if (!is.null(panels)) panels <- factor(panels, unique(panels[index]))
        list(panels, index)
    },
    draw = function(self, data, statistics, panels, index,
                    position, scales, facet,
                    plot, height, plot_cut_height, center, type,
                    root, segment_params) {
        if (nlevels(panels) > 1L && type == "triangle") {
            cli::cli_warn(c(
                paste(
                    "{.arg type} of {.arg triangle}",
                    "is not well support for facet dendrogram"
                ),
                i = "will use {.filed rectangle} dendrogram instead"
            ))
            type <- "rectangle"
        }
        for (scale in scales) {
            if (any(.subset2(scale, "expand") > 0L)) {
                cli::cli_warn(
                    "adding axis expand in a dendrogram will break the layout"
                )
                break
            }
        }
        if (!identical(statistics$order, index)) {
            cli::cli_abort(c(
                "Cannot draw the dendrogram",
                i = "the node order has been changed"
            ), call = self$call)
        }
        data <- dendrogram_data(
            statistics,
            priority = switch_position(position, "left", "right"),
            center = center,
            type = type,
            leaf_braches = panels,
            root = root
        )
        node <- .subset2(data, "node")
        edge <- .subset2(data, "edge")
        if (to_coord_axis(position) == "y") {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }
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
