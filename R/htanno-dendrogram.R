htanno_dendro <- function(mapping = aes(), ...,
                          distance = "euclidean",
                          method = "complete",
                          use_missing = "pairwise.complete.obs",
                          k = NULL, h = NULL,
                          plot_cut_height = FALSE, root = NULL,
                          center = FALSE, type = "rectangle", data = NULL,
                          position = NULL, size = unit(10, "mm"),
                          active = TRUE, name = NULL, order = NULL,
                          check.param = TRUE) {
    htanno(
        HtannoDendro,
        position = position,
        params = list(
            distance = distance, method = method, use_missing = use_missing,
            k = k, h = h, plot_cut_height = plot_cut_height,
            plot = ggplot2::ggplot(mapping = mapping),
            params = rlang::list2(...),
            center = center, type = type, root = root
        ),
        active = active, name = name, order = NULL,
        size = size, check.param = check.param
    )
}

HtannoDendro <- ggplot2::ggproto("HtannoDendro", HtannoProto,
    setup_params = function(self, data, position, params) {
        assert_number(.subset2(params, "k"), null_ok = TRUE, arg = "k")
        assert_number(.subset2(params, "h"), null_ok = TRUE, arg = "h")
        params
    },
    add_gg = function(self, gg, object_name) {
        self$draw_params$plot <- ggplot2::ggplot_add(
            gg, .subset2(self$draw_params, "plot"), object_name
        )
        self
    },
    compute = function(data, position, distance, method, use_missing) {
        hclust2(data, distance, method, use_missing)
    },
    make_panels = function(self, data, statistics, panels, position, k, h) {
        if (!is.null(panels)) {
            cli::cli_abort(c(
                "{.fn {snake_class(self)}} cannot do sub-split",
                i = "group of heatmap {to_axis(position)} already exists"
            ))
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
        # reorder panel factor levels to following the dendrogram order
        factor(panels, unique(panels[statistics$order]))
    },
    reorder = function(data, statistics, panels, position) {
        statistics$order
    },
    draw = function(self, data, statistics, index,
                    panels, scales, facet, position,
                    plot, height, plot_cut_height, center, type,
                    root, params) {
        data <- dendrogram_data(
            statistics,
            center = center,
            type = type,
            leaf_braches = panels,
            root = root
        )
        data <- lapply(data, rename, c(panel = ".panels"))
        breaks <- vapply(split(seq_along(index), panels[index]), function(x) {
            max(x)
        }, numeric(1L))
        node <- .subset2(data, "node")
        plot$data <- node
        edge <- .subset2(data, "edge")
        edge_mapping <- ggplot2::aes(
            x = .data$x, y = .data$y,
            xend = .data$xend, yend = .data$yend
        )
        plot$layers <- append(plot$layers,
            rlang::inject(ggplot2::geom_segment(
                mapping = edge_mapping,
                !!!params,
                stat = "identity",
                data = edge
            )),
            after = 0L
        )
        plot + ggplot2::labs(y = "height") +
            # ggbreak::scale_x_cut(
            #     breaks = breaks[-length(breaks)],
            #     scales = abs(diff(c(1, breaks)))
            # ) +
            ggplot2::coord_cartesian(clip = "off") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = -60, hjust = 0L),
                panel.background = ggplot2::element_blank()
            )
    }
)
