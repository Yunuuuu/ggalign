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
            plot = ggplot2::ggplot(mapping = mapping) +
                anno_default_theme(),
            segment_params = rlang::list2(...),
            center = center, type = type, root = root,
            # initialize height value
            height = NULL
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
    setup_params = function(self, data, params, position) {
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
    setup_data = function(self, data, params, position) {
        ans <- as.matrix(data)
        assert_(ans, function(x) is.numeric(x),
            arg = "data", call = self$call
        )
        ans
    },
    add = function(self, object, object_name) {
        plot <- .subset2(self$draw_params, "plot")
        self$draw_params$plot <- htanno_dendro_add(
            object, plot, object_name, self
        )
        self
    },
    compute = function(self, data, panels, index, position,
                       distance, method, use_missing, k, h) {
        # what if the old panels exist, we should do sub-clustering
        if (!is.null(panels)) {
            # if the heatmap has established groups,
            # `k` and `h` must be NULL, and we'll do sub-clustering
            if (!is.null(k) || !is.null(h)) {
                cli::cli_abort(sprintf(
                    "Cannot cut tree since the heatmap %s %s",
                    to_matrix_axis(position), "has been splitted"
                ))
            }
            children <- vector("list", nlevels(panels))
            names(children) <- levels(panels)
            labels <- rownames(data)
            # we do clustering within each group
            for (g in levels(panels)) {
                index <- which(panels == g)
                gdata <- data[index, , drop = FALSE]
                if (nrow(gdata) == 1L) {
                    children[[g]] <- structure(
                        index,
                        class = "dendrogram",
                        leaf = TRUE,
                        height = 0,
                        label = .subset(labels, index),
                        members = 1L
                    )
                } else {
                    child <- stats::as.dendrogram(self$compute(
                        data = gdata,
                        panels = NULL,
                        distance = distance,
                        method = method,
                        use_missing = use_missing
                    ))
                    # we restore the actual index of the original matrix
                    child <- stats::dendrapply(child, function(x) {
                        if (stats::is.leaf(x)) {
                            ans <- .subset(index, x)
                            attributes(ans) <- attributes(x)
                            ans
                        } else {
                            x
                        }
                    })
                    children[[g]] <- child
                }
            }
            if (nlevels(panels) == 1L) { # only one parent
                ans <- .subset2(children, 1L)
            } else {
                parent_data <- t(sapply(levels(panels), function(g) {
                    colMeans(data[panels == g, , drop = FALSE])
                }))
                rownames(parent_data) <- levels(panels)
                parent <- stats::as.dendrogram(self$compute(
                    data = parent_data,
                    panels = NULL,
                    distance = distance,
                    method = method,
                    use_missing = use_missing
                ))
                ans <- merge_dendrogram(parent, children)
                self$draw_params$height <- attr(ans, "cutoff_height")
            }
            return(ans)
        }
        hclust2(data, distance, method, use_missing)
    },
    layout = function(self, data, statistics, panels, old_index,
                      position, k, h) {
        # we'll always reorder the dendrogram
        index <- order2(statistics)
        if (!is.null(k)) {
            panels <- stats::cutree(statistics, k = k)
            self$draw_params$height <- cutree_k_to_h(statistics, k)
        } else if (!is.null(h)) {
            panels <- stats::cutree(statistics, h = h)
            self$draw_params$height <- h
        }
        # reorder panel factor levels to following the dendrogram order
        if (!is.null(panels)) panels <- factor(panels, unique(panels[index]))
        list(panels, index)
    },
    draw = function(self, data, statistics, panels, index, position,
                    # other argumentds
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
        plot <- anno_add_default_mapping(plot, position, switch_position(
            position,
            aes(y = .data$y),
            aes(x = .data$x)
        ))
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
                switch_position(
                    position,
                    ggplot2::geom_vline(
                        xintercept = height, linetype = "dashed"
                    ),
                    ggplot2::geom_hline(
                        yintercept = height, linetype = "dashed"
                    )
                )
        }
        if (!identical(plot$coordinates$clip, "off")) {
            coord <- ggproto_clone(plot$coordinates)
            coord$clip <- "off" # this'll change the input of user.
            plot$coordinates <- coord
        }
        plot + switch_position(
            position,
            ggplot2::labs(x = "height"),
            ggplot2::labs(y = "height")
        )
    }
)
