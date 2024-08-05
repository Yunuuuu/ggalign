#' Dendrogram plot
#'
#' @inheritParams align_gg
#' @param ... Additional arguments passed to
#' [geom_segment()][ggplot2::geom_segment].
#' @inheritParams hclust2
#' @inheritParams dendrogram_data
#' @param reorder_group A single boolean value, indicates whether we should do
#' Hierarchical Clustering between groups, only used when previous groups have
#' been established.
#' @param k An integer scalar indicates the desired number of groups.
#' @param h A numeric scalar indicates heights where the tree should be cut.
#' @param plot_cut_height A boolean value indicates whether plot the cut height.
#' @inheritParams align
#' @section ggplot2 details:
#' `align_dendro` initializes a `ggplot` data and `mapping`.
#'
#' The internal will always use a default mapping of `aes(x = .data$x, y =
#' .data$y)`.
#'
#' The default ggplot data is the `node` coordinates, in addition, a
#' [geom_segment][ggplot2::geom_segment] layer with a data of the tree segments
#' `edge` coordinates will be added.
#'
#' `node` and tree segments `edge` coordinates contains following columns:
#'   - `index`: the original index in the tree for the current node
#'   - `label`: node label text
#'   - `x` and `y`: x-axis and y-axis coordinates for current node or the start
#'                  node of the current edge.
#'   - `xend` and `yend`: the x-axis and y-axis coordinates of the terminal node
#'                        for current edge.
#'   - `branch`: which branch current node or edge is. You can use this column
#'               to color different groups.
#'   - `panel`: which panel current node is, if we split the plot into panels
#'              using [facet_grid][ggplot2::facet_grid], this column will show
#'              which panel current node or edge is from. Note: some nodes may
#'              fall outside panels (between two panels), so there are possible
#'              `NA` values in this column. We also provide `.panel` column,
#'              which always give the right branch for usage of the ggplot
#'              facet.
#'   - `.panel`: See `panel`, this is what we often used.
#'   - `panel1` and `panel2`: The panel1 and panel2 variables have the same
#'     functionality as `panel`, but they are specifically for the `edge` data
#'     and correspond to both nodes of each edge.
#'   - `leaf`: A logical value indicates whether current node is a leaf.
#'
#' @inherit align return
#' @examples
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     align_dendro()
#' ggheatmap(matrix(rnorm(81), nrow = 9)) +
#'     hmanno("top") +
#'     align_dendro(k = 3L)
#' @importFrom ggplot2 element_blank
#' @seealso
#' - [dendrogram_data()]
#' - [hclust2()]
#' @export
align_dendro <- function(mapping = aes(), ...,
                         distance = "euclidean",
                         method = "complete",
                         use_missing = "pairwise.complete.obs",
                         reorder_group = FALSE,
                         k = NULL, h = NULL,
                         plot_cut_height = NULL, root = NULL,
                         center = FALSE, type = "rectangle",
                         size = NULL, data = NULL,
                         set_context = TRUE, order = NULL, name = NULL) {
    assert_bool(reorder_group)
    assert_mapping(mapping)
    align(
        align_class = AlignDendro,
        params = list(
            distance = distance, method = method, use_missing = use_missing,
            k = k, h = h, plot_cut_height = plot_cut_height,
            segment_params = rlang::list2(...),
            center = center, type = type, root = root,
            reorder_group = reorder_group,
            mapping = mapping
        ),
        set_context = set_context, name = name, order = order,
        size = size, data = data
    )
}

#' @importFrom ggplot2 aes theme
AlignDendro <- ggplot2::ggproto("AlignDendro", Align,
    setup_params = function(self, data, params) {
        call <- .subset2(self, "call")
        assert_number(.subset2(params, "k"),
            null_ok = TRUE, arg = "k",
            call = call
        )
        assert_number(.subset2(params, "h"),
            null_ok = TRUE, arg = "h", call = call
        )
        assert_bool(
            .subset2(params, "plot_cut_height"),
            null_ok = TRUE,
            arg = "plot_cut_height", call = call
        )
        params
    },
    setup_data = function(self, data, params) {
        ans <- as.matrix(data)
        assert_(
            ans, function(x) is.numeric(x),
            "numeric",
            arg = "data",
            call = .subset2(self, "call")
        )
        ans
    },
    compute = function(self, panels, index,
                       distance, method, use_missing, reorder_group,
                       k = NULL, h = NULL) {
        data <- .subset2(self, "data")
        if (nrow(data) < 2L) {
            cli::cli_abort(
                c(
                    "Cannot do Hierarchical Clustering",
                    i = "must have >= 2 observations to cluster"
                ),
                call = .subset2(self, "call")
            )
        }
        # if the old panels exist, we do sub-clustering
        if (!is.null(panels)) {
            # if the heatmap has established groups,
            # `k` and `h` must be NULL, and we'll do sub-clustering
            if (!is.null(k) || !is.null(h)) {
                cli::cli_abort(
                    c(
                        paste(
                            "{.fn {snake_class(self)}} cannot cut tree since",
                            "the layout", .subset2(self, "direction"),
                            "direction", "has been splitted"
                        ),
                        i = "{.arg k} and {.arg h} must be `NULL`"
                    ),
                    call = .subset2(self, "call")
                )
            }
            children <- vector("list", nlevels(panels))
            names(children) <- levels(panels)
            labels <- rownames(data)
            # we do clustering within each group ---------------
            for (g in levels(panels)) {
                i <- which(panels == g)
                gdata <- data[i, , drop = FALSE]
                if (nrow(gdata) == 1L) {
                    children[[g]] <- tree_one_node(i, .subset(labels, i))
                } else {
                    child <- stats::as.dendrogram(hclust2(
                        gdata,
                        distance = distance,
                        method = method,
                        use_missing = use_missing
                    ))
                    # we restore the actual index of the original matrix
                    child <- stats::dendrapply(child, function(x) {
                        if (stats::is.leaf(x)) {
                            ans <- .subset(i, x)
                            attributes(ans) <- attributes(x)
                            ans
                        } else {
                            x
                        }
                    })
                    children[[g]] <- child
                }
            }

            # merge children tree ------------------------------
            if (nlevels(panels) == 1L) { # only one parent
                ans <- .subset2(children, 1L)
            } else if (reorder_group) {
                parent_levels <- levels(panels)
                parent_data <- t(sapply(parent_levels, function(g) {
                    colMeans(data[panels == g, , drop = FALSE])
                }))
                rownames(parent_data) <- parent_levels
                parent <- stats::as.dendrogram(hclust2(
                    parent_data,
                    distance = distance,
                    method = method,
                    use_missing = use_missing
                ))
                # reorder parent based on the parent tree
                panels <- factor(
                    panels, parent_levels[stats::order.dendrogram(parent)]
                )
                ans <- merge_dendrogram(parent, children)
                # we don't cutree
                # self$draw_params$height <- attr(ans, "cutoff_height")
            } else {
                ans <- Reduce(merge, children)
            }
            self$panels <- panels
            return(ans)
        }
        hclust2(data, distance, method, use_missing)
    },
    layout = function(self, panels, index, k, h) {
        statistics <- .subset2(self, "statistics")
        if (!is.null(panels)) { # we have do sub-clustering
            panels <- .subset2(self, "panels")
        } else if (!is.null(k)) {
            panels <- stats::cutree(statistics, k = k)
            self$height <- cutree_k_to_h(statistics, k)
        } else if (!is.null(h)) {
            panels <- stats::cutree(statistics, h = h)
            self$height <- h
        }
        index <- order2(statistics)
        # reorder panel factor levels to following the dendrogram order
        if (!is.null(panels)) panels <- factor(panels, unique(panels[index]))
        list(panels, index)
    },
    ggplot = function(self, mapping, segment_params) {
        direction <- .subset2(self, "direction")
        ans <- ggplot2::ggplot(mapping = mapping) +
            rlang::inject(ggplot2::geom_segment(
                mapping = aes(
                    x = .data$x, y = .data$y,
                    xend = .data$xend, yend = .data$yend
                ),
                !!!segment_params,
                stat = "identity"
            )) +
            switch_direction(
                direction,
                ggplot2::labs(x = "height"),
                ggplot2::labs(y = "height")
            )
        add_default_mapping(ans, aes(x = .data$x, y = .data$y))
    },
    draw = function(self, panels, index,
                    # other argumentds
                    plot_cut_height, center, type,
                    root, segment_params) {
        direction <- .subset2(self, "direction")
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
            .subset2(self, "statistics"),
            priority = switch_direction(direction, "left", "right"),
            center = center,
            type = type,
            leaf_braches = panels,
            root = root
        )
        node <- .subset2(data, "node")
        edge <- .subset2(data, "edge")
        node <- rename(node, c(ggpanel = ".panel"))
        edge <- rename(edge, c(ggpanel = ".panel"))
        if (is_horizontal(direction)) {
            edge <- rename(
                edge,
                c(x = "y", xend = "yend", y = "x", yend = "xend")
            )
            node <- rename(node, c(x = "y", y = "x"))
        }
        plot <- .subset2(self, "plot")
        plot$data <- node
        # edge layer should be in the first
        plot$layers[[1L]]$data <- edge
        height <- .subset2(self, "height")
        plot_cut_height <- plot_cut_height %||% !is.null(height)
        if (plot_cut_height && !is.null(height)) {
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
        # always turn off clip, this is what dendrogram dependends on
        if (!identical(plot$coordinates$clip, "off")) {
            coord <- ggproto_clone(plot$coordinates)
            coord$clip <- "off" # this'll change the input of user.
            plot$coordinates <- coord
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
